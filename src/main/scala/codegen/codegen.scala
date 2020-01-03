package ttbf.codegen
import ttbf.common.ast._
import ttbf.common.instrm._

package object Codegen {
  import ops._
  import env._

  def genStmt(stmt: ASTStmt)(implicit global: GlobalEnv, local: Option[LocalEnv], lookupTable: LookupTable) =
    stmt match {
      case ASTAsg(lval, rval) =>
        for {
          _ <- genExpr(rval)
          _ <- genExpr(lval)
          _ <- convertToLVal
          _ <- Op.push("o")
        } yield ()
      case ASTRead(expr) =>
        for {
          _ <- genExpr(expr)
          _ <- convertToLVal
          _ <- Op.push("Ro") // TODO: check whether expr is character or integer
        } yield ()
      case ASTWrite(expr) =>
        for {
          _ <- genExpr(expr)
          _ <- Op.push("Wo0") // TODO: same as above
        } yield ()
      case ASTSubrtCall(subrt, params) =>
        setupSubrtCall(subrt, params, false)
    }

  def setupSubrtCall(
      name: ASTId,
      params: List[ASTExpr],
      needsRetVal: Boolean
  )(implicit global: GlobalEnv, local: Option[LocalEnv], lookupTable: LookupTable) = {
    val instrs = params map { param =>
          for {
            pos <- Op.getPos // remember current position
            _   <- genExpr(param)
            _   <- ops.moveTo(pos)
            _   <- Op.moveRight(1)
            _   <- Op.push("o")
          } yield ()
        }
    for {
      pos <- InstrM.unit(
        if (local.isDefined)
          Pos(PositionState.LOCAL, local.get.largestPos + 1)
        else
          Pos(PositionState.GLOBAL, global.largestPos + 1)
      )
      _ <- ops.moveTo(pos)
      _ <- Op.push("u" + lookupTable(name) + "o")
      _ <- Op.seq(instrs)
      _ <- ops.moveTo(pos)
      _ <- Op.push("s")
      _ <- if (needsRetVal) {
        Op.push("u")
      } else InstrM.unit(())
    } yield ()
  }

  def genStmts(
      stmts: List[ASTStmt]
  )(implicit global: GlobalEnv, local: Option[LocalEnv], lookupTable: LookupTable) = {
    val instrs: List[InstrM[Unit]] = stmts.map(genStmt(_))
    Op.seq(instrs)
  }
  def genExpr(
      expr: ASTExpr // change to TypedExpr later
  )(implicit global: GlobalEnv, local: Option[LocalEnv], lookupTable: LookupTable): InstrM[Unit] = expr match {
    case ASTIdxedVar(id, idx) => ???
    case ASTConst(v)          => Op.push(f"u$v%d")
    case ASTVar(id) => {
      for {
        _ <- if (local.isDefined && local.get.contains(id)) {
          moveToLocal((local.get)(id).p)
        } else if (global contains id) {
          moveToGlobal(global(id).p)
        } else {
          throw new IllegalArgumentException("Can't find variables")
        }
        _ <- Op.push("u")
      } yield ()
    }
    // TODO: if oprand is variable, then we can actually do some optimizations...
    case ASTPlus(lv, rv) =>
      for {
        _ <- genExpr(lv)
        _ <- genExpr(rv)
        _ <- Op.push("+")
      } yield ()
    case ASTMinus(lv, rv) =>
      for {
        _ <- genExpr(lv)
        _ <- genExpr(rv)
        _ <- Op.push("-")
      } yield ()
    case ASTFunCall(fun, params) => setupSubrtCall(fun, params, true)
  }
  def genSubrt(subrt: ASTSubrt)(implicit global: GlobalEnv, lookupTable: LookupTable): InstrM[Unit] = {
    val local = {
      val retValAndName = if (subrt.retVal.isDefined) Some(subrt.retVal.get, subrt.name) else None
      declsToLocalEnv(subrt.params, subrt.vars, retValAndName)
    }

    subrt match {
      case ASTFun(funName, params, vars, body, retType) =>
        for {
          _ <- enterSubrtState
          _ <- genStmts(subrt.body)(global, Some(local), lookupTable)
          _ <- moveToLocal(0)
        } yield ()
      case ASTProc(procName, params, vars, body) =>
        for {
          _ <- enterSubrtState
          _ <- Op.push(">1") // the next cell is the start of parameters
          _ <- genStmts(subrt.body)(global, Some(local), lookupTable)
          _ <- moveToLocal(0)
          _ <- Op.push("<1")
        } yield ()
    }
  }

  def genMainBody(mainBody: List[ASTStmt])(implicit global: GlobalEnv, lookupTable: LookupTable): InstrM[Unit] =
    for {
      _ <- enterMainBodyState
      _ <- mainSettingUpInstr
      _ <- genStmts(mainBody)(global, None, lookupTable)
    } yield ()

  def genProg(prog: ASTProg): InstrM[Unit] = {
    val global         = declsToGlobalEnv(prog.globalVars)
    val lookupTable    = subrtsToLookupTable(prog.subrts)
    val subrtInstrList = prog.subrts.map { genSubrt(_)(global, lookupTable) }
    val mainInstrs     = genMainBody(prog.mainBody)(global, lookupTable)
    val instrs = (mainInstrs :: subrtInstrList).foldRight(printError) { (a, b) =>
      val bb = decArrVal flatMap { case () => b }
      ifTopThenElse(bb, a)
    }
    instrs
  }
}
