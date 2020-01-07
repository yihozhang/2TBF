package ttbf.codegen
import ttbf.common.ast._
import ttbf.common.instrm._

package object Codegen {
  import ops._
  import env._

  def genStmt(stmt: ASTStmt)(implicit global: GlobalEnv, local: Option[LocalEnv], subrtEnv: SubrtEnv): InstrM[Unit] =
    stmt match {
      case ASTAsg(lval, rval) =>
        for {
          _ <- genExpr(rval)
          _ <- genExpr(lval)
          _ <- convertToLValAndPop
        } yield ()
      case ASTRead(expr) =>
        for {
          _ <- readInt // TODO: check whether expr is character or integer
          _ <- genExpr(expr)
          _ <- convertToLValAndPop
        } yield ()
      case ASTWrite(expr) =>
        for {
          _ <- genExpr(expr)
          _ <- writeInt
          _ <- popAway // TODO: same as above
        } yield ()
      case ASTSubrtCall(subrt, params) =>
        setupSubrtCall(subrt, params, false)
      case ASTBlock(stmts) => genStmts(stmts)
      case ASTIf(cond, thn, els) =>
        ifCondThenElse(genExpr(cond), genStmt(thn), genStmt(els.getOrElse(ASTBlock(Nil))))
      case ASTWhile(cond, body) => {
        val condInstr = genExpr(cond)
        val bodyInstr = genStmt(body)
        for {
          _ <- condInstr
          _ <- loop {
            for {
              _ <- popAway
              _ <- bodyInstr
              _ <- condInstr
            } yield ()
          }
          _ <- popAway
        } yield ()
      }
    }

  def setupSubrtCall(
      name: ASTId,
      params: List[ASTExpr],
      needsRetVal: Boolean
  )(implicit global: GlobalEnv, local: Option[LocalEnv], subrtEnv: SubrtEnv) = {
    val instrs = params map { param =>
          for {
            pos <- getPos // remember current position
            _   <- genExpr(param)
            _   <- moveTo(pos)
            _   <- moveRight(1)
            _   <- popToArr
          } yield ()
        }
    for {
      pos <- InstrM.unit(
        if (local.isDefined)
          Pos(PositionState.LOCAL, local.get.largestPos + 1)
        else
          Pos(PositionState.GLOBAL, global.largestPos + 1)
      )
      _ <- moveTo(pos)
      _ <- setVal(subrtEnv(name))
      _ <- seq(instrs)
      _ <- moveTo(pos)
      _ <- startRecurse
      _ <- if (needsRetVal) {
        pushValToStk
      } else InstrM.unit(())
    } yield ()
  }

  def genStmts(
      stmts: List[ASTStmt]
  )(implicit global: GlobalEnv, local: Option[LocalEnv], subrtEnv: SubrtEnv) = {
    val instrs: List[InstrM[Unit]] = stmts.map(genStmt(_))
    seq(instrs)
  }

  def getVarPos(id: ASTId)(implicit global:GlobalEnv, local: Option[LocalEnv]) = {
    if (local.isDefined && local.get.contains(id)) {
        Pos(PositionState.LOCAL, (local.get)(id).p)
      } else if (global contains id) {
        Pos(PositionState.GLOBAL, global(id).p)
      } else {
        throw new IllegalArgumentException("Can't find variables: " + id.v)
      }
  }
  def genExpr(
      expr: ASTExpr // change to TypedExpr later
  )(implicit global: GlobalEnv, local: Option[LocalEnv], subrtEnv: SubrtEnv): InstrM[Unit] = expr match {
    case ASTIdxedVar(id, idx) => for {
      _ <- genExpr(idx)
      typeInfo <- InstrM.unit {
        (if (local.isDefined && local.get.contains(id)) {
          (local.get)(id).typ
        } else {
          global(id).typ
        }).asInstanceOf[ASTArr]
      }
      _ <- pushToStk(typeInfo.lbound)
      _ <- minus
      _ <- moveTo(getVarPos(id))
      _ <- dynamicFetchAndReplaceByOffsetOnStk
    } yield ()
    case ASTConst(v) => pushToStk(v)
    case ASTVar(id) => {
      for {
        _ <- moveTo(getVarPos(id))
        _ <- pushValToStk
      } yield ()
    }
    // TODO: if oprand is variable, then we can actually do some optimizations...
    case ASTPlus(lv, rv) =>
      for {
        _ <- genExpr(lv)
        _ <- genExpr(rv)
        _ <- plus
      } yield ()
    case ASTMinus(lv, rv) =>
      for {
        _ <- genExpr(lv)
        _ <- genExpr(rv)
        _ <- minus
      } yield ()
    case ASTFunCall(fun, params) => setupSubrtCall(fun, params, true)
  }
  def genSubrt(subrt: ASTSubrt)(implicit global: GlobalEnv, subrtEnv: SubrtEnv): InstrM[Unit] = {
    val local = {
      val retValAndName = if (subrt.retVal.isDefined) Some(subrt.retVal.get, subrt.name) else None
      declsToLocalEnv(subrt.params, subrt.vars, retValAndName)
    }

    for {
      _ <- enterSubrtState
      _ <- genStmts(subrt.body)(global, Some(local), subrtEnv)
      _ <- moveToLocal(0)
    } yield ()
  }

  def genMainBody(mainBody: List[ASTStmt])(implicit global: GlobalEnv, subrtEnv: SubrtEnv): InstrM[Unit] =
    for {
      _ <- enterMainBodyState
      _ <- mainSettingUpInstr
      _ <- genStmts(mainBody)(global, None, subrtEnv)
      _ <- enterSubrtState // for the harmony of the overall if-branching structure
    } yield ()

  def genProg(prog: ASTProg): InstrM[Unit] = {
    val global         = declsToGlobalEnv(prog.globalVars)
    val subrtEnv       = subrtsToSubrtEnv(prog.subrts)
    val subrtInstrList = prog.subrts.map { genSubrt(_)(global, subrtEnv) }
    val mainInstrs     = genMainBody(prog.mainBody)(global, subrtEnv)
    val instrs = (mainInstrs :: subrtInstrList).foldRight(printError) { (a, b) =>
      val bb = decArrVal flatMap { case () => b }
      ifCondThenElse(pushValToStk, bb, a)
    }
    instrs
  }
}
