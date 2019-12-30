package ttbf.codegen
import ttbf.common.ast._
import ttbf.common.instrm._

package object Codegen {
  import ops._
  import env._
  def genStmts(
      stmts: List[ASTStmt]
  )(implicit global: GlobalEnv, local: LocalEnv) = {
    val instrs: List[InstrM[Unit]] = stmts map {
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
              _ <- Op.push("Wo") // TODO: same as above
            } yield ()
        }
    instrs.reduceRight { (a: InstrM[Unit], b: InstrM[Unit]) =>
      a.flatMap((_: Unit) => b)
    }
  }
  def genExpr(
      expr: ASTExpr // change to TypedExpr later
  )(implicit local: LocalEnv, global: GlobalEnv): InstrM[Unit] = expr match {
    case ASTIdxedVar(id, idx) => ???
    case ASTConst(v)          => Op.push(f"u$v%d")
    case ASTVar(id) => {
      for {
        _ <- if (local contains id) {
          moveToLocal(local(id).p)
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
  }
  def genSubrt(subrt: ASTSubrt)(implicit global: GlobalEnv): InstrM[Unit] =
    for {
      _ <- enterSubrtState
      local <- InstrM.unit {
        val retValAndName = if (subrt.retVal.isDefined) Some(subrt.retVal.get, subrt.name) else None
        declsToLocalEnv(subrt.params, subrt.vars, retValAndName)
      }
      _ <- genStmts(subrt.body)(global, local)
    } yield ()

  def genMainBody(mainBody: List[ASTStmt])(implicit global: GlobalEnv): InstrM[Unit] =
    for {
      _ <- enterMainBodyState
      _ <- mainSettingUpInstr
      _ <- genStmts(mainBody)(global, LocalEnv.initial)
    } yield ()

  def genProg(prog: ASTProg): InstrM[Unit] = {
    val global         = declsToGlobalEnv(prog.globalVars)
    val subrtInstrList = prog.subrts.map((subrt => genSubrt(subrt)(global)))
    val subrtInstrs = subrtInstrList.foldRight(printError) { (a, b) =>
      val bb = for {
        _ <- decArrVal
        _ <- b
      } yield ()
      val aa = for {
        _ <- Op.push(">1")
        _ <- a
      } yield ()
      ifTopThenElse(bb, aa)
    }
    val mainInstrs = genMainBody(prog.mainBody)(global)
    ifTopThenElse(subrtInstrs, mainInstrs)
  }
}
