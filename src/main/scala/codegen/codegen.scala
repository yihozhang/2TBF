package ttbf.codegen
import ttbf.common.ast._
import ttbf.common.instrm._

package object Codegen {
  import ops._
  import env._
  def genStmts(
      stmts: List[ASTStmt]
  )(implicit local: LocalEnv, global: GlobalEnv) = {
    val instrs: List[InstrM[Unit]] = stmts map {
          case ASTAsg(lval, rval) =>
            for {
              _ <- genExpr(rval)
              _ <- genExpr(lval)
              _ <- convertToLVal
              _ <- Op.push("o")
            } yield ()
          case ASTRead(expr)  =>
            for {
              _ <- genExpr(expr)
              _ <- convertToLVal
              _ <- Op.push("Ro")   // TODO: check whether expr is character or integer
            } yield ()
          case ASTWrite(expr) =>
            for {
              _ <- genExpr(expr)
              _ <- Op.push("Wo")    // TODO: same as above
            } yield ()
        }
    instrs.reduceLeft { (a: InstrM[Unit], b: InstrM[Unit]) =>
      a.flatMap((_: Unit) => b)
    }
  }
  def genExpr(
      expr: ASTExpr
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
}
