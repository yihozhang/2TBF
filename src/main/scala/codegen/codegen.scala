package ttbf.codegen
import ttbf.common.AST._

object Codegen {
  private type Env = Map[String, Var]
  private case class LocalEnv(env: Env)
  private case class GlobalEnv(env: Env)
  private class Var(val name: String, val size: Int, val typ: ASTVarType) {
    def this(varDecl: ASTVarDecl) {
      this(varDecl.name, calcSize(varDecl.varType), varDecl.varType)
    }
  }
  private def calcSize(typ: ASTVarType): Int = typ match {
    case ASTArr(lbound, ubound, varType) =>
      (ubound - lbound + 1) * calcSize(varType)
    case ASTChar => 1
    case ASTInt  => 1
  }
  private object Code {
    val SET_ZERO = "!0"
    val ELIM_ZERO = "!0"
    val MOVE_TO_ZERO = "?0-1[o0<1?0-1]"
  }
}

class Codegen(prog: ASTProg) {
  import Codegen._
  private lazy val globalVEnv = prog.globalVars.map { v =>
    (v.name, new Codegen.Var(v))
  } toMap
  private def genStmts(
      stmts: List[ASTStmt]
  )(implicit local: LocalEnv, global: GlobalEnv): List[String] = {
    var pos = 0
    stmts map {
      case ASTAsg(lval, rval) => {
        val lvalCode = genExpr(lval)
        val rvalCode = genExpr(rval)
        ???
      }
      case ASTRead(expr)  => ???
      case ASTWrite(expr) => ???
    }
  }
  private def genExpr(
      expr: ASTExpr
  )(implicit local: LocalEnv, global: GlobalEnv): List[String] = expr match {
    case ASTIdxedVar(id, idx) => ???
    case ASTConst(v)          => List(f"u$v%d")
    case ASTVar(id)           => ???
    case ASTPlus(lv, rv)      => ???
    case ASTMinus(lv, rv)     => ???
  }
}
