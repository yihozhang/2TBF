package ttbf.codegen
import ttbf.common.AST._
class Codegen(prog: ASTProg) {
    private lazy val globalVars = prog.globalVars.map(new Codegen.Var(_))
    private def genStmts(stmts: List[ASTStmt]): List[String] = {
        var pos = 0
        stmts map {
            case ASTAsg(lval, rval) => ???
            case ASTRead(expr) => ???
            case ASTWrite(expr) => ???
        }
    }
}
object Codegen {
    private class Var(val name: String, val size: Int, val typ: ASTVarType) {
        def this(varDecl: ASTVarDecl) {
            this(varDecl.name, calcSize(varDecl.varType), varDecl.varType)
        }
    }
    private def calcSize(typ: ASTVarType): Int = typ match {
        case ASTArr(lbound, ubound, varType) => (ubound - lbound + 1) * calcSize(varType)
        case ASTChar => 1
        case ASTInt => 1
    }
    private object Code {
        val SET_ZERO = "!0"
        val ELIM_ZERO = "!0"
        val MOVE_TO_ZERO = "?0-1[o0<1?0-1]"
    }
}