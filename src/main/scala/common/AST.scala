package ttbf.common
package object AST {

    sealed abstract class AST

    case class ASTProg(globalVars: ASTVarDecls,
        subrts: List[ASTSubrt], mainBody: List[ASTStmt]) extends AST

    type ASTDecls[T<:ASTDeclAbstract] = List[T]
    // case class ASTDecls[T<:ASTDeclAbstract](varDecl: List[T]) extends AST
    sealed abstract class ASTDeclAbstract(varName: ASTId, varType: ASTVarType) extends AST {
        lazy val name = varName.v
    }
    type ASTVarDecls = ASTDecls[ASTVarDecl];
    type ASTParamDecls = ASTDecls[ASTParamDecl];
    case class ASTVarDecl(varName: ASTId, varType: ASTVarType) extends ASTDeclAbstract(varName, varType)
    case class ASTParamDecl(varName: ASTId, varType: ASTVarType, passByReference: Boolean)
        extends ASTDeclAbstract(varName, varType)
    sealed abstract class ASTVarType extends AST
    sealed abstract class ASTBaseVarType extends ASTVarType
    case object ASTInt extends ASTBaseVarType
    case object ASTChar extends ASTBaseVarType
    case class ASTArr(lbound: Int, ubound: Int, varType: ASTBaseVarType) extends ASTVarType

    sealed abstract class ASTSubrt(procName: ASTId, params: ASTParamDecls, vars: ASTVarDecls, body: List[ASTStmt]) extends AST
    case class ASTProc(procName: ASTId, params: ASTParamDecls, vars: ASTVarDecls, body: List[ASTStmt])
        extends ASTSubrt(procName, params, vars, body)
    case class ASTFun(funName: ASTId, params: ASTParamDecls, vars: ASTVarDecls, body: List[ASTStmt], retType: ASTBaseVarType)
        extends ASTSubrt(funName, params, vars, body)

    sealed abstract class ASTStmt extends AST
    case class ASTAsg(lval: ASTExpr, rval: ASTExpr) extends ASTStmt
    case class ASTRead(expr: ASTExpr) extends ASTStmt
    case class ASTWrite(expr: ASTExpr) extends ASTStmt

    sealed abstract class ASTExpr extends AST
    case class ASTVar(id: ASTId) extends ASTExpr
    case class ASTPlus(lv: ASTExpr, rv: ASTExpr) extends ASTExpr
    case class ASTMinus(lv: ASTExpr, rv: ASTExpr) extends ASTExpr
    case class ASTConst(v: Int) extends ASTExpr
    case class ASTIdxedVar(id: ASTId, idx: ASTExpr) extends ASTExpr

    case class ASTId(v: String) extends AST
}