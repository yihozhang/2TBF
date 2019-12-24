package common.AST

sealed abstract class AST

case class ASTProg(globalVars: ASTVarDecls,
    subrts: List[ASTSubrt], mainBody: ASTStmts) extends AST

case class ASTVarDecls(varDecl: List[ASTVarDecl]) extends AST
case class ASTVarDecl(varName: ASTId, varType: ASTVarType) extends AST
sealed abstract class ASTVarType extends AST
sealed abstract class ASTBaseVarType extends ASTVarType
case object ASTInt extends ASTBaseVarType
case object ASTChar extends ASTBaseVarType
case class ASTArr(lbound: Int, ubound: Int, varType: ASTVarType) extends ASTVarType

sealed abstract class ASTSubrt(procName: ASTId, params: ASTVarDecls, vars: ASTVarDecls, body: ASTStmts) extends AST
case class ASTProc(procName: ASTId, params: ASTVarDecls, vars: ASTVarDecls, body: ASTStmts)
    extends ASTSubrt(procName, params, vars, body)
case class ASTFun(funName: ASTId, params: ASTVarDecls, vars: ASTVarDecls, body: ASTStmts, retType: ASTBaseVarType)
    extends ASTSubrt(funName, params, vars, body)

case class ASTStmts(stmts: List[ASTStmt]) extends AST
sealed abstract class ASTStmt extends AST

case class ASTId(v: String) extends AST
case class ASTConst(v: Int) extends AST
