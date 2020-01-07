package ttbf.common
package object ast {

  sealed abstract class AST

  case class ASTProg(globalVars: ASTVarDecls, subrts: List[ASTSubrt], mainBody: List[ASTStmt]) extends AST

  type ASTVarDecls = List[ASTVarDecl]
  // case class ASTDecls[T<:ASTDeclAbstract](varDecl: List[T]) extends AST
//   sealed abstract class ASTDeclAbstract(val varName: ASTId, val varType: ASTVarType) extends AST {
//     lazy val name = varName.v
//   }
//   type ASTVarDecls   = ASTDecls[ASTVarDecl];
  type ASTParamDecls = ASTVarDecls;
  case class ASTVarDecl(varName: ASTId, varType: ASTVarType) {
    lazy val name = varName.v
  }
  trait PassByReference                                                extends ASTVarDecl
  sealed abstract class ASTVarType                                     extends AST
  sealed abstract class ASTBaseVarType                                 extends ASTVarType
  case object ASTInt                                                   extends ASTBaseVarType
  case object ASTChar                                                  extends ASTBaseVarType
  case class ASTArr(lbound: Int, ubound: Int, varType: ASTBaseVarType) extends ASTVarType

  sealed abstract class ASTSubrt(
      val name: ASTId,
      val params: ASTParamDecls,
      val vars: ASTVarDecls,
      val body: List[ASTStmt]
  ) extends AST {
    def retVal: Option[ASTBaseVarType]
  }
  case class ASTProc(
      procName: ASTId,
      override val params: ASTParamDecls,
      override val vars: ASTVarDecls,
      override val body: List[ASTStmt]
  ) extends ASTSubrt(procName, params, vars, body) {
    val retVal = None
  }
  case class ASTFun(
      funName: ASTId,
      override val params: ASTParamDecls,
      override val vars: ASTVarDecls,
      override val body: List[ASTStmt],
      retType: ASTBaseVarType
  ) extends ASTSubrt(funName, params, vars, body) {
    val retVal = Some(retType)
  }

  sealed abstract class ASTStmt                                       extends AST
  case class ASTAsg(lval: ASTExpr, rval: ASTExpr)                     extends ASTStmt
  case class ASTRead(expr: ASTExpr)                                   extends ASTStmt
  case class ASTWrite(expr: ASTExpr)                                  extends ASTStmt
  case class ASTIf(cond: ASTExpr, thn: ASTStmt, els: Option[ASTStmt]) extends ASTStmt
  case class ASTBlock(stmts: List[ASTStmt]) extends ASTStmt

  sealed abstract class ASTExpr                               extends AST
  class TypedExpr[T <: ASTExpr](val expr: T, typ: ASTVarType) extends ASTExpr
  case class ASTVar(id: ASTId)                                extends ASTExpr
  case class ASTPlus(lv: ASTExpr, rv: ASTExpr)                extends ASTExpr
  case class ASTMinus(lv: ASTExpr, rv: ASTExpr)               extends ASTExpr
  case class ASTConst(v: Int)                                 extends ASTExpr
  case class ASTIdxedVar(id: ASTId, idx: ASTExpr)             extends ASTExpr

  case class ASTId(v: String) extends AST

  case class ASTFunCall(fun: ASTId, params: List[ASTExpr])     extends ASTExpr
  case class ASTSubrtCall(subrt: ASTId, params: List[ASTExpr]) extends ASTStmt
  case class ASTWhile(cond: ASTExpr, body: ASTStmt) extends ASTStmt
}
