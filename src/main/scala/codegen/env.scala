package ttbf.codegen

package object env {
  import ttbf.common.ast._
  class Env(env: Map[ASTId, Var]) {
    def contains(id: ASTId) = env.contains(id)
    def apply(id: ASTId)    = env(id)
  }
  case class LocalEnv(env: Map[ASTId, Var])  extends Env(env)
  case class GlobalEnv(env: Map[ASTId, Var]) extends Env(env)
  class Var(
      val name: ASTId,
      val p: Int,
      val size: Int,
      val typ: ASTVarType
  ) {
    def this(varDecl: ASTVarDecl, p: Int) {
      this(varDecl.varName, p, calcSize(varDecl.varType), varDecl.varType)
    }
  }
  def declsToEnv(vars: ASTVarDecls) = {
    var cur = 0
    val env = vars
      .map(astv => {
        val v = new Var(astv, cur)
        cur += v.size
        (v.name, v)
      })
      .toMap

    env
  }

  def calcSize(typ: ASTVarType): Int = typ match {
    case ASTArr(lbound, ubound, varType) =>
      (ubound - lbound + 1) * calcSize(varType)
    case ASTChar => 1
    case ASTInt  => 1
  }
}
