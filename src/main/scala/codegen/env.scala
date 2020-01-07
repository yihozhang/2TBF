package ttbf.codegen
import Math._
package object env {
  import ttbf.common.ast._
  class Env(env: Map[ASTId, Var]) {
    def contains(id: ASTId) = env.contains(id)
    def apply(id: ASTId)    = env(id)
    def largestPos = {
      env.map(_._2.p).toList.max
    }
  }
  case class LocalEnv(env: Map[ASTId, Var])  extends Env(env)
  object LocalEnv {
    lazy val initial = LocalEnv(Map())
  }
  case class GlobalEnv(env: Map[ASTId, Var]) extends Env(env) {
    override def largestPos = if (env.isEmpty) 0 else super.largestPos
  }
  object GlobalEnv {
    lazy val initial = GlobalEnv(Map())
  }
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
  trait PassByReference extends Var

  private def declsToEnvImpl(vars: ASTVarDecls, cur: Int): (Map[ASTId, Var], Int) = {
    var _cur = cur
    val env = vars
      .map(astv => {
        val v = new Var(astv, _cur)
        _cur += v.size
        (v.name, v)
      })
      .toMap
    (env, _cur)
  }
  def declsToGlobalEnv(vars: ASTVarDecls): GlobalEnv =
    GlobalEnv(declsToEnvImpl(vars, 1)._1)

  def declsToLocalEnv(params: ASTParamDecls, vars: ASTVarDecls, retVal: Option[(ASTBaseVarType, ASTId)]): LocalEnv = {
    val (retValEnv: Map[ASTId, Var], (paramEnv, cur)) = retVal match {
      case None            => (Map(), declsToEnvImpl(params, 1))
      case Some((typ, id)) => (Map(id -> new Var(id, 0, 1, typ)), declsToEnvImpl(params, 1))
    }
    val (varEnv, _) = declsToEnvImpl(vars, cur)
    if ((paramEnv.keySet & varEnv.keySet).isEmpty &&
        (retValEnv.keySet & paramEnv.keySet).isEmpty &&
        (retValEnv.keySet & varEnv.keySet).isEmpty) {
      LocalEnv(retValEnv ++ paramEnv ++ varEnv)
    } else {
      throw new IllegalArgumentException("parameter and var declaration both contains variables with same name")
    }
  }

  def calcSize(typ: ASTVarType): Int = typ match {
    case ASTArr(lbound, ubound, varType) =>
      (ubound - lbound + 1) * calcSize(varType)
    case ASTChar => 1
    case ASTInt  => 1
  }

  case class SubrtEnv(table: Map[ASTId, Int]) {
    def apply(id: ASTId) = table(id)
  }
  def subrtsToSubrtEnv(subrts: List[ASTSubrt]): SubrtEnv = {
    SubrtEnv(subrts.zipWithIndex.map {
      case (subrt, idx) => (subrt.name, idx + 1)
    } toMap)
  }
}
