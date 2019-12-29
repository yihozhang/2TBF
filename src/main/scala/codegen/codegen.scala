package ttbf.codegen
import ttbf.common.AST._
import ttbf.common.InstrM._

object Codegen {
  private class Env(env: Map[ASTId, Var]) {
    def contains(id: ASTId) = env.contains(id)
    def apply(id: ASTId)    = env(id)
  }
  private case class LocalEnv(env: Map[ASTId, Var])  extends Env(env)
  private case class GlobalEnv(env: Map[ASTId, Var]) extends Env(env)
  private class Var(
      val name: ASTId,
      val p: Int,
      val size: Int,
      val typ: ASTVarType
  ) {
    def this(varDecl: ASTVarDecl, p: Int) {
      this(varDecl.varName, p, calcSize(varDecl.varType), varDecl.varType)
    }
  }
  private def calcSize(typ: ASTVarType): Int = typ match {
    case ASTArr(lbound, ubound, varType) =>
      (ubound - lbound + 1) * calcSize(varType)
    case ASTChar => 1
    case ASTInt  => 1
  }

  private def moveTo(pos: Pos): InstrM[Unit] = ???
  private def moveToLocal(p: Int): InstrM[Unit] =
    for {
      a <- Op.getPos
      _ <- if (a.isLocal) {
        Op.moveRight(p - a.p)
      } else {
        for {
          _ <- Op.restoreToLocal
          b <- Op.getPos
          _ <- Op.moveRight(p - b.p)
        } yield ()
      }
    } yield ()
  private def moveToGlobal(p: Int): InstrM[Unit] = 
    for {
      a <- Op.getPos
      _ <- if (a.isLocal) {
        for {
          _ <- Op.moveToGlobalZero
          _ <- Op.moveRight(p)
        } yield ()
      } else {
        Op.moveRight(p - a.p)
      }
    } yield ()
}

class Codegen(prog: ASTProg) {
  import Codegen._
  private lazy val globalVEnv = {
    var cur = 0
    val env = prog.globalVars
      .map(astv => {
        val v = new Codegen.Var(astv, cur)
        cur += v.size
        (v.name, v)
      })
      .toMap
    GlobalEnv(env)
  }

  private def genStmts(
      stmts: List[ASTStmt]
  )(implicit local: LocalEnv, global: GlobalEnv) = {
    val instrs = stmts map {
          case ASTAsg(lval, rval) =>
            for {
              _ <- genExpr(lval)
              _ <- genExpr(rval)
              // ???
            } yield ()
          case ASTRead(expr)  => ???
          case ASTWrite(expr) => ???
        }
    ???
  }
  private def genExpr(
      expr: ASTExpr
  )(implicit local: LocalEnv, global: GlobalEnv): InstrM[Unit] = expr match {
    case ASTIdxedVar(id, idx) => ???
    case ASTConst(v)          => Op.push(f"u$v%d")
    case ASTVar(id) => {
      if (local contains id) {
        moveToLocal(local(id).p)
      } else if (global contains id) {
        moveToGlobal(global(id).p)
      } else {
        throw new IllegalArgumentException("Can't find variables")
      }
    }
    case ASTPlus(lv, rv)  => ???
    case ASTMinus(lv, rv) => ???
  }
}
