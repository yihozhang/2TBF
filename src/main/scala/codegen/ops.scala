package ttbf.codegen

package object ops {
  import ttbf.common.instrm._

  private object Code {
    val SET_ZERO                          = "!0"
    val ELIM_ZERO                         = "!0"
    val MOVE_FROM_LOCAL_TO_GLOBAL_ZERO    = "u0?0-1[o0<1+1?0-1]o0o"
    val RESTORE_FROM_GLOBAL_ZERO_TO_LOCAL = "u[>1-1]o0"
    val SET_ARRAY_START_POSITION          = "!1"
    val MOVE_BACK_FROM_FETCH_POSITION     = "?1-1[o0<1?1-1]o0!1"
  }

  private val moveToGlobalZero = StateM { (s: InstrState) =>
    if (s.pos.isGlobal)
      ((), s.toLeft(s.pos.p).push(f"<${s.pos.p}%d"))
    else
      (
        (),
        s.savePos()
          .setPos(Pos(PositionState.GLOBAL, 0))
          .push(Code.MOVE_FROM_LOCAL_TO_GLOBAL_ZERO)
      )
  }
  private val restoreToLocal: InstrM[Unit] = StateM { (s: InstrState) =>
    if (s.pos.isLocal) {
      throw new IllegalArgumentException("apply restore at a local state")
    } else {
      (
        (),
        s.push(f"<${s.pos.p}%d")
          .push(Code.RESTORE_FROM_GLOBAL_ZERO_TO_LOCAL)
          .restoreSavedPos()
      )
    }
  }

  private def setPos(pos: Pos) = StateM { (s: InstrState) =>
    ((), s.setPos(pos))
  }

  private def push(instr: String) = StateM { (s: InstrState) =>
    ((), s.push(instr))
  }

  def moveTo(pos: Pos): InstrM[Unit] =
    if (pos.isGlobal) {
      moveToGlobal(pos.p)
    } else {
      moveToLocal(pos.p)
    }

  def moveToLocal(p: Int): InstrM[Unit] =
    for {
      a <- getPos
      _ <- if (a.isLocal) {
        moveRight(p - a.p)
      } else {
        for {
          _ <- restoreToLocal
          b <- getPos
          _ <- moveRight(p - b.p)
        } yield ()
      }
    } yield ()

  def moveToGlobal(p: Int): InstrM[Unit] =
    for {
      a <- getPos
      _ <- if (a.isLocal) {
        for {
          _ <- moveToGlobalZero
          _ <- moveRight(p)
        } yield ()
      } else {
        moveRight(p - a.p)
      }
    } yield ()

  def moveLeft(x: Int): InstrM[Unit] =
    if (x == 0)
      InstrM.unit(())
    else if (x < 0)
      moveRight(-x)
    else
      StateM { (s: InstrState) =>
        ((), s.toLeft(x).push(f"<$x%d"))
      }

  def moveRight(x: Int): InstrM[Unit] =
    if (x == 0)
      InstrM.unit(())
    else if (x < 0)
      moveLeft(-x)
    else
      StateM { (s: InstrState) =>
        ((), s.toRight(x).push(f">$x%d"))
      }

  val dynamicFetchAndReplaceByOffsetOnStk = for {
    _ <- push(Code.SET_ARRAY_START_POSITION)
    _ <- push(">")
    _ <- popAway
    _ <- pushValToStk()
    _ <- push(Code.MOVE_BACK_FROM_FETCH_POSITION)
  } yield ()

  val getPos = StateM { (s: InstrState) =>
    (s.pos, s)
  }
  val popInstr = StateM { (s: InstrState) =>
    s.pop
  }
  def seq[T](instrs: List[InstrM[T]]): InstrM[Unit] = instrs.foldRight(InstrM.unit()) { (a, b) =>
    a flatMap ((_) => b)
  }

  def convertToLValAndPop: InstrM[Unit] =
    for {
      instr <- popInstr
      _ <- instr match {
        case "u" => popToArr

        case Code.MOVE_BACK_FROM_FETCH_POSITION =>
          for {
            instr2 <- popInstr
            _ <- if (instr2 == "u") for {
              _ <- popToArr
              _ <- push(instr)
            } yield ()
            else throw new IllegalStateException("Can't convert a non lval to lval with instr: " + instr2 + instr)
          } yield ()
        
        case _ => throw new IllegalStateException("Can't convert a non lval to lval with instr: " + instr)
      }
    } yield ()
  def enterSubrtState    = setPos(Pos(PositionState.LOCAL, 0))
  def enterMainBodyState = setPos(Pos(PositionState.GLOBAL, 0))
  def ifCondThenElse(cond: InstrM[Unit], thn: InstrM[Unit], els: InstrM[Unit]) =
    for {
      _   <- push("u1u0")
      _   <- cond
      pos <- getPos
      _   <- push("[o0o0o0u0u1u0]o0") // if val > 0 push {0, 1}; else push {1, 0}
      _   <- push("[")
      _   <- thn
      _   <- ops.moveTo(pos)
      _   <- push("-1") // to enforce exit
      _   <- push("]")
      _   <- push("o0") // pop the first condition var
      _   <- push("[")
      _   <- els
      _   <- ops.moveTo(pos)
      _   <- push("-1") // to enforce exit
      _   <- push("]")
      _   <- push("o0") // pop the second condition var
    } yield ()

  val decArrVal = push("u-1o")
  val printError = {
    val instrs = "Error!!"
      .map(_.toByte)
      .map(code => push("u" + code + "wo"))
      .toList
    seq(instrs)
  }
  val mainSettingUpInstr = push("!0")
  val popToArr           = push("o")
  val popAway            = push("o0")
  def pushValToStk()     = push("u")
  def setVal(v: Int) =
    for {
      _ <- pushToStk(v)
      _ <- popToArr
    } yield ()
  def pushToStk(v: Int) = push(f"u$v%d")
  val readInt           = push("R")
  val writeInt          = push("W")
  val plus              = push("+")
  val minus             = push("-")
  val startRecurse      = push("s")
  def loop(body: InstrM[Unit]) =
    for {
      _   <- push("[")
      pos <- getPos
      _   <- body
      _   <- moveTo(pos)
      _   <- push("]")
    } yield ()
}
