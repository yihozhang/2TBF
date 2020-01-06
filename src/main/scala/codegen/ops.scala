package ttbf.codegen

package object ops {
  import ttbf.common.instrm._
  def moveTo(pos: Pos): InstrM[Unit] =
    if (pos.isGlobal) {
      moveToGlobal(pos.p)
    } else {
      moveToLocal(pos.p)
    }
  def moveToLocal(p: Int): InstrM[Unit] =
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
  def moveToGlobal(p: Int): InstrM[Unit] =
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

  def convertToLVal: InstrM[Unit] =
    for {
      instr <- Op.pop
      _ <- if (instr.last != 'u') {
        throw new IllegalStateException("Can't convert a non lval to lval")
      } else {
        if (instr.length() == 1)
          InstrM.unit(())
        else
          Op.push(instr.substring(0, instr.length() - 1))
      }
    } yield ()
  def enterSubrtState    = Op.setPos(Pos(PositionState.LOCAL, 0))
  def enterMainBodyState = Op.setPos(Pos(PositionState.GLOBAL, 0))
  def ifCondThenElse(cond: InstrM[Unit], thn: InstrM[Unit], els: InstrM[Unit]) =
    for {
      _ <- Op.push("u1u0")
      _ <- cond
      pos <- Op.getPos
      _ <- Op.push("[o0o0o0u0u1u0]o0") // if val > 0 push {0, 1}; else push {1, 0}
      _ <- Op.push("[")
      _ <- thn
      _ <- ops.moveTo(pos)
      _ <- Op.push("-1") // to enforce exit
      _ <- Op.push("]")
      _ <- Op.push("o0") // pop the first condition var
      _ <- Op.push("[")
      _ <- els
      _ <- ops.moveTo(pos)
      _ <- Op.push("-1") // to enforce exit
      _ <- Op.push("]")
      _ <- Op.push("o0") // pop the second condition var
    } yield ()
  val decArrVal = Op.push("u-1o")
  lazy val printError = {
    val instrs = "Error!!"
      .map(_.toByte)
      .map(code => Op.push("u" + code + "wo")).toList
    Op.seq(instrs)
  }
  val mainSettingUpInstr = Op.push("!0")
}
