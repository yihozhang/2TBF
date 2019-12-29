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
        Op.push(instr.substring(0, instr.length() - 1))
      }
    } yield ()

}
