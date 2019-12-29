package ttbf.common

import scala.collection.immutable.Nil

package object instrm {

  object PositionState extends Enumeration {
    type PositionState = Value
    val GLOBAL, LOCAL = Value
  }
  case class Pos(s: PositionState.PositionState, p: Int) {
    val isGlobal        = s == PositionState.GLOBAL
    val isLocal         = s == PositionState.LOCAL
    def toLeft(x: Int)  = Pos(s, p - x)
    def toRight(x: Int) = toLeft(-x)
  }

  case class StateM[S, A](runState: S => (A, S)) {
    def flatMap[B](f: A => StateM[S, B]) = StateM[S, B] { (s: S) =>
      {
        val (v, ss) = this runState s
        f(v) runState ss
      }
    }
    def map[B](f: A => B) = flatMap { (a: A) =>
      StateM.unit(f(a))
    }
  }
  object StateM {
    def unit[S, A](a: A) = StateM[S, A] { (s: S) =>
      (a, s)
    }
  }

  case class InstrState(val pos: Pos, val instr: List[String], val posSaved: Option[Pos] = None) {
    def toLeft(x: Int)    = InstrState(pos.toLeft(x), instr)
    def toRight(x: Int)   = InstrState(pos.toRight(x), instr)
    def push(str: String) = InstrState(pos, str :: instr)
    def pop = instr match {
      case hd :: tl => (hd, InstrState(pos, tl))
      case Nil      => throw new IllegalArgumentException("can't pop out an empty instr stack")
    }
    def top = instr match {
      case hd :: tl => (hd, this)
      case Nil      => throw new IllegalArgumentException("can't get the top of an empty instr stack")
    }
    def setPos(newPos: Pos) = InstrState(newPos, instr)
    def savePos()           = InstrState(pos, instr, Some(pos))
    def restoreSavedPos() = posSaved match {
      case None        => throw new IllegalStateException("try to restore to a local state that does not exist")
      case Some(value) => InstrState(value, instr)
    }
  }
  type InstrM[A] = StateM[InstrState, A]
  object InstrM {
    def unit[A](a: A): InstrM[A] = StateM.unit(a)
    val initialState = InstrState(Pos(PositionState.GLOBAL, 0), Nil)
    def getState[T](instr: InstrM[T]): (T, InstrState) = instr runState initialState
    def getInstrList[T](instr: InstrM[T]): List[String] = (getState(instr))._2.instr.reverse
    def getInstrs[T](instr: InstrM[T]) = getInstrList(instr).flatten.mkString
  }

  private object Code {
    val SET_ZERO  = "!0"
    val ELIM_ZERO = "!0"
    // val MOVE_TO_ZERO = "?0-1[o0<1?0-1]"
    // NOTE: only use this when move from local to global scope
    val MOVE_FROM_LOCAL_TO_GLOBAL_ZERO    = "u0?0-1[o0<1+1?0-1]o"
    val RESTORE_FROM_GLOBAL_ZERO_TO_LOCAL = "u[>1-1]o0"
  }

  object Op {
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
    val moveToGlobalZero = StateM { (s: InstrState) =>
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
    val restoreToLocal: InstrM[Unit] = StateM { (s: InstrState) =>
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

    // def setPos(pos: Pos) = StateM { (s: InstrState) =>
    //   ((), s.setPos(pos))
    // }
    val getPos = StateM { (s: InstrState) =>
      (s.pos, s)
    }
    def push(instr: String) = StateM { (s: InstrState) =>
      ((), s.push(instr))
    }
    val pop = StateM { (s: InstrState) =>
      s.pop
    }
  }
}
