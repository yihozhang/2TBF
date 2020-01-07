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

  case class InstrState(val pos: Pos, val instr: List[String], val posSaved: Option[Pos]) {
    def toLeft(x: Int)    = InstrState(pos.toLeft(x), instr, posSaved)
    def toRight(x: Int)   = InstrState(pos.toRight(x), instr, posSaved)
    def push(str: String) = InstrState(pos, str :: instr, posSaved)
    def pop = instr match {
      case hd :: tl => (hd, InstrState(pos, tl, posSaved))
      case Nil      => throw new IllegalArgumentException("can't pop out an empty instr stack")
    }
    def top = instr match {
      case hd :: tl => (hd, this)
      case Nil      => throw new IllegalArgumentException("can't get the top of an empty instr stack")
    }
    def setPos(newPos: Pos) = InstrState(newPos, instr, posSaved)
    def savePos()           = InstrState(pos, instr, Some(pos))
    def restoreSavedPos() = posSaved match {
      case None        => throw new IllegalStateException("try to restore to a local state that does not exist")
      case Some(value) => InstrState(value, instr, None)
    }
  }
  type InstrM[A] = StateM[InstrState, A]
  object InstrM {
    def unit[A](a: A): InstrM[A]                        = StateM.unit(a)
    val initialState                                    = InstrState(Pos(PositionState.LOCAL, 0), Nil, None)
    def getState[T](instr: InstrM[T]): (T, InstrState)  = instr runState initialState
    def getInstrList[T](instr: InstrM[T]): List[String] = (getState(instr))._2.instr.reverse
    def getInstrs[T](instr: InstrM[T])                  = getInstrList(instr).flatten.mkString
  }

}
