package ttbf.common


package object InstrM {
    case class Pos(isLocal: Boolean, p: Int) {
        def isGlobal = !isLocal
        def toLeft(x: Int) = Pos(isLocal, p - x)
        def toRight(x: Int) = toLeft(-x)
    }
    case class StateM[S,A](runState: S => (A, S)) {
        def flatMap[B](f: A => StateM[S,B]) = StateM[S,B] {
            (s:S) => {
                val (v, ss) = this runState s
                f(v) runState ss
            }
        }
        def map[B](f: A => B) = flatMap { (a: A) => StateM.pure(f(a)) }
    }
    object StateM {
        def pure[S, A](a: A) = StateM[S,A] {
            (s:S) => (a, s)
        }
    }
    // case class StateT[S, A](val runStateT: S => Writer[(A, S)]) {
    //     def flatMap[B](f: A => StateT[S,B]) = 
    //         (s: S) => for (
    //             t <- this.runStateT(s)
    //          ) yield {
    //             val (v, ss) = t
    //             f(v).runStateT(ss)
    //         }
    // }
    // object StateT {
    //     def lift[S,A](c: Writer[A]): StateT[S,A] = StateT { 
    //         (s: S) => c flatMap { (a: A) => Writer.pure (a,s) }
    //     }
    // }
    
    object Instr {
        type InstrM[A] = StateM[Pos, A]
        // def moveLeft(x: Int): InstrM[Unit] = StateT { (pos: Pos) => Writer.pure(((), pos.toLeft(x)))}
        // def moveRight(x: Int): InstrM[Unit] = StateT { (pos: Pos) => Writer.pure(((), pos.toRight(x)))}
        // def tell(x:String): InstrM[Unit] = StateT.lift(Writer.tell(x))
    }
}