package lecture2

class Pouring(capacity: Vector[Int]) {
  val initialState = capacity.map(_ => 0)

  trait Move {
    def change(state: Vector[Int]): Vector[Int]
    override def toString: String = {
      ""
    }
  }
  case class Empty(glass: Int) extends Move {
    override def change(state: Vector[Int]): Vector[Int] = state.updated(glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    override def change(state: Vector[Int]): Vector[Int] = state.updated(glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: Vector[Int]): Vector[Int] = {
      val spaceInTo = capacity(to) - state(to)
      val takeFrom = Math.min(state(from), spaceInTo)
      state.updated(from, from - takeFrom).updated(to, to + takeFrom)
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (g1 <- glasses; g2 <- glasses if g2 != g1) yield Pour(g1, g2))

  class Path(history: List[Move]) {
    def endState: Vector[Int] = trackState(history)

    def trackState(remainingHistory: List[Move]): Vector[Int] = remainingHistory match {
      case Nil => initialState
      case move :: xs1 => move.change(trackState(xs1))
    }

    override def toString: String = {
      history.mkString(" ") + trackState(history)
    }
  }
}

