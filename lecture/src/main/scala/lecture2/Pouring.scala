package lecture2

class Pouring(capacity: Vector[Int]) {
  val initialState = capacity.map(_ => 0)
  val glasses = 0 until capacity.length

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
      println("takeFrom " + takeFrom + " state To " + state(to) + " state From" + state(from))
      state.updated(from, state(from) - takeFrom).updated(to, state(to) + takeFrom)
    }
  }

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (g1 <- glasses; g2 <- glasses if g2 != g1) yield Pour(g1, g2))

  class Path(history: List[Move]) {
    def endState: Vector[Int] = trackState

    def trackState: Vector[Int] = (history foldRight(initialState)) {
      (move, acc) => move.change(acc)
    }

    override def toString: String = {
      history.mkString(" ") + endState
    }

    def extend(move: Move): Path = {
      new Path(move :: history)
    }
  }

  val initialPath = new Path(Nil)

  val paths = from(Set(initialPath))

  def from(input: Set[Path]): Stream[Set[Path]] = {
    if (input.isEmpty) {
      Stream.empty
    } else {
      val more = for {
        path <- input
        next <- moves.map(path.extend)
      } yield next
      input #:: from(more)
    }
  }
}

