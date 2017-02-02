package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      delta() match {
        case negative if negative < 0 => Set()
        case zero if zero == 0 => Set(b() * -1 / (a() * 2))
        case pos => Set((b() * -1 + Math.sqrt(pos)) / (a() * 2), (b() * -1 - math.sqrt(pos)) / (a() * 2))
      }

    }
  }
}
