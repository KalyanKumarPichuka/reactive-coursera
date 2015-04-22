package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
      Signal {
        val cacheB = b()
        cacheB * cacheB - 4 * a() * c()
      }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val av = a()
    val bv = b()
    val cv = c()
    Signal {
      computeDelta(Signal(av), Signal(bv), Signal(cv))() match {
        case x if x < 0 => Set()
        case x if x == 0 => Set(-bv / (2 * av))
        case x => {
          Set((-bv + Math.sqrt(x)) / 2 * av, (-bv - Math.sqrt(x)) / 2 * av)
        }
      }
    }
  }
}
