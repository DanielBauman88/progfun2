package lecture4

class Circuit {
  class Wire() {
    var signal = false
    private var actions: List[Action] = Nil
    def getSignal(): Boolean = {
      signal
    }

    def setSignal(v: Boolean): Unit = {
      if (signal != v) {
        signal = v
        actions foreach (_())
      }
    }

    def addAction(a: Action): Unit = {
      a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    val inputValue = input.getSignal()
    input.addAction(() => output.setSignal(!inputValue))
  }

  def andGate(a1: Wire, a2: Wire, output: Wire): Unit = {
    val a1Value = a1.getSignal()
    val a2Value = a2.getSignal()
    a1.addAction(() => output.setSignal(a1Value & a2Value))
    a2.addAction(() => output.setSignal(a1Value & a2Value))
  }

  def orGate(o1: Wire, o2: Wire, output: Wire): Unit = {
    val o1Value = o1.getSignal()
    val o2Value = o2.getSignal()
    o1.addAction(() => output.setSignal(o1Value | o2Value))
    o2.addAction(() => output.setSignal(o1Value | o2Value))
  }

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d = new Wire
    val e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fulAdder(a: Wire, b:Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s = new Wire
    val c1 = new Wire
    val c2 = new Wire
    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }

  type Action = () => Unit

  trait Simulation {
    def currentTime: Int = ???
    def afterDelay(delay: Int)(block: => Unit): Unit = ???
    def run: Unit = ???
  }

}
