import lecture3.BankAccount
object account {
  println("new worksheet")
  val acct = new BankAccount
  acct deposit 50

  def a(x: Int): Int = {
    println("LOOK")
    println(x)
    x
  }

  a {
    1 +
    2
    3
  }
}

account.a {
  1 +
  2
  5
}
