package lecture3

class BankAccount {
  private var balance = 0
  def deposit(ammount: Int): Unit = {
    if (ammount > 0) balance = balance + ammount
  }

  def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
    command
    if (condition) () else REPEAT(command)(condition)
  }

  def REPEAT2(command: => Unit): Unit = {
  }

  def a(x: Int, y: Int, z: Int): Unit = {
    println(x + y + z)
  }

}
