package week2.loops

object Loops {

  def WHILE(condition: => Boolean)(command: => Unit): Unit = {
    if(condition) {
      command
      WHILE(condition)(command)
    } else ()
  }

  def REPEAT(command: => Unit)(condition: => Boolean): Unit =
    new REPEAT(command) UNTIL condition
}

class REPEAT(command: => Unit) {
  def UNTIL(condition: => Boolean): Unit = {
    command
    if (condition) () else UNTIL(condition)
  }
}