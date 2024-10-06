// You do not need to modify this file
package repls
import scala.io.StdIn.readLine

class RunIntREPL {
  def main(args: Array[String]): Unit = {
    REPLFactory.makeIntREPL().run()
  }
}

object RunMultiSetREPL {
  def main(args: Array[String]): Unit = {
    REPLFactory.makeMultiSetREPL().run()
  }
}

object RunREPL {

  def main(args: Array[String]): Unit = {
    println("Adjust code in repls.RunREPL to run the other REPL")
    val repl = REPLFactory.makeIntREPL()
    // val repl = REPLFactory.makeMultiSetREPL()
    repl.run()
  }
}
