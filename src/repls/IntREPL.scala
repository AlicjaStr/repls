package repls

class IntREPL extends REPLBase {
    // Have a REPL of type Int
    type Base = Int
    override val replName: String = "int-repl"

    override def readEval(command: String): String = {
        val elements = command.split("\\s") // split string based on whitespace
        // TODO: complete me!
        ""
    }

    // TODO: Implement any further functions that are specifically for an IntREPL
}
