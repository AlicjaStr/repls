package repls
import scala.collection.mutable

abstract class REPLBase extends REPL {
    type Base
    val bindings: mutable.Map[String, Base] = mutable.Map()

    def isValue(el: String): Boolean

    def toExpression(str: String): Expression

    def getOperator(l: Base, name: String, r: Base): Base

    def defaultSimplify(): Base

    def simplifyExp(exp: Expression): Expression

    def readEval(command: String): String = {
        val elements = command.split("\\s")

        if (elements.startsWith("@"))
            return evaluate(elements.map(_.trim).drop(1))
        if (elements.contains("="))
            assignVar(command)
        else
            evaluate(elements)
    }

    private def evaluate(elements: Array[String]): String = {
        val exprTree = rpnToExpression(infixToRPN(elements))
        val simplified = checkSimplify(exprTree)

        try {
            val result = eval(bindings, simplified)
            result.toString
        } catch {
            case _: NoSuchElementException => simplified.toString
        }
    }

    private def assignVar(command: String): String = {
        val elements = command.split("=", 2)
        val variable = elements(0).trim
        val result = eval(bindings, rpnToExpression(infixToRPN(elements(1).trim.split("\\s"))))

        bindings(variable) = result
        s"$variable = $result"
    }

    private def infixToRPN(elements: Array[String]): Array[String] = {
        val operators = mutable.Stack[String]()
        val output = mutable.Stack[String]()

        for (el <- elements) {
            if (isValue(el) || el.matches("-?\\d+"))
                output.push(el)
            else if (el == "(")
                operators.push(el)
            else if (el == ")") {
                while (operators.top != "(" && operators.nonEmpty)
                    output.push(operators.pop())
                operators.pop()
            } else if (bindings.contains(el))
                output.push(bindings(el).toString)
            else if (isOperator(el)) {
                while (operators.nonEmpty && operators.top != "(" && getPrecedence(operators.top) >= getPrecedence(el))
                    output.push(operators.pop())
                operators.push(el)
            } else
                output.push(el)
        }
        while (operators.nonEmpty)
            output.push(operators.pop())
        output.reverse.toArray
    }

    private def rpnToExpression(elements: Seq[String]): Expression = {
        val stack: mutable.Stack[Expression] = mutable.Stack()

        for (el <- elements) {
            if (isValue(el) || el.matches("-?\\d+"))
                stack.push(toExpression(el))
            else if (isOperator(el)) {
                val right = stack.pop()
                val left = stack.pop()
                stack.push(Operator(left, el, right))
            } else
                stack.push(Var(el))
        }
        stack.top
    }

    private def checkSimplify(exp: Expression): Expression = {
        var currentExp = exp
        var simplifiedExp = simplify(currentExp)

        while (currentExp != simplifiedExp) {
            currentExp = simplifiedExp
            simplifiedExp = simplify(currentExp)
        }
        simplifiedExp
    }

    private def isOperator(el: String): Boolean = {
        if (el == "+" || el == "*" || el == "-") true
        else false
    }

    private def getPrecedence(op: String): Int = {
        op match {
            case "+" => 2
            case "-" => 2
            case "*" => 3
        }
    }

    private def eval(bindings: mutable.Map[String, Base], exp: Expression): Base =
        exp match {
        case Const(value) => value
        case Var(name) =>
            if (!bindings.contains(name)) throw new NoSuchElementException()
            else bindings(name)
        case Operator(lhs, op, rhs) =>
            getOperator(eval(bindings, lhs), op, eval(bindings, rhs))
    }

    def simplify(exp: Expression): Expression = {
        exp match {
        case Operator(Const(a), "+", rhs) if a == defaultSimplify() => simplify(rhs)
        case Operator(lhs, "+", Const(a)) if a == defaultSimplify() => simplify(lhs)
        case Operator(Const(a), "*", _) if a == defaultSimplify() => Const(defaultSimplify())
        case Operator(_, "*", Const(a)) if a == defaultSimplify() => Const(defaultSimplify())
        case Operator(lhs, "-", rhs) if lhs == rhs => Const(defaultSimplify())
        case _ => simplifyExp(exp)
        }
    }

    abstract class Expression

    case class Const(const: Base) extends Expression {
        override def toString: String = const.toString
    }

    private case class Var(str: String) extends Expression {
        override def toString: String = str
    }

    case class Operator(lhs: Expression, operator: String, rhs: Expression) extends Expression {
        override def toString: String = {
            val left = lhs match {
                case Operator(_, op, _) if getPrecedence(op) < getPrecedence(operator) =>
                    s"( $lhs )"
                case _ => lhs.toString
            }
            val right = rhs match {
                case Operator(_, op, _) if getPrecedence(op) <= getPrecedence(operator) =>
                    s"( $rhs )"
                case _ => rhs.toString
            }
            s"$left $operator $right"
        }
    }
}
