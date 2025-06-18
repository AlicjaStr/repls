package repls
import scala.collection.mutable

class IntREPL extends REPLBase {

    type Base = Int
    override val replName: String = "int-repl"
    override val bindings: mutable.Map[String, Int] = mutable.Map()

    override def isValue(el: String): Boolean = {
        for(i <- el.toCharArray)
            if(!i.isDigit) return false
        if(el.isEmpty)
            false
        else true
    }

    override def toExpression(el: String): Expression = Const(el.toInt)

    override def getOperator(l: Base, name: String, r: Base): Base = {
        name match {
            case "+" => l + r
            case "-" => l - r
            case "*" => l * r
        }
    }

    override def defaultSimplify(): Base = 0

    override def simplifyExp(exp: Expression): Expression = {
        exp match {
            case Operator(Const(l), "+", Const(r)) => Const(l + r)
            case Operator(Const(l), "*", Const(r)) => Const(l * r)

            case Operator(Const(1), "*", e) => simplify(e)
            case Operator(e, "*", Const(1)) => simplify(e)

            case Operator(Operator(a1, "*", b), "+", Operator(a2, "*", c)) if a1 == a2 =>
                Operator(a1, "*", simplify(Operator(b, "+", c)))
            case Operator(Operator(b, "*", a1), "+", Operator(a2, "*", c)) if a1 == a2 =>
                Operator(a1, "*", simplify(Operator(b, "+", c)))
            case Operator(Operator(a1, "*", b), "+", Operator(c, "*", a2)) if a1 == a2 =>
                Operator(a1, "*", simplify(Operator(b, "+", c)))
            case Operator(Operator(b, "*", a1), "+", Operator(c, "*", a2)) if a1 == a2 =>
                Operator(a1, "*", simplify(Operator(b, "+", c)))

            case Operator(l, op, r) => Operator(simplify(l), op, simplify(r))
            case _ => exp
        }
    }
}
