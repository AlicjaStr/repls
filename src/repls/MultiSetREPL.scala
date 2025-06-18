package repls
import scala.collection.mutable

class MultiSetREPL extends REPLBase {

    override type Base = MultiSet[String]
    override val replName: String = "multiset-repl"
    override val bindings: mutable.Map[String, MultiSet[String]] = mutable.Map()

    override def isValue(el: String): Boolean = {
        el.startsWith("{") && el.endsWith("}")
    }

    override def toExpression(el: String): Expression = {
        Const(MultiSet(el.drop(1).dropRight(1).split(",").map(_.trim).filter(_.nonEmpty).toSeq))
    }

    override def getOperator(l: Base, name: String, r: Base): Base = {
        name match {
            case "+" => l + r
            case "-" => l - r
            case "*" => l * r
        }
    }
    override def defaultSimplify(): MultiSet[String] = MultiSet.empty

    override def simplifyExp(exp: Expression): Expression = {
        exp match {
            case Operator(l, "*", r) if l == r => simplify(l)
            case Operator(Const(a), "+", Const(b)) => Const(a + b)
            case Operator(Const(a), "-", Const(b)) => Const(a - b)
            case Operator(Const(a), "*", Const(b)) => Const(a * b)
            case Operator(l, op, r) => Operator(simplify(l), op, simplify(r))
            case _ => exp
        }
    }
}
