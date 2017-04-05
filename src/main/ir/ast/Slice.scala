package ir.ast
import lift.arithmetic.ArithExpr
import ir.interpreter.Interpreter.ValueMap
import ir.{ArrayType, Type, TypeException, UndefType}

case class Slice(start: ArithExpr, end: ArithExpr)
    extends Pattern(arity = 1) with isGenerable {
  override def toString: String = "Slice(" + end + "," + end + ")"
  override def checkType(argType: Type, setType: Boolean): Type = {
    argType match {
      //todo @bastian include comment again, issue with pad2d(a,b, 0,0)
      case ArrayType(t, n) /*if (0 <= s <= e < len)*/ => ArrayType(t, end - start)
      case _ => throw new TypeException(argType, "ArrayType")
    }
  }
  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    args.head match {
      case a: Array[_] => a.slice(start.eval,end.eval)
    }
  }
}
