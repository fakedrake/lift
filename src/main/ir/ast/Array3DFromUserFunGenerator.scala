package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap

case class Array3DFromUserFunGenerator(f: UserFun,
                                       override val at: ArrayType) extends ArrayConstructors(at) {
  override def copy: Expr = Array3DFromUserFunGenerator(f, at)

  override def eval(valueMap: ValueMap): Any = {
    at match {
      case ArrayType(ArrayType(ArrayType(_, o_), n_), m_) =>
        val o = o_.eval
        val n = n_.eval
        val m = m_.eval
        Array.tabulate(m, n, o)( (i, j, k) => f.eval(valueMap, i, j, k, m, n, o) )
      case _ =>
        throw new Exception(s"Expected Array(Array(_, _), _)")
    }
  }

}