package opencl.generator

import lift.arithmetic._
import ir._
import ir.view.AccessVar
import opencl.generator.OpenCLAST._
import opencl.ir._

object OpenCLPrinter {
  def apply() = new OpenCLPrinter
}

/** The printer walks the AST emitted by the [[OpenCLGenerator]] and generates
  * standalone OpenCL-C code.
  */
class OpenCLPrinter {
  /**
   * Entry point for printing an AST.
   *
   * @param node The root of the AST (the global scope block).
   * @return A string representation of the AST as OpenCL-C code.
   */
  def apply(node: OclAstNode): String = {
    indent = 0
    print(node)
    sb.toString()
  }

  def toString(t: Type, seenArray: Boolean = false) : String = {
    t match {
      case ArrayType(elemT, _) =>
        val s = toString(elemT, seenArray=true)
        if (!seenArray) s + "*" else s
      case VectorType(elemT, len) => toString(elemT, seenArray) + toString(len)
      case ScalarType(name, _) => name
      case tt: TupleType => Type.name(tt)
      case NoType => "void"
      case _ => throw new NotPrintableExpression(t.toString)
    }
  }

  def toString(e: ArithExpr) : String = {
    e match {
      case Cst(c) => c.toString
      case Pow(b, ex) =>
        "(int)pow((float)" + toString(b) + ", " + toString(ex) + ")"
      case Log(b, x) => "(int)log"+b+"((float)"+toString(x)+")"
      case Prod(es) => "(" + es.foldLeft("1")( (s: String, e: ArithExpr) => {
        s + (e match {
          case Pow(b, Cst(-1)) => " / (" + toString(b) + ")"
          case _ => " * " + toString(e)
        })
      } ).drop(4) + ")" // drop(4) removes the initial "1 * "
      case Sum(es) => "(" + es.map(toString).reduce( _ + " + " + _  ) + ")"
      case Mod(a,n) => "(" + toString(a) + " % " + toString(n) + ")"
      case of: OclFunction => of.toOCLString
      case ai: AccessVar => ai.array + "[" + toString(ai.idx.content) + "]"
      case v: Var => v.toString
      case IntDiv(n, d) => "(" + toString(n) + " / " + toString(d) + ")"
      case lu: Lookup => "lookup" + lu.id + "(" + toString(lu.index) + ")"
      case i: lift.arithmetic.IfThenElse =>
        s"( (${toString(i.test.lhs)} ${i.test.op} ${toString(i.test.rhs)}) ? " +
        s"${toString(i.t)} : ${toString(i.e)} )"
      case _ => throw new NotPrintableExpression(e.toString)
    }
  }

  def toString(p: Predicate) : String = {
    s"(${toString(p.lhs)} ${p.op} ${toString(p.rhs)})"
  }


  /** Output stream for current AST */
  private val sb: StringBuilder = new StringBuilder

  private def print(s: String): Unit = {
    sb ++= s
  }

  private def println(s: String): Unit = {
    sb ++= s + "\n" + tab()
  }

  /** Current indentation (depth of scope) */
  private var indent: Int = 0

  /** Create a block between braces. */
  private def printBlock(code: => Unit): Unit = {
    indent += 1
    println("{")
    code
    indent -= 1
    moveCursorBack(tabSize)
    print("}")
  }

  /** Print the given string an create an indented new line */
  private val tabSize = 2

  /** Insert the correct indentation */
  private def tab() = {
    lazy val whiteSpace: String = " " * tabSize
    whiteSpace * indent
  }

  /** Move cursor back by given size. Used to fix indentation */
  private def moveCursorBack(size: Int): Unit = {
    for (_ <- 1 to size) {
      if (sb.last.isWhitespace) { sb.deleteCharAt(sb.size - 1) }
    }
  }

  /**
   * Main print method. Print the current node and recurse.
   *
   * @param node The current node to emit code for.
   */
  private def print(node: OclAstNode): Unit = node match {
    case b: Block =>
      if(b.global) {
        b.content.foreach(
          n => {
            print(n)
            println("")
          }
        )

      }
      else printBlock {
        b.content.foreach(n => {
          print(n)
          println("")
        })
      }

    case f: Function      => print(f)
    case i: OpenCLCode    => sb ++= i.code
    case e: OpenCLExpression => sb ++= e.code
    case c: Comment       => print(s"/* ${c.content} */")
    case v: VarDecl       => print(v)
    case v: VarRef        => print(v)
    case p: ParamDecl     => print(p)
    case b: Barrier       => print(b)
    case l: ForLoop       => print(l)
    case w: WhileLoop     => print(w)
    case es: ExpressionStatement => print(es)
    case ae: ArithExpression  => print(toString(ae.content))
    case c: CondExpression   => print(c)
    case a: AssignmentExpression    => print(a)
    case f: FunctionCall  => print(f)
    case l: Load          => print(l)
    case s: Store         => print(s)
    case t: TypeDef       => print(t)
    case a: TupleAlias    => print(a)
    case c: Cast          => print(c)
    case l: VectorLiteral => print(l)
    case e: OpenCLExtension     => print(e)
    case i: OpenCLAST.IfThenElse    => print(i)
    case l: Label         => print(l)
    case g: GOTO          => print(g)
    case b: Break         => print(b)
    case s: StructConstructor => print(s)

    case x => print(s"/* UNKNOWN: ${x.getClass.getSimpleName} */")
  }

  private def print(c: CondExpression): Unit = {
      print(c.lhs)
      print(c.cond.toString)
      print(c.rhs)
  }

  private def print(c: Cast): Unit = {
    print(s"(${c.t})")
    print(c.v)
  }

  private def print(l: VectorLiteral): Unit = {
    print(s"(${l.t})(")
    var c = 0
    l.vs.foreach( v => {
      print(v)
      c = c+1
      if (c != l.vs.length) { print(", ") }
    })
    print(")")
  }

  private def print(t: TypeDef): Unit = t.t match {
    case tt: TupleType =>
      tt.elemsT.foreach(t => print(TypeDef(t)))
      val name = Type.name(tt)
      val fields = tt.elemsT.zipWithIndex.map({case (ty,i) => Type.name(ty)+" _"+i})
      print(s"""#ifndef ${name}_DEFINED
        |#define ${name}_DEFINED
        |typedef struct {
        |  ${fields.reduce(_+";\n  "+_)};
        |} $name;
        |#endif
        |""".stripMargin)
    case _ =>
  }

  private def print(e: OpenCLExtension): Unit = {
    println(s"#pragma OPENCL EXTENSION ${e.content} : enable")
  }

  private def print(alias: TupleAlias): Unit = alias.t match {
    case tt: TupleType =>
      println(s"typedef ${Type.name(tt)} ${alias.name};")
    case _ =>
  }

  private def print(l: Load): Unit = {
      if (!UseCastsForVectors()) {
      print(s"vload${l.t.len}(")
      print(l.offset)
      print(",")
      print(l.v)
      print(")")
    } else {
      print(s"*( ((${l.openCLAddressSpace} ${l.t}*)")
      print(l.v)
      print(s") + ")
      print(l.offset)
      print(")")
    }
  }

  private def print(s: Store): Unit = {
    if (!UseCastsForVectors()) {
      print(s"vstore${s.t.len}(")
      print(s.value)
      print(",")
      print(s.offset)
      print(",")
      print(s.v)
      print(");")
    } else {
      print(s"*( ((${s.openCLAddressSpace} ${s.t}*)")
      print(s.v)
      print(s") + ")
      print(s.offset)
      print(") = ")
      print(s.value)
    }
  }

  private def print(f: FunctionCall): Unit = {
    print(f.name + "(")
    f.args.foreach(x => {
      print(x)
      if(!x.eq(f.args.last)) print(", ")
    })
    print(")")
  }

  private def print(v: VarRef): Unit = {
    print(toString(v.v))
    if(v.arrayIndex != null) {
      print("[")
      print(v.arrayIndex)
      print("]")
    }
    if(v.suffix != null) {
      print(v.suffix)
    }
  }

  private def print(f: Function): Unit = {
    if(f.kernel) sb ++= "kernel void"
    else sb ++= toString(f.ret)
    sb ++= s" ${f.name}("
    f.params.foreach(x => {
      print(x)
      if(!x.eq(f.params.last)) sb ++= ", "
    })
    sb ++= ")"
    if(f.kernel)
      sb ++= "{ \n" +
        "#ifndef WORKGROUP_GUARD\n" +
        "#define WORKGROUP_GUARD\n" + 
        "#endif\n" +
        "WORKGROUP_GUARD\n"
    print(f.body)
    if(f.kernel)
      println("}")
  }

  private def print(es: ExpressionStatement): Unit = {
    print(es.e)
    print(";")
  }


  private def print(a: AssignmentExpression): Unit = {
    print(a.to)
    print(" = ")
    print(a.value)
  }

  private def print(p: ParamDecl): Unit = p.t match {
    case ArrayType(_,_) =>
      // Const restricted pointers to read-only global memory. See issue #2.
      val (const, restrict) = if (p.const) ("const ", "restrict ") else ("","")
      print(const + p.addressSpace + " " + toString(Type.devectorize(p.t)) +
            " " + restrict + p.name)

    case x =>
      print(toString(p.t) + " " + p.name)
  }


  private def print(vd: VarDecl): Unit = vd.t match {
    case a: ArrayType =>
      vd.addressSpace match {
        case PrivateMemory =>
          for (i <- 0 until vd.length)
            println(toString(Type.getValueType(vd.t)) + " " + toString(vd.v) + "_" +
                    toString(i) + ";")

        case LocalMemory if vd.length != 0 =>
          val baseType = Type.getBaseType(vd.t)
          val declaration =
            s"${vd.addressSpace} ${toString(baseType)} ${toString(vd.v)}[${vd.length}]"

          // Make sure the memory is correctly aligned when using pointer casts
          // for forcing vector loads on NVIDIA.
          val optionalAttribute =
            if (UseCastsForVectors()) " __attribute__ ((aligned(16)));" else ";"

          val fullDeclaration = declaration + optionalAttribute

          print(fullDeclaration)

        case x =>
          val baseType = Type.getBaseType(vd.t)
          print(s"${vd.addressSpace} ${toString(baseType)} *${toString(vd.v)}")
          if(vd.init != null) {
            print(s" = ")
            print(vd.init)
          }
          print(";")
      }

    case x =>
      // hackily add support for global memory pointers, but _only_ pointers
      vd.t match {
        case IntPtr => 
          if(vd.addressSpace == GlobalMemory)
          print(vd.addressSpace + " ")
        case _ => 
      }
      if(vd.addressSpace == LocalMemory)
        print(vd.addressSpace + " ")
      print(s"${toString(vd.t)} ${toString(vd.v)}")
      if(vd.init != null) {
        print(s" = ")
        print(vd.init)
      }
      print(";")
  }

  /**
   * Generate a barrier for the given address space scope.
   * If the scope is not defined as global or local, the barrier assumes both.
 *
   * @param b A [[Barrier]] node.
   */
  private def print(b: Barrier): Unit = println (b.mem.addressSpace match {
    case GlobalMemory => "barrier(CLK_GLOBAL_MEM_FENCE);"
    case LocalMemory => "barrier(CLK_LOCAL_MEM_FENCE);"
    case _ => "barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);"
  })

  /**
    * Generate a for loop.
    *
    * @param fl a [[ForLoop]] node.
    */
  private def print(fl: ForLoop): Unit = {
    print("for (")
    print(fl.init)
    print(fl.cond)
    print(fl.increment)
    print(")")
    print(fl.body)
  }


  /**
    * Generate a while loop. This is fairly simple so no 
    * optimisations can be realistically applied.
    * 
    * @param wl a [[WhileLoop]] node.
    */
  private def print(wl: WhileLoop): Unit = {
    print("while("+ toString(wl.loopPredicate) + ")")
    print(wl.body)
  }


  /** Generate an if-then-else conditional set of statements
    * 
    * @param s a [[IfThenElse]] node
    */
  private def print(s: OpenCLAST.IfThenElse): Unit = {
    print("if(")
    print(s.cond)
    println(")")

    print(s.trueBody)

    if(s.falseBody != Block())
    {
      println("else")
      print(s.falseBody)
    }
  }

  /** Generate a label for a goto
    * 
    * @param l a [[Label]] node
    */
  private def print(l: Label): Unit = {
    println(l.nameVar.toString + ": ;")
  }

  /** Generate a goto statement for a corresponding label
    * 
    * @param g a [[GOTO]] node
    */
  private def print(g: GOTO): Unit = {
    println("goto " + g.nameVar.toString + ";")
  }

  private def print(b: Break) : Unit = {
    print("break;")
  }

  private def print(s: StructConstructor): Unit = {
    print(s"(${toString(s.t)}){")
    s.args.foreach(x => {
      print(x)
      if(!x.eq(s.args.last)) print(", ")
    })
    print("}")
  }
}
