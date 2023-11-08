package popl

import js.util.JsApp

object hw07 extends JsApp:
  import js.ast._
  import js._
  import Bop._, Uop._
  /*
   * CSCI-UA.0480-055: Homework 7
   *
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your solution will _not_ be graded if it does not compile!!
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   *
   */

  /* Collections and Higher-Order Functions */
  
  /* Lists */
  
  def compressRec[A](l: List[A]): List[A] = l match
    case Nil | _ :: Nil => l
    case h1 :: (t1 @ h2 :: _) =>
      if h1 == h2 then compressRec(t1) else h1 :: compressRec(t1)
  
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    (h, acc) => 
      acc match
        case Nil => h :: acc
        case h1 :: tl => if h1 == h then acc else h :: acc
  }
  
  def mapFirst[A](f: A => Option[A])(l: List[A]): List[A] = l match
    case Nil => Nil
    case h :: t => 
      f(h) match
        case None => h :: mapFirst(f)(t) 
        case Some(hp) => hp :: t
  
  /* Search Trees */
  
  enum Tree:
    case Empty
    case Node(l: Tree, d: Int, r: Tree)

    def insert(n: Int): Tree = this match
      case Empty => Node(Empty, n, Empty)
      case Node(l, d, r) => if n > d then Node(l insert n, d, r) else Node(l, d, r insert n)
    
    def foldLeft[A](z: A)(f: (A, Int) => A): A =
      def loop(acc: A, t: Tree): A = t match
        case Empty => acc
        case Node(l, d, r) =>
          loop(f(loop(acc, l), d), r)
      loop(z, this)
    
    def pretty: String =
      def p(acc: String, t: Tree, indent: Int): String = t match
        case Empty => acc
        case Node(l, d, r) =>
          val spacer = " " * indent
          p("%s%d%n".format(spacer, d) + p(acc, l, indent + 2), r, indent + 2)
      p("", this, 0)

  import Tree._

  def treeFromList(l: List[Int]): Tree =
    l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }
  
  def sum(t: Tree): Int = t.foldLeft(0){ (acc, d) => acc + d }
  
  def strictlyOrdered(t: Tree): Boolean =
    val (b, _) = t.foldLeft((true, None: Option[Int])) {
      (acc, d) =>
        acc match
          case (b, Some(d1)) => (b && d1 > d, Some(d))
          case (b, None) => (b, Some(d))
    }
    b
  
  /** alternative
  def strictlyOrdered(t: Tree): Boolean =
    val (b, _) = t.foldLeft((true, None: Option[Int])){
      case ((b, dp), d) => (dp map (d1 => b && d1 > d) getOrElse b, Some(d))
    }
    b
  */
  
  
  /* JakartaScript */
  
  def toNum(v: Val): Double =
    v match
      case Num(n) => n
      case Bool(false) => 0
      case Bool(true) => 1
      case Undefined => Double.NaN
      case Str(s) => try s.toDouble catch { case _: Throwable => Double.NaN }
      case Function(_, _, _) => Double.NaN
  
  def toBool(v: Val): Boolean =
    v match
      case Num(n) if (n compare 0.0) == 0 || (n compare -0.0) == 0 || n.isNaN => false
      case Num(_) => true
      case Bool(b) => b
      case Undefined => false
      case Str("") => false
      case Str(_) => true
      case Function(_, _, _) => true
  
  def toStr(v: Val): String =
    v match
      case Num(n) => if n.isWhole then "%.0f" format n else n.toString
      case Bool(b) => b.toString
      case Undefined => "undefined"
      case Str(s) => s
      case Function(_, _, _) => "function"

  
  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   */
  def inequalityVal(bop: Bop, v1: Val, v2: Val): Boolean =
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match
      case (Str(s1), Str(s2)) =>
        (bop: @unchecked) match
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
      case _ =>
        val (n1, n2) = (toNum(v1), toNum(v2))
        (bop: @unchecked) match
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
    
  /* 
   * Substitutions e[v/x]
   */
  def subst(e: Expr, x: String, v: Val): Expr =
    require(closed(v))
    /* Simple helper that calls substitute on an expression
     * with the input value v and variable name x. */
    def substX(e: Expr): Expr = subst(e, x, v)
    /* Body */
    e match
      case Num(_) | Bool(_) | Undefined | Str(_) => e
      case Var(y) => if (x == y) v else e
      case Print(e1) => Print(substX(e1))
      case UnOp(uop, e1) => UnOp(uop, substX(e1))
      case BinOp(bop, e1, e2) => BinOp(bop, substX(e1), substX(e2))
      case If(b, e1, e2) => If(substX(b), substX(e1), substX(e2))
      case ConstDecl(y, ed, eb) => 
        ConstDecl(y, substX(ed), if x == y then eb else substX(eb))
      case Call(e0, es) => Call(substX(e0), es map substX)
      case Function(p, ys, eb) => 
        if p.contains(x) || ys.contains(x) then e else Function(p, ys, substX(eb))

  
  /*
   * This code is a reference implementation of JakartaScript without
   * functions and big-step static binding semantics.
   */
  def eval(e: Expr): Val =
    require(closed(e), "eval called on non-closed expression:\n" + e.prettyJS)
    /* Some helper functions for convenience. */
    def eToNum(e: Expr): Double = toNum(eval(e))
    def eToBool(e: Expr): Boolean = toBool(eval(e))
    e match
      /* Base Cases */
      case v: Val => v
      
      /* Inductive Cases */
      
      // EvalPrint
      case Print(e) => println(eval(e).prettyVal); Undefined

      // EvalUMinus
      case UnOp(UMinus, e1) => Num(- eToNum(e1))

      // EvalNot
      case UnOp(Not, e1) => Bool(! eToBool(e1))
      
      // EvalPlus*
      case BinOp(Plus, e1, e2) => (eval(e1), eval(e2)) match
        case (Str(s1), v2) => Str(s1 + toStr(v2))
        case (v1, Str(s2)) => Str(toStr(v1) + s2)
        case (v1, v2) => Num(toNum(v1) + toNum(v2))
      
      // EvalArith
      case BinOp(Minus, e1, e2) => Num(eToNum(e1) - eToNum(e2))
      case BinOp(Times, e1, e2) => Num(eToNum(e1) * eToNum(e2))
      case BinOp(Div, e1, e2) => Num(eToNum(e1) / eToNum(e2))
      
      // EvalAnd*
      case BinOp(And, e1, e2) => 
        val v1 = eval(e1)
        if toBool(v1) then /* EvalAndTrue */ eval(e2)
        else /* EvalAndFalse */ v1
      
      // EvalOr* 
      case BinOp(Or, e1, e2) =>
        val v1 = eval(e1)
        if toBool(v1) then /* EvalOrTrue */ v1
        else /* EvalOrFalse */ eval(e2)
      
      // EvalSeq
      case BinOp(Seq, e1, e2) => eval(e1); eval(e2)
      
      // EvalEqual, EvalTypeErrorEqual1, EvalTypeErrorEqual2
      case BinOp(bop, e1, e2) =>
        bop match
          case Eq | Ne => 
            def checkFun(v: Expr): Unit = v match
              case Function(_, _, _) => throw DynamicTypeError(e)
              case _ => ()
            val v1 = eval(e1)
            checkFun(v1)
            val v2 = eval(e2)
            checkFun(v2)
            (bop: @unchecked) match
               case Eq => Bool(v1 == v2)
               case Ne => Bool(v1 != v2)
          case _ => Bool(inequalityVal(bop, eval(e1), eval(e2)))
              
      // EvalIf*
      case If(e1, e2, e3) => 
        if eToBool(e1) then /* EvalIfThen */ eval(e2)
        else /* EvalIfElse */ eval(e3)
      
      // EvalConstDecl
      case ConstDecl(x, ed, eb) => 
        eval(subst(eb, x, eval(ed)))
      
      // EvalCall, EvalCallRec, EvalTypeErrorCall
      case Call(e0, es) => 
        val v0 = eval(e0)
        v0 match
          case Function(p, xs, eb) => 
            val ebp = p match
              case None => eb
              case Some(x0) => subst(eb, x0, v0)
            // evaluate es and extend result with Undefined values if es.size < xs.size
            val vs_padded = es.map(eval).padTo(xs.size, Undefined)
            // compute common substitutions for EvalCall and EvalCallRec rules
            val ebpp = xs.lazyZip(vs_padded).foldRight(ebp){
              case ((xi, vi), ebpp) => subst(ebpp, xi, vi)
            }
            eval(ebpp)
          case _ => throw DynamicTypeError(e)
        
      case Var(_) => throw StuckError(e) // this should never happen
   
  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def evaluate(s: String): Val = eval(parse.fromString(s))
    
  /* Interface to run your interpreter from the command line.  You can ignore the code below. */ 
  
  def processFile(file: java.io.File): Unit =
    if debug then
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    
    val expr = handle(fail()) {
      parse.fromFile(file)
    }
      
    if debug then
      println("Parsed expression:")
      println(expr)
    
    handle(()) {
      val v = eval(expr)
      println(v.prettyVal)
    }

end hw07