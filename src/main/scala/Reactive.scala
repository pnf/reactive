package com.podsnap.scala.playground
import java.lang.String
import scala.collection.mutable
import com.typesafe.scalalogging.slf4j.Logging
import scala.math._

// xTODO: logging [Still want to understand how lazy interpolation works.)
// xTODO: propagate dirty bit
// xTODO: quadrature using reactives for integrand.
// TODO: thread-safety during evaluation, parallel eval() of children, with proper handling of shared references
// TODO: deep copy of integrand
// TODO: temporal model with harmonic history and immutables

object Counter {
  var i : Integer = 0
  def id() : Integer = {
    this.synchronized {
    i = i+1
    i
    }
  }
}

abstract class Reactive extends Logging {
  var dirty = false
  val parents = new mutable.WeakHashMap[Reactive,Integer]()

  def sully(indentForLogging : Integer = 1) {
    if(dirty) {return}
    logger.debug(s"${"*"*indentForLogging}sullying $this")
    dirty = true
    parents.map { case (p,_) => if(!p.dirty) p.sully(indentForLogging+1) }
  }

  def addparent(p : Reactive) {
    parents.put(p,1)
    p.sully()
  }
}



abstract class RD extends Reactive { //reactive double
  var v = 0.0
  val id : Integer
  def eval() : Double

  def value() = {
    logger.debug(s"old value of $this is $v; dirty=$dirty")
    if(dirty) {
      v = eval()
      dirty = false
    }
    logger.debug(s"new value of $this is $v")
    v
  }
  // provide one of these
  def makeme(dict : mutable.HashMap[Integer,RD]) : RD
  def copy() : RD = copy(new mutable.HashMap[Integer,RD]())
  def copy(dict : mutable.HashMap[Integer,RD]) : RD = {
    dict.get(id) match {
      case Some(r) => r
      case None => {
        val r = makeme(dict)
        dict += id -> r
        r
      }
    }
  }

  def +(rhs : RD) : RD = new RDAdder(this,rhs)
  def *(rhs : RD) : RD = new RDMultiplier(this,rhs)
  def /(rhs : RD) : RD = new RDDivider(this,rhs)
  def fib : RD = new RDFibonacci(this)
  def fact : RD = new RDFactorial(this)
}

case class RDConst(vv : Double, id : Integer  = Counter.id()) extends RD {
  v = vv
  def eval() : Double = vv
  def makeme(dict : mutable.HashMap[Integer,RD]) = {RDConst(v)}
}

case class RDVal(vv : Double = 0.0, id : Integer = Counter.id()) extends RD {
  v = vv
  override def toString = "RDVal(" + v + "," + id + ")"
  def set(u : Double)  = {
    v = u
    sully()
    this
  }
  def makeme(dict : mutable.HashMap[Integer,RD]) : RD = {RDVal(v)}
  def eval() : Double = v

}

case class RDMultiplier(lhs : RD, rhs : RD, id : Integer = Counter.id()) extends RD {
  lhs.addparent(this)
  rhs.addparent(this)
  def makeme(dict : mutable.HashMap[Integer,RD]) : RD = {RDMultiplier(lhs.copy(dict),rhs.copy(dict))}
  def eval() = {
    v = lhs.value * rhs.value
    v
  }
}

case class RDDivider(lhs: RD, rhs: RD, id : Integer = Counter.id()) extends RD {
  lhs.addparent(this)
  rhs.addparent(this)
  def makeme(dict : mutable.HashMap[Integer,RD]) : RD = {RDDivider(lhs.copy(dict),rhs.copy(dict))}
  def eval() = {
    v = lhs.value / rhs.value
    v
  }
}

case class RDAdder(lhs : RD, rhs : RD, id : Integer = Counter.id()) extends RD {
  lhs.addparent(this)
  rhs.addparent(this)
  def makeme(dict : mutable.HashMap[Integer,RD]) : RD = {RDAdder(lhs.copy(dict),rhs.copy(dict))}
  def eval(): Double = {
    v = lhs.value + rhs.value
    v
  }
}

object Mathy extends Logging {
  val SQRT_PI = sqrt (Pi)
  val EPSILON = 1E-9
  def fac (k: Long): Long = {
    var prod: Long = 1
    for (i <- 2.asInstanceOf [Long] to k) prod *= i
    prod
  }
  def laGamma(x:Double):Double={
    lazy val p=Seq(676.5203681218851, -1259.1392167224028, 771.32342877765313,
      -176.61502916214059, 12.507343278686905, -0.13857109526572012,
      9.9843695780195716e-6, 1.5056327351493116e-7)

    if(x < 0.5) {
      math.Pi/(math.sin(math.Pi*x)*laGamma(1-x))
    } else {
      val x2=x-1
      val t=x2+7+0.5
      val a=p.zipWithIndex.foldLeft(0.99999999999980993)((r,v) => r+v._1/(x2+v._2+1))
      math.sqrt(2*math.Pi)*math.pow(t, x2+0.5)*math.exp(-t)*a
    }
  }

  def fib(prev2 : (Long, Long), n : Long) : (Long,Long) = {
    val next2 = (prev2._2, prev2._1+prev2._2)
    if(n==3) next2 else fib(next2,n-1)
  }

  def fib(n : Long) : Long =
    n match {
      case m if(m<0)  => 0
      case 1 => 1
      case 2 => 1
      case m => fib((1,1),m)._2
    }

  type Method = (Double=>Double, Double, Double) => Double
  def simpson(f:Double=>Double, a:Double, b:Double)=(f(a)+4*f((a+b)/2)+f(b))/6
  def trapezoid(f:Double=>Double, a:Double, b:Double)=(f(a)+f(b))/2
  def integrate_n(f:Double=>Double, a:Double, b:Double, steps:Int, m:Method)= {
    val delta:Double=(b-a)/steps
    delta*(a until b by delta).foldLeft(0.0)((s,x) => s+m(f, x, x+delta))
  }
  // List(1,2,3,4,5,6).view.scanLeft(0)(_+_).takeWhile(x => x < 10).last
  def integrate(f:Double=>Double, a:Double, b:Double, eps:Double, m:Method): Double = {
    // interwebs notwithstanding, the immediately below doesn't exit loop correctly
    /*
    def step(ev:(Double,Double),n:Int) : (Double,Double) = {
      logger.info(s"Step $ev $n")
      val v = integrate_n(f,a,b,n,m)
      (abs(v-ev._2),v)
    }
    List(10,100,1000,10000,100000,1000000).view.scanLeft((eps*2, 0.0))(step).takeWhile(x => x._1>eps).last._2
    */
    var n = 10
    var e = eps*2.0
    var v = 0.0
    while(e>eps && n<1000000) {
      logger.info(s"Step $e $v $n")
      val vold = v
      v = integrate_n(f,a,b,n,m)
      e = abs(v-vold)
      n = n*2
    }
    v
  }
}

case class RDQuad(yy:RD ,xx:RDVal)(a:RD,b:RD, eps : RD, idd : Integer = Counter.id()) extends RD{
  val id = idd
  val dict = new mutable.HashMap[Integer,RD]
  val y = yy.copy(dict)
  // There has got to be a better way...
  val x = dict.get(xx.id) match {case Some(r) => r match {case r : RDVal => r}
                                 case _ => RDVal(v)}
  a.addparent(this)
  b.addparent(this)
  eps.addparent(this)
  def makeme(dict : mutable.HashMap[Integer,RD]) : RD = {new RDQuad(y,x)(a.copy(dict),b.copy(dict),eps.copy(dict))}

  def eval() : Double = {
    val av = a.value()
    val bv = b.value()
    val ev = eps.value()
    Mathy.integrate(vv  => { x.set(vv); y.value()}, av, bv, ev, Mathy.trapezoid)
    /*
    var n = 5
    var error = ev*2
    var vret = 0.0
    var vnew = 0.0
    while(error>ev && n<1000000) {
      vnew = Mathy.integrate_n(vv  => { x.set(vv); y.value()}, av, bv, n, Mathy.trapezoid)
      error = abs(vnew - vret)
      vret = vnew
      n = n * 2
    }
    vret
      */
  }
}

case class RDFactorial(arg : RD, id : Integer = Counter.id()) extends RD {
  arg.addparent(this)
  def makeme(dict : mutable.HashMap[Integer,RD]) : RD = {new RDFactorial(arg.copy())}
  def eval(): Double = Mathy.laGamma(arg.value()+1.0)
}


case class RDFibonacci(arg : RD, id : Integer = Counter.id()) extends RD {
  arg.addparent(this)
  def makeme(dict : mutable.HashMap[Integer,RD]) : RD = {new RDFibonacci(arg.copy())}
  def eval() : Double = Mathy.fib(arg.value().toLong).toDouble
}

case class RF1(f : Double=>Double, arg: RD, id : Integer = Counter.id()) extends RD {
  arg.addparent(this)
  def makeme(dict : mutable.HashMap[Integer,RD]) : RD = {new RF1(f,arg.copy())}

  def eval() : Double = f(arg.value())
}

object Implicits {
  implicit def double2RDConst(x : Double) = new RDConst(x)
}
import Implicits._

object R {
  def ~(x : Double) : RDConst = new RDConst(x)
}

  object MainReactive {
    def main(args : Array[String]) {}
    var foo : RD = _
    foo = R ~ 3.0 + 8.0
    println(foo);  println(foo.value())
    var x  = RDVal().set(3.14)
    foo = R ~ 1.0 + x * 2.0
    println(foo); println(foo.value())
    x.set(2.71828)
    println(foo); println(foo.value())
    foo = (R~5.0).fact
    println(foo); println(foo.value())
    foo = (R~100).fib
    println(foo); println(foo.value())

    val y = R~1 / x
    var eps = RDVal(0.01)
    val z = RDQuad(y,x)(0.1,1.0,eps)
    println(z); println(z.value())
    eps.set(0.001)
    println(z); println(z.value())

}
