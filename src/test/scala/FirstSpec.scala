import com.podsnap.scala.playground._
import org.scalatest.FlatSpec
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner

//@RunWith(classOf[JUnitRunner])
class FirstSpec extends FlatSpec {
  "An RDVal" should "become dirty when set" in {
    val x = RDVal(3.0)
    assert(x.dirty===false)
    x.set(5.0)
    assert(x.dirty===true)
    x.value()
    assert(x.dirty===false)
  }

  "An RD should" should "sully its parents" in {
    val x = RDVal(3.0)
    val y = R~1 + x
    assert(y.value()===4.0)
    assert(x.dirty===false)
    assert(y.dirty===false)
    x.set(4.0)
    assert(x.dirty===true)
    assert(y.dirty===true)
    assert(y.value()===5.0)
  }

}
