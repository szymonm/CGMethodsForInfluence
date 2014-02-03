package pl.szymonmatejczyk.competetiveShapley

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.HashMap
import pl.szymonmatejczyk.competetiveShapley.utils.PIMap

class PairIndexedMapTest  extends FlatSpec with ShouldMatchers {
	val m = new PIMap[Int, Double](0.0)
	
	"map functionality" should "work" in {
	  m += (1, 1) -> 2.0
	  m += (2, 1) -> 3.0
	  assert(m((1,1)) === 2.0)
	  assert(m((2,1)) === 3.0)
	  assert(m.get((3,3)) === None)
	}
	
	"first iterator" should "work" in {
	  m += (3,1) -> 1.0
	  m += (3,2) -> 2.0
	  val it = m.byFirstIterator(3)
	  it.next should equal ((3,1), 1.0)
	  it.next should equal ((3,2), 2.0)
	  it.hasNext should equal (false)
	}
	
	"second iterator" should "work" in {
	  m += (1, 4) -> 1.0
	  m += (2, 4) -> 2.0
	  val it = m.bySecondIterator(4)
	  it.next should equal ((1, 4), 1.0)
	  it.next should equal ((2, 4), 2.0)
	  it.hasNext should equal (false)
	}
	
	"default value" should "be 0.0" in {
	  m(7,7) should equal (0.0)
	}
	
}