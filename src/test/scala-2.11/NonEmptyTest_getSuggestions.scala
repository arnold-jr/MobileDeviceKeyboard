import org.scalatest.FunSuite
import Suggester._
/**
  * Created by joshuaarnold on 7/14/16.
  */
class NonEmptyTest_getSuggestions extends FunSuite {

  test("testGetSuggestions") {
    val farm = createTree("Cat cow cow 1234;_cattle catheter dog dog dog duck.")

    assert( (farm getSuggestions "ca") === List(new Hint("cat",1), new Hint("catheter", 1), new Hint("cattle",1)), "1")
    assert( (farm getSuggestions "d") === List(new Hint("dog",3), new Hint("duck", 1)), "1")
    assert( (farm getSuggestions "") ===  (farm.parse sortBy(h => (- h.confidence, h.word))), "1")
  }

}
