import org.scalatest.FunSuite
import Suggester._
/**
  * Created by joshuaarnold on 7/14/16.
  */
class NonEmptyTest_getSuggestions extends FunSuite {

  test("testRemnant") {
    val farm = createTree("Cat cow 1234;_cattle dog dart.")

    assert( (farm getSuggestions "ca") === List("cattle"), "cattle")
    assert( (farm getSuggestions "c").sorted === List("cattle","cow"), "cattle, cow")
    assert( (farm getSuggestions "d").sorted === List("dart","dog"), "dart, dog")
  }

}
