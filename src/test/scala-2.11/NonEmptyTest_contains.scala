import org.scalatest.FunSuite
import Suggester._

/**
  * Created by joshuaarnold on 7/13/16.
  */
class NonEmptyTest_contains extends FunSuite {

  test("testContains") {
    val farm = createTree("Cat cow 1234;_cattle dog dart.")

    assert( (farm contains "cows") === false, "cows")
    assert( (farm contains "cow") === true, "cow")
    assert( (farm contains "co") === true, "co")
    assert( (farm contains "c") === true, "c")

    assert( (farm contains "cat") === true, "cat")
  }

}
