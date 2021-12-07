package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  //PBT
  test("PBT: selectDigits' property is all numerical") {
    forAll { (text: String) =>
      assert(selectDigits(text).forall(_.isDigit))
    }
  }

  test("secret") {
    assert(secret("abc123") == "******")
  }

  //PBT
  test("PBT: secret length should be the same as input") {
    forAll { (text: String) =>
      assert(secret(text).length == text.length)
    }
  }

  test("secret could be executed in parallel") {
    forAll { (text1: String, text2: String) =>
      val secret1 = secret(text1 + text2)
      val secret2 = secret(text1) + secret(text2)
      assert(secret1 == secret2)
    }
  }

  test("isValidUsernameCharacter") {
    assert(isValidUsernameCharacter('a'))
    assert(isValidUsernameCharacter('A'))
    assert(isValidUsernameCharacter('1'))
    assert(isValidUsernameCharacter('-'))
    assert(isValidUsernameCharacter('_'))
    assert(!isValidUsernameCharacter('~'))
    assert(!isValidUsernameCharacter('!'))
  }

  test("isValidUsername") {
    assert(isValidUsername("john-doe"))
    assert(!isValidUsername("*john*"))
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("isPositive") {
    assert(Point(2, 4, 9).isPositive)
    assert(Point(0, 0, 0).isPositive)
    assert(!Point(0, -2, 1).isPositive)
  }

  //PBT -> special case -2147483648 if use x.abs
  test("PBT: isPositive") {
    forAll(Gen.posNum[Int], Gen.posNum[Int], Gen.posNum[Int]) { (x: Int, y: Int, z: Int) =>
      assert(Point(x, y, z).isPositive)
    }
  }

  test("isEven") {
    assert(Point(2, 4, 8).isEven)
    assert(Point(0, -8, -2).isEven)
    assert(!Point(3, -2, 0).isEven)
  }

  test("forAll") {
    assert(Point(1, 1, 1).forAll(_ == 1))
    assert(!Point(1, 2, 5).forAll(_ == 1))
  }

  //PBT
  test("PBT: forAll from Point should be consistent to standard lib (e.g. List)") {
    forAll { (testX: Int, testY: Int, testZ: Int, testR: Int => Boolean) =>
      assert(List(testX, testY, testZ).forall(testR) == Point(testX, testY, testZ).forAll(testR))
    }
  }
}
