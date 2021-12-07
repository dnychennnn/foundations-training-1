package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair(0, 1).swap == Pair(1, 0))
  }

  test("Pair map") {
//    assert(Pair(0, 1).map(_ + 1) == Pair(1, 2))
//    assert(Pair("hello", "world").map(_.take(2)) == Pair("he", "wo"))

    // "identity" is a better transformation for test because it's not domain-specific
    assert(Pair(0, 1).map(identity) == Pair(0, 1))
  }

  test("Pair zipWith") {
    assert(Pair(0, 1).zipWith(Pair(2, 3))(_ + _) == Pair(2, 4))

    def replicate(iteration: Int, word: String): String = word * iteration
    assert(Pair(2, 3).zipWith(Pair("Hello ", "World "))(replicate) == Pair("Hello Hello ", "World World World "))
  }

  test("Pair decoded") {
    assert(decoded == Pair("Functional", "Programming"))
  }

  test("Pair productNames") {
    assert(products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate && examples") {
    assert((isEven && isPositive)(12))
    assert(!(isEven && isPositive)(11))
    assert(!(isEven && isPositive)(-4))
    assert(!(isEven && isPositive)(-7))
  }

  test("Predicate || examples") {
    assert((isEven || isPositive)(12))
    assert((isEven || isPositive)(11))
    assert((isEven || isPositive)(-4))
    assert(!(isEven || isPositive)(-7))
  }

  test("Predicate flip") {
    assert(isEven.flip(3) == isEven(3 + 1))
  }

  test("Predicate isValidUser") {
    assert(isValidUser(User("John", 20)))
    assert(!isValidUser(User("John", 17)))
    assert(!isValidUser(User("john", 20)))
    assert(!isValidUser(User("x", 23)))
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode("1234") == UserId(1234))
    assert(userIdDecoder.decode("-1") == UserId(-1))

    assert(Try(userIdDecoder.decode("hello")).isFailure)
    assert(Try(userIdDecoder.decode("1111111111111111")).isFailure)
  }

  test("PBT: JsonDecoder UserId round-trip") {
    forAll { number: Int =>
      val json = number.toString
      assert(userIdDecoder.decode(json) == UserId(number))
    }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020, 3, 26))
    assert(Try(localDateDecoder.decode("2020-03-26")).isFailure)
    assert(Try(localDateDecoder.decode("hello")).isFailure)
  }

//  val dateGen: Gen[LocalDate] = for {
//    year  <- Gen.choose(1900, 2100)
//    month <- Gen.choose(1, 12)
//    day   <- Gen.choose(1, 28)
//  } yield LocalDate.of(year, month, day)

  val dateGen: Gen[LocalDate] = for {
    epochDay <- Gen.choose(0, LocalDate.MAX.toEpochDay)
  } yield LocalDate.ofEpochDay(epochDay)

  test("PBT: JsonDecoder LocalDate round-trip") {
    forAll(dateGen) { localDate: LocalDate =>
      val json = "\"" + localDate.toString + "\""
      assert(localDateDecoder.decode(json) == localDate)
    }
  }

  test("JsonDecoder orElse") {
    val successDecoder: JsonDecoder[Int]  = (json: Json) => 5
    val successDecoder2: JsonDecoder[Int] = (json: Json) => 6
    val failureDecoder: JsonDecoder[Int]  = (json: Json) => throw new IllegalArgumentException("Failed to decode")

    val json = ""
    assert(successDecoder.orElse(successDecoder2).decode(json) == 5)
    assert(successDecoder.orElse(failureDecoder).decode(json) == 5)
    assert(Try(failureDecoder.orElse(failureDecoder).decode(json)).isFailure)
    assert(failureDecoder.orElse(successDecoder).decode(json) == 5)
  }

  test("JsonDecoder weirdLocalDateDecoder") {
    val date = LocalDate.of(2020, 3, 26)
    assert(weirdLocalDateDecoder.decode("\"2020-03-26\"") == date)
    assert(weirdLocalDateDecoder.decode("18347") == date)
    assert(Try(weirdLocalDateDecoder.decode("hello")).isFailure)
  }

}
