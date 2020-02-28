package goose

import input._

import org.scalatest._

class InputSpec extends FlatSpec with Matchers {

  "Name validation" should "not allow empty names" in {
    validateName("") shouldBe a[Left[_, _]]
    validateName("  ") shouldBe a[Left[_, _]]
  }

  it should "strip leading and trailing spaces" in {
    validateName(" Foo ") shouldBe Right("Foo")
    validateName("  Foo") shouldBe Right("Foo")
    validateName("Foo  ") shouldBe Right("Foo")
  }

  "Roll parsing" should "successfully parse well-formed roll texts" in {
    parseRoll("1, 6") shouldBe Some(Roll(1, 6))
    parseRoll("  1, 6") shouldBe Some(Roll(1, 6))
    parseRoll("  1, 6  ") shouldBe Some(Roll(1, 6))
    parseRoll("1,  6") shouldBe Some(Roll(1, 6))
    parseRoll("1  ,  6") shouldBe Some(Roll(1, 6))
    parseRoll("  1  ,  6  ") shouldBe Some(Roll(1, 6))
  }

  it should "interpret empty or invalid texts as no roll" in {
    parseRoll("") shouldBe None
    parseRoll("  ") shouldBe None
    parseRoll("1,") shouldBe None
    parseRoll("1") shouldBe None
    parseRoll("1, 3, 4") shouldBe None
    parseRoll("0, 3") shouldBe None
    parseRoll("1, 7") shouldBe None
  }

  "Input parsing" should "successfully parse well-formed input" in {
    parse("add player Foo") shouldBe Right(AddPlayer("Foo"))
    parse(" add player Foo ") shouldBe Right(AddPlayer("Foo"))
    parse("move Foo") shouldBe Right(Move("Foo", None))
    parse(" move Foo ") shouldBe Right(Move("Foo", None))
    parse("move Foo 1, 2") shouldBe Right(Move("Foo", Some(Roll(1, 2))))
    parse(" move Foo 1, 2 ") shouldBe Right(Move("Foo", Some(Roll(1, 2))))
  }

  it should "return meaningful messages upon invalid input" in {
    parse("add player") shouldBe Left("Please provide a player name")
    parse("add player  ") shouldBe Left("Please provide a player name")
    parse("move") shouldBe Left("Please specify which player")
    parse("move  ") shouldBe Left("Please specify which player")
    parse("  add player Foo") shouldBe Left("I don't understand")
    parse("  move Foo") shouldBe Left("I don't understand")
    parse("bla bla") shouldBe Left("I don't understand")
  }

}
