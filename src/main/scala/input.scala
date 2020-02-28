package goose

import scala.util.{Try, Success}

object input {
  def parse(in: String): Either[String, Input] =
    in.stripPrefix(" ").stripSuffix(" ") match {

      // add player
      case i if i.startsWith("add player") =>
        val name = i.stripPrefix("add player")
        validateName(name).map(AddPlayer.apply)

      // move
      case i if i.startsWith("move") =>
        val rest = i.stripPrefix("move")
        val parts = cleanSplit(rest, " ")
        if (!parts.isEmpty) for {
          name <- validateName(parts.head)
          rollText = i.stripPrefix(s"move $name")
          r = parseRoll(rollText)
        } yield Move(name, r)
        else Left("Please specify which player")

      // invalid
      case _ =>
        Left("I don't understand")

    }

  def validateName(name: String): Either[String, String] =
    name.replaceAll(" ", "") match {
      case n if n.size == 0 => Left("Please provide a player name")
      // TODO add more validations
      case n => Right(n)
    }

  def parseRoll(rollText: String): Option[Roll] = {
    val parts = cleanSplit(rollText.replaceAll(" ", ""), ",")
    parts.toList match {
      case d1 :: d2 :: Nil =>
        val tryR = for {
          n1 <- Try(d1.toInt)
          n2 <- Try(d2.toInt)
          r <- Try(Roll(n1, n2))
        } yield r
        tryR.toOption
      case _ => None
    }
  }

  def cleanSplit(s: String, sep: String): Array[String] =
    s.stripPrefix(" ").stripSuffix(" ").split(sep).filterNot(_ == "")
}
