package goose

import input._, game._

import scala.io.StdIn

object console {
  def main(args: Array[String]): Unit = {
    implicit val config = Config(
      playerCount = 2,
      winAt = 63,
      bridgeAt = (6, 12),
      goosesAt = List(5, 9, 14, 18, 23, 27),
      withPrank = args.contains("-withPrank")
    )
    step(Game.init)
  }

  def step(game: Game)(implicit config: Config): Unit =
    game match {
      case Finished(winner) =>
      case _ =>
        val read = StdIn.readLine("> ")
        parse(read) match {
          case Left(msg) =>
            println(msg)
            step(game)
          case Right(in) =>
            val (result, msg) = play(game, in)
            println(msg)
            step(result)
        }
    }

}
