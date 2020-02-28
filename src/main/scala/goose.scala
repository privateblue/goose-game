package object goose {

  type Player = String
  type Square = Int
  type Message = String

  case class Config(
      playerCount: Int,
      winAt: Int,
      bridgeAt: (Int, Int),
      goosesAt: List[Int],
      withPrank: Boolean
  ) {
    assert(playerCount > 0)
    assert(winAt > 0)
    assert(bridgeAt._1 < winAt && bridgeAt._2 < winAt)
    assert(goosesAt.forall(_ < winAt))
  }

  case class Roll(dice1: Int, dice2: Int) {
    assert(dice1 >= 1 && dice1 <= 6)
    assert(dice2 >= 1 && dice2 <= 6)
  }

  type Squares = List[(Player, Square)]

  sealed trait Game
  case class NotStarted(players: List[Player]) extends Game
  case class Started(squares: Squares, current: Player) extends Game
  case class Finished(winner: Player) extends Game

  object Game {
    val init = NotStarted(List.empty[Player])
  }

  sealed trait Input
  case class AddPlayer(player: Player) extends Input
  case class Move(player: Player, roll: Option[Roll]) extends Input

}
