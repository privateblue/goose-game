package goose

import game._

import org.scalatest._
import org.scalatest.OptionValues._

import scala.util.Random
import scala.language.postfixOps

class GameSpec extends FlatSpec with Matchers {

  implicit val config = Config(
    playerCount = 2,
    winAt = 63,
    bridgeAt = (6, 12),
    goosesAt = List(5, 9, 14, 18, 23, 27),
    withPrank = true
  )

  val ps = 1 to config.playerCount map (n => s"player$n") toList
  val started = Started(ps.map(_ -> 0), "player1")

  "Adding a player" should "add the player and wait for another addition" in {
    val (game, message) = play(Game.init, AddPlayer("Foo"))
    game shouldBe NotStarted(List("Foo"))
    message shouldBe "players: Foo"
  }

  "Adding playerCount players" should "start the game" in {
    val game = ps.foldLeft(Game.init: Game) {
      case (g, p) => play(g, AddPlayer(p))._1
    }
    game shouldBe started
  }

  "Moving before the game started" should "give an error message" in {
    val (game, message) = play(Game.init, Move("Foo", None))
    game shouldBe Game.init
    message shouldBe "Please add more users"
  }

  "Adding the same player again" should "give an error message" in {
    val (game1, _) = play(Game.init, AddPlayer("Foo"))
    val (game2, message) = play(game1, AddPlayer("Foo"))
    game2 shouldBe game1
    message shouldBe "Foo: already existing player"
  }

  "Moving with a player" should "advance them along the board" in {
    val (game, message) = play(started, Move("player1", Some(Roll(3, 1))))
    game shouldBe a[Started]
    game.asInstanceOf[Started].squares.find(_._1 == "player1") shouldBe
      Some(("player1" -> 4))
    message shouldBe "player1 rolls 3, 1. player1 moves from 0 to 4"
  }

  it should "automatically roll a dice if no rolls provided for the player" in {
    // we make sure no goose is hit, so these features don't interfere here
    val source = config.goosesAt.lastOption.getOrElse(0) + 1
    val game1 = send("player1", started, source)
    val (game2, _) = play(game1, Move("player1", None))
    game2 shouldBe a[Started]
    positionOf("player1", game2).value should be > (source)
  }

  it should "advance who's next" in {
    if (config.playerCount > 1) {
      var n = 1
      val (game1, _) = play(started, Move(s"player$n", Some(Roll(1, 1))))
      game1 shouldBe a[Started]
      game1.asInstanceOf[Started].current shouldBe ps(n % config.playerCount)

      n = 2
      val (game2, _) = play(game1, Move(s"player$n", Some(Roll(1, 1))))
      game2 shouldBe a[Started]
      game2.asInstanceOf[Started].current shouldBe ps(n % config.playerCount)
    }
  }

  "Moving with a player other than the next" should "fail" in {
    if (config.playerCount > 1) {
      val (game, message) = play(started, Move("player2", None))
      game shouldBe started
      message shouldBe "It's player1's turn"
    }
  }

  "Moving with an unknown player" should "give a meaningful error message" in {
    val (game, message) = play(started, Move("Foo", None))
    game shouldBe started
    message shouldBe "I don't know Foo"
  }

  "Reaching the winning square" should "finish the game" in {
    val source = config.winAt - 5
    val target = config.winAt
    val game1 = send("player1", started, source)
    val (game2, message) = play(game1, Move("player1", Some(Roll(2, 3))))
    game2 shouldBe a[Finished]
    game2.asInstanceOf[Finished].winner shouldBe "player1"
    message shouldBe s"player1 rolls 2, 3. player1 moves from $source to $target. player1 Wins!!"
  }

  "Overshooting the winning square" should "bounce the player back" in {
    val source = config.winAt - 5
    val target = config.winAt
    val result = config.winAt - 7
    val game1 = send("player1", started, source)
    val (game2, message) = play(game1, Move("player1", Some(Roll(6, 6))))
    game2 shouldBe a[Started]
    positionOf("player1", game2) shouldBe Some(result)
    message shouldBe s"player1 rolls 6, 6. player1 moves from $source to $target. player1 bounces! player1 returns to $result"
  }

  "Reaching The Bridge" should "jump the player" in {
    val source = config.bridgeAt._1 - 3
    val target = config.bridgeAt._1
    val jump = config.bridgeAt._2
    val game1 = send("player1", started, source)
    val (game2, message) = play(game1, Move("player1", Some(Roll(1, 2))))
    game2 shouldBe a[Started]
    positionOf("player1", game2) shouldBe Some(jump)
    message shouldBe s"player1 rolls 1, 2. player1 moves from $source to The Bridge. player1 jumps to $jump"
  }

  "Reaching The Goose" should "move the player again with the same amount" in {
    if (config.goosesAt.size > 0) {
      val source = config.goosesAt.head - 2
      val target = config.goosesAt.head
      val jump = config.goosesAt.head + 2
      val game1 = send("player1", started, source)
      val (game2, message) = play(game1, Move("player1", Some(Roll(1, 1))))
      game2 shouldBe a[Started]
      positionOf("player1", game2) shouldBe Some(jump)
      message shouldBe s"player1 rolls 1, 1. player1 moves from $source to $target, The Goose. player1 moves again and goes to $jump"
    }
  }

  it should "move the player yet again, even if already coming from The Goose" in {
    if (config.goosesAt.size > 1) {
      val step = config.goosesAt(1) - config.goosesAt(0)
      val d1 = Random.nextInt(step - 1) + 1
      val d2 = step - d1
      val source = config.goosesAt(0) - step
      val target = config.goosesAt(0)
      val jump1 = config.goosesAt(1)
      val jump2 = config.goosesAt(1) + step
      val game1 = send("player1", started, source)
      val (game2, message) = play(game1, Move("player1", Some(Roll(d1, d2))))
      game2 shouldBe a[Started]
      positionOf("player1", game2) shouldBe Some(jump2)
      message shouldBe s"player1 rolls $d1, $d2. player1 moves from $source to $target, The Goose. player1 moves again and goes to $jump1, The Goose. player1 moves again and goes to $jump2"
    }
  }

  "Prank turned on" should "send back other players on the target square to where the current player arrived from" in {
    if (config.playerCount > 1) {
      // we make sure no goose is hit, so these features don't interfere here
      val source = config.goosesAt.lastOption.getOrElse(0) + 1
      val target = source + 2
      val game1 = send("player2", started, target)
      val game2 = send("player1", game1, source)

      val config1 = config.copy(withPrank = false)
      val (game3, message1) =
        play(game2, Move("player1", Some(Roll(1, 1))))(config1)
      game3 shouldBe a[Started]
      positionOf("player1", game3) shouldBe Some(target)
      positionOf("player2", game3) shouldBe Some(target)
      message1 shouldBe s"player1 rolls 1, 1. player1 moves from $source to $target"

      val config2 = config.copy(withPrank = true)
      val (game4, message2) =
        play(game2, Move("player1", Some(Roll(1, 1))))(config2)
      game4 shouldBe a[Started]
      positionOf("player1", game4) shouldBe Some(target)
      positionOf("player2", game4) shouldBe Some(source)
      message2 shouldBe s"player1 rolls 1, 1. player1 moves from $source to $target. On $target there is player2, who returned to $source"
    }
  }

  // TODO test if it works in combination with The Goose

  def send(player: Player, game: Started, target: Square): Started =
    game.copy(
      squares = game.squares
        .filterNot(_._1 == player) :+ (player -> target)
    )

  def positionOf(player: Player, game: Game): Option[Square] =
    game
      .asInstanceOf[Started]
      .squares
      .find(_._1 == player)
      .map(_._2)

}
