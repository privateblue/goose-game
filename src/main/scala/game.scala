package goose

import scala.util.Random

object game {
  def play(game: Game, input: Input)(implicit config: Config): (Game, Message) =
    (game, input) match {

      // adding players
      case (NotStarted(players), AddPlayer(player)) =>
        if (players.contains(player))
          (game, s"$player: already existing player")
        else {
          val nps = players :+ player
          val plist = nps.mkString(", ")
          if (nps.size == config.playerCount) {
            val squares = nps.map(_ -> 0)
            (Started(squares, nps.head), s"players: $plist. Game started")
          } else (NotStarted(nps), s"players: $plist")
        }

      // invalid move before started
      case (NotStarted(_), _) =>
        (game, "Please add more users")

      // non-existent player
      case (Started(squares, _), Move(p, _))
          if !squares.map(_._1).contains(p) =>
        (game, s"I don't know $p")

      // wrong player
      case (Started(_, current), Move(p, _)) if current != p =>
        (game, s"It's $current's turn")

      // game move
      case (Started(squares, current), Move(p, roll)) if current == p =>
        move(squares, current, roll)

      // invalid move during game
      case (Started(_, _), _) =>
        (game, "The game has started, please make a move")

      case _ =>
        (game, "")
    }

  def move(
      squares: Squares,
      current: Player,
      roll: Option[Roll]
  )(implicit config: Config): (Game, Message) = {
    val r = roll.getOrElse(autoRoll)
    val v = r.dice1 + r.dice2
    val s = squareOf(current, squares)
    val ns = s + v
    ns match {
      // win
      case ns if ns == config.winAt =>
        (
          Finished(current),
          s"$current rolls ${r.dice1}, ${r.dice2}. $current moves from $s to $ns. $current Wins!!"
        )
      // bounce
      case ns if ns > config.winAt =>
        val bs = 2 * config.winAt - ns
        val nsquares = set(current, bs, squares)
        val c = nextPlayer(current, squares.map(_._1))
        (
          Started(nsquares, c),
          s"$current rolls ${r.dice1}, ${r.dice2}. $current moves from $s to ${config.winAt}. $current bounces! $current returns to $bs"
        )
      // bridge
      case ns if ns == config.bridgeAt._1 =>
        val nsquares = set(current, config.bridgeAt._2, squares)
        val c = nextPlayer(current, squares.map(_._1))
        (
          Started(nsquares, c),
          s"$current rolls ${r.dice1}, ${r.dice2}. $current moves from $s to The Bridge. $current jumps to ${config.bridgeAt._2}"
        )
      // goose
      case ns if config.goosesAt.contains(ns) =>
        val path = goose(ns, v)
        val nsquares = set(current, path.last, squares)
        // TODO make it work with prank
        val c = nextPlayer(current, squares.map(_._1))
        val gooseText =
          path.mkString(s", The Goose. $current moves again and goes to ")
        (
          Started(nsquares, c),
          s"$current rolls ${r.dice1}, ${r.dice2}. $current moves from $s to $gooseText"
        )
      // regular move
      case _ =>
        val nsquares = set(current, ns, squares)
        val (pnsquares, prankText) = prankIfNeeded(current, nsquares, s, ns)
        val c = nextPlayer(current, squares.map(_._1))
        (
          Started(pnsquares, c),
          s"$current rolls ${r.dice1}, ${r.dice2}. $current moves from $s to $ns$prankText"
        )
    }
  }

  def autoRoll: Roll = Roll(Random.nextInt(6) + 1, Random.nextInt(6) + 1)

  def nextPlayer(player: Player, players: List[Player]): Player = {
    val rest = players.dropWhile(_ != player).tail
    rest.headOption.getOrElse(players.head)
  }

  def squareOf(player: Player, squares: Squares): Square =
    squares
      .find(_._1 == player)
      .getOrElse(throw new Exception(s"No $player user found"))
      ._2

  def set(
      player: Player,
      square: Square,
      squares: Squares
  ): Squares =
    squares.filterNot(_._1 == player) :+ (player -> square)

  def goose(from: Square, step: Int)(implicit config: Config): List[Square] =
    if (config.goosesAt.contains(from + step))
      List(from) ++ goose(from + step, step)
    else List(from, from + step)

  def prankIfNeeded(
      player: Player,
      squares: Squares,
      source: Square,
      target: Square
  )(implicit config: Config): (Squares, String) =
    if (!config.withPrank) (squares, "")
    else {
      val ps = squares.filter(s => s._2 == target && s._1 != player).map(_._1)
      val pnames = ps.mkString(" and ")
      val pnsquares = ps.foldLeft(squares) {
        case (nsquares, p) => set(p, source, nsquares)
      }
      val text =
        if (ps.isEmpty) ""
        else s". On $target there is $pnames, who returned to $source"
      (pnsquares, text)
    }
}
