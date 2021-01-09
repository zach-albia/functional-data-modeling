package fdm

import java.net.URL
import java.time.Instant
import java.util.Currency

import scala.annotation.tailrec
import scala.util.Random

/**
 * The following exercises test your ability to model various entities using case classes.
 */
object product_modeling {

  /**
   * EXERCISE 1
   *
   * Using a case class, create a model of a product, which has a name, description, and a price.
   *
   */
  final case class Product(name: String, description: String, price: Currency)

  /**
   * EXERCISE 2
   *
   * Using a case class, create a model of a a user profile, which has a picture URL, and text-
   * based location (indicating the geographic area where the user is from).
   */
  final case class UserProfile(pictureUrl: URL, location: String)

  /**
   * EXERCISE 3
   *
   * Using a case class, create a model of an item that can be posted on LinkedIn's feed. This
   * item contains a subject and some text.
   */
  final case class FeedItem(subject: String, content: String)

  /**
   * EXERCISE 4
   *
   * Using a case class, create a model of an event, which has an event id, a timestamp, and a
   * map of properties (String/String).
   */
  final case class Event(id: String, timestamp: Instant, properties: Map[String, String])
}

/**
 * The following exercises test your ability to model various entities using enums.
 */
object sum_modeling {

  /**
   * EXERCISE 1
   *
   * Using an enum, create a model of a color, which could be `Red`, `Green`, `Blue`, or `Custom`,
   * and if `Custom`, then it should store `red`, `green`, and `blue` components individually, as
   * an integer (`Int`) value.
   */
  sealed trait Color
  object Color {
    case object Red                                 extends Color
    case object Blue                                extends Color
    case object Green                               extends Color
    case class Custom(r: Short, g: Short, b: Short) extends Color
  }

  /**
   * EXERCISE 2
   *
   * Using an enum, create a model of an web event, which could be either a page load for a certain
   * URL, a click on a particular button, or a click to a specific URL.
   */
  sealed trait WebEvent
  object WebEvent {
    final case class PageLoad(url: String) extends WebEvent
    final case object ButtonClick          extends WebEvent
    final case class UrlClick(url: String) extends WebEvent
  }

  /**
   * EXERCISE 3
   *
   * Using an enum, create a model of an age bracket, which could be baby, child, young adult,
   * teenager, adult, mature adult, or senior adult.
   */
  sealed trait AgeBracket
  object AgeBracket {
    case object Baby        extends AgeBracket
    case object Child       extends AgeBracket
    case object Teenager    extends AgeBracket
    case object YoungAdult  extends AgeBracket
    case object Adult       extends AgeBracket
    case object MatureAdult extends AgeBracket
    case object SeniorAdult extends AgeBracket
  }

  /**
   * EXERCISE 4
   *
   * Using an enum, create a model of a step in a JSON pipeline, which could be transform,
   * aggregate, or save to file.
   * aggregate.
   */
  type Json
  sealed trait JsonPipelineStep
  object JsonPipeline {
    final case class Transform(fn: Json => Json)           extends JsonPipelineStep
    final case class Aggregate(fn: Iterable[Json] => Json) extends JsonPipelineStep
    final case class SaveToFile(fn: Json => Unit)          extends JsonPipelineStep
  }
}

/**
 * The following exercises test your ability to model various entities using both case classes and
 * enums.
 */
object mixed_modeling {

  /**
   * EXERCISE 1
   *
   * Using only case classes and enums, create a model of an order for an e-commerce platform, which
   * would consist of a number of items, each with a certain price, and an overall price, including
   * shipping and handling charges.
   */
  final case class Order(items: List[OrderItem], shippingCharges: Currency, handlingCharges: Currency)
  final case class OrderItem(name: String, price: Currency)

  /**
   * EXERCISE 2
   *
   * Using only case classes and enums, create a model of an `Email`, which contains a subject,
   * a body, a recipient, and a from address.
   */
  final case class Email(subject: String, body: String, recipient: String, from: String)

  /**
   * EXERCISE 3
   *
   * Using only case classes and enums, create a model of a page layout for a content-management
   * system, which could consist of predefined elements, such as a news feed, a photo gallery,
   * and other elements, arranged in some well-defined way relative to each other.
   */
  sealed trait PageLayout
  object PageLayout {
    final case class NewsFeed(news: List[String])                extends PageLayout
    final case class PhotoGallery(photos: List[String])          extends PageLayout
    final case class CustomElement(source: Any)                  extends PageLayout
    final case class ComposedElement(elements: List[PageLayout]) extends PageLayout
  }

  /**
   * EXERCISE 4
   *
   * Using only case classes and enums, create a model of a rule that describes the conditions for
   * triggering an email to be sent to a shopper on an e-commerce website.
   */
  sealed trait EmailTriggerRule
  object EmailTriggerRule {
    final case object Shopper
    final case object EcommerceWebsite
  }
}

object basic_dm_graduation {
  sealed trait Command
  object Command {
    case object Look                      extends Command
    case object Quit                      extends Command
    final case class LookAt(what: String) extends Command
    final case class Go(where: String)    extends Command
    final case class Take(item: String)   extends Command
    final case class Drop(item: String)   extends Command
    final case class Fight(who: String)   extends Command

    def fromString(string: String): Option[Command] =
      string.trim.toLowerCase.split("\\s+").toList match {
        case "go" :: where :: Nil          => Some(Go(where))
        case "look" :: Nil                 => Some(Look)
        case "look" :: "at" :: what :: Nil => Some(LookAt(what))
        case "take" :: item :: Nil         => Some(Take(item))
        case "drop" :: item :: Nil         => Some(Drop(item))
        case "fight" :: who :: Nil         => Some(Fight(who))
        case ("quit" | "exit") :: Nil      => Some(Quit)
        case _                             => None
      }
  }

  /**
   * EXERCISE
   *
   * Using case classes and sealed traits (and whatever other data types you like), design a game
   * world that can be used to play a simple text-based role playing game.
   *
   * The data type should model the player, non-player characters, and items available to pick up
   * or drop in the game world.
   */
  sealed abstract class Direction(val rOffset: Int, val cOffset: Int)
  object Direction {
    case object North extends Direction(-1, 0)
    case object South extends Direction(1, 0)
    case object East  extends Direction(0, 1)
    case object West  extends Direction(0, -1)

    def fromInt(i: Int): Option[Direction] = i match {
      case 0 => Some(Direction.North)
      case 1 => Some(Direction.East)
      case 2 => Some(Direction.South)
      case 3 => Some(Direction.West)
      case _ => None
    }

    def fromString(s: String): Option[Direction] = s.toLowerCase match {
      case "north" => Some(North)
      case "south" => Some(South)
      case "east"  => Some(East)
      case "west"  => Some(West)
      case _       => None
    }
  }

  final case class Player(
    location: (Int, Int),
    direction: Direction,
    inventory: Map[String, Int],
    bestiary: Set[String]
  ) {
    def addItem(name: String, quantity: Int): Player =
      copy(inventory = inventory.updatedWith(name) {
        case Some(n) => Some(n + quantity)
        case None    => Some(quantity)
      })

    def tileLocation(direction: Direction): (Int, Int) =
      (location._1 + direction.rOffset, location._2 + direction.cOffset)

    def fight(name: String, drop: Option[Cell.Item]): Player =
      copy()
  }

  sealed trait Cell
  object Cell {
    final case object Empty extends Cell

    final case object Player extends Cell

    final case class Mob(name: String, drop: Option[Item]) extends Cell
    object Mob { val names = Array("Ahriman", "Flan", "Behemoth", "Cactuar", "Tonberry", "Grendel", "Adamantoise") }

    final case class Item(name: String, quantity: Int) extends Cell
    object Item { val names = Array("Potion", "Phoenix Down", "Ether", "Remedy", "Elixir") }
  }

  final case class State private (player: Player, cells: Vector[Vector[Cell]]) {
    lazy val lookAtPlayerDirection: (Option[Cell], State) = lookAt(player.direction)

    def lookAt(direction: Direction): (Option[Cell], State) =
      (lookTargetCell(direction), copy(player = player.copy(direction = direction)))

    def movePlayer(direction: Direction): Either[String, State] = {
      val (r, c)       = player.location
      val (newR, newC) = player.tileLocation(direction)
      cells
        .lift(newR)
        .flatMap(_.lift(newC))
        .fold[Either[String, State]](Left(s"You're at the end of the world. 🗺🔚"))(_ match {
          case Cell.Empty => // player can only move to an empty cell
            val emptyPlayerCell = cells.updated(r, cells(r).updated(c, Cell.Empty))
            Right(
              State(
                player.copy(location = (newR, newC)),
                emptyPlayerCell.updated(newR, emptyPlayerCell(newR).updated(newC, Cell.Player))
              )
            )
          case Cell.Mob(name, drop)      => Left("There is a monster in the way! 🐱‍🐉")
          case Cell.Item(name, quantity) => Left("There is an item in the way! 🎁")
          case Cell.Player               => Left("You have a doppelganger! 😱")
        })
    }

    def take(item: String): Either[String, (Int, State)] =
      (lookTargetCell(), item) match {
        case (_, item) if !Cell.Item.names.contains(item) =>
          Left("No such item")
        case (Some(Cell.Item(name, quantity)), item) if name == item =>
          Right((quantity, replaceCell(lookTargetLocation, Cell.Empty).copy(player = player.addItem(name, quantity))))
        case (Some(_), _) =>
          Left(s"""The cell you're looking at doesn't contain "$item"""")
        case _ =>
          Left("You can't take items from outside the world!")
      }

    def drop(itemName: String): Either[String, State] =
      (lookTargetCell(), player.inventory.get(itemName)) match {
        case (_, _) if !Cell.Item.names.contains(itemName) =>
          Left("No such item")
        case (None, _) =>
          Left("You can't drop items outside the world!")
        case (_, None) =>
          Left("There's nothing to drop!")
        case (Some(Cell.Empty), Some(quantity)) =>
          Right(replaceCell(lookTargetLocation, Cell.Item(itemName, quantity)))
      }

    def fight(mob: String): Either[String, State] =
      if (Cell.Mob.names.contains(mob)) {
        lookTargetCell() match {
          case Some(Cell.Mob(mobName, mobDrop)) =>
            Right(replaceCell(lookTargetLocation, Cell.Empty).copy(player.fight(mobName, mobDrop)))
          case _ =>
            Left(s"There's no $mob to fight in front of you.")
        }
      } else Left("No such monster.")

    private def lookTargetCell(direction: Direction = player.direction) =
      cellAt(lookTargetLocation)

    private lazy val lookTargetLocation = player.location match {
      case (r, c) => (r + player.direction.rOffset, c + player.direction.cOffset)
    }

    private def replaceCell(location: (Int, Int), cell: Cell): State = location match {
      case (r, c) => copy(cells = cells.updated(r, cells(r).updated(c, cell)))
    }

    private def cellAt(location: (Int, Int)) =
      cells.lift(location._1).flatMap(_.lift(location._2))
  }

  object State {
    val worldSize = (90, 160)

    def apply(): State = {
      val random         = new Random
      val randomLocation = (random.nextInt(worldSize._1), random.nextInt(worldSize._2))
      val player         = Player(randomLocation, randomDirection(), Map.empty[String, Int], Set.empty[String])
      val state = new State(
        player,
        (0 until worldSize._1)
          .map(r => (0 until worldSize._2).map(c => randomCell()).toVector)
          .toVector
      )
      // insert player
      state.copy(
        cells = state.cells.updated(
          randomLocation._1,
          state.cells(randomLocation._2).updated(randomLocation._2, Cell.Player)
        )
      )
    }

    private def randomCell(): Cell = {
      val random = new Random
      random.nextInt(2) match {
        case 0 => Cell.Empty
        case 1 => randomItem()
        case 2 =>
          Cell.Mob(
            Cell.Mob.names(random.nextInt(Cell.Mob.names.size)),
            Option.when(random.nextBoolean())(randomItem())
          )
      }
    }

    private def randomItem() = {
      val random = new Random
      Cell.Item(Cell.Item.names(random.nextInt(Cell.Item.names.size)), random.nextInt(3))
    }

    private def randomDirection() = Direction.fromInt((new Random).nextInt(4)).get
  }

  def describe(state: State): String = {
    val player          = state.player
    val inventoryString = player.inventory.map { case (name, n) => s"$name x$n" }.mkString("\n")
    s"""|You are at ${player.location}, facing ${player.direction.toString.toLowerCase}.
        |
        |You have the following items in your inventory:
        |${if (inventoryString.isEmpty) "EMPTY" else inventoryString}
        |
        |Monsters you have fought so far:
        |${player.bestiary.mkString("\n")}
        |""".stripMargin
  }

  def process(state: State, command: Command): (String, Option[State]) = {
    val lookedAtNothing  = ("You're staring into the void.", Some(state))
    def look(cell: Cell) = s"You looked at $cell"

    command match {
      case Command.Quit =>
        ("You quitted", None)
      case Command.Look =>
        val (cell, newState) = state.lookAtPlayerDirection
        (cell.fold(lookedAtNothing)(cell => (look(cell), Some(newState))))
      case Command.LookAt(where) =>
        Direction
          .fromString(where)
          .map(state.lookAt)
          .map { case ((cell, newState)) => (cell.fold(lookedAtNothing)(cell => (look(cell), Some(newState)))) }
          .getOrElse((s"'$where' is not a valid direction.", Some(state)))
      case Command.Go(where) =>
        Direction
          .fromString(where)
          .map(state.movePlayer)
          .getOrElse(Left(s"'$where' is not a valid direction."))
          .fold((_, Some(state)), newState => (s"Player moved to ${newState.player.location}", Some(newState)))
      case Command.Take(item) =>
        state
          .take(item)
          .fold(
            e => (e, Some(state)),
            ((n: Int, newState: State) => (s"""You have taken "$item"""", Some(newState))).tupled
          )
      case Command.Drop(item) =>
        state
          .drop(item)
          .fold(
            e => (e, Some(state)),
            newState => (s"""You have dropped all of your "$item".""", Some(newState))
          )
      case Command.Fight(mob) =>
        state
          .fight(mob)
          .fold(
            e => (e, Some(state)),
            newState => (s"You fought and defeated the $mob.", Some(newState))
          )
    }
  }

  def main(args: Array[String]): Unit = {
    @tailrec
    def loop(state: State): Unit = {
      println(describe(state))

      val line = scala.io.StdIn.readLine()

      Command.fromString(line) match {
        case None =>
          println("Unrecognized command")
          loop(state)

        case Some(command) =>
          process(state, command) match {
            case (output, next) =>
              println(output)
              next match {
                case Some(value) => loop(value)
                case None        => println("Goodbye!")
              }
          }
      }
    }

    loop(State())
  }
}
