package ffgg

import scala.math.{Ordering => SOrdering}

import scalaz._
import Scalaz._

case class Room(
  no: String,
  floor: Int,
  view: Boolean,
  capacity: Int,
  price: Double,
  rating: Double,
  booked: Boolean)

case class Booking(rooms: List[Room])

object Functions {

  implicit def roomOrdering: SOrdering[Room] = new SOrdering[Room] {
    def compare(r1: Room, r2: Room): Int = (r1.rating - r2.rating).toInt
  }.reverse

  val costPerPerson: Room => Double = {
    case room => room.price / room.capacity
  }

  def filter(predicate: Room => Boolean): List[Room] => List[Room] = {
    case rooms => rooms.filter(predicate)
  }

  val pickAvailable: List[Room] => List[Room] = filter(!_.booked)

  val filterWithView: List[Room] => List[Room] = filter(_.view)

  val sortByRating: List[Room] => List[Room] =
    (rooms: List[Room]) => rooms.sorted

  val proposeBest: Booking => Room =
    ((b: Booking) => b.rooms) >>> pickAvailable >>> filterWithView >>> sortByRating >>> (rooms => rooms(0))

  val costPerPersonForBest: Booking => Double =
    proposeBest >>> costPerPerson
}

object Sandbox extends App {

  import Functions._

  val booking = Booking(List(
    Room("1", 0, view = true, capacity = 5, price = 100, rating = 3.2, booked = false),
    Room("2", 0, view = true, capacity = 3, price = 150, rating = 9.2, booked = true),
    Room("3", 0, view = false, capacity = 3, price = 120, rating = 8.4, booked = false),
    Room("4", 0, view = true, capacity = 4, price = 140, rating = 7.2, booked = false),
    Room("5", 0, view = true, capacity = 4, price = 140, rating = 4.6, booked = false)
  ))

  println(costPerPersonForBest(booking))
}
