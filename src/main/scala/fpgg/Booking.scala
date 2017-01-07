package ffgg

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

  val costPerPerson: Room => Double = {
    case room => room.price / room.capacity
  }

  def filter(predicate: Room => Boolean): List[Room] => List[Room] = {
    case rooms => rooms.filter(predicate)
  }

  val pickAvailable: List[Room] => List[Room] = filter(!_.booked)

  val filterWithView: List[Room] => List[Room] = filter(_.view)

  val sortByRating: List[Room] => List[Room] = ???


  val proposeBest: Booking => Room =
    ((b: Booking) => b.rooms) >>> pickAvailable >>> filterWithView >>> sortByRating >>> (rooms => rooms(0))


}
