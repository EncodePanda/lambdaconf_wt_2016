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

object QuickSort {

  private def lessOrEqual(room: Room, l: List[Room]) =
    l.filter(r => r.rating <= room.rating)

  private def greater(room: Room, l: List[Room]) =
    l.filter(r => r.rating > room.rating)

  def sort(rooms: List[Room]): List[Room] = rooms match {
    case Nil => Nil
    case r :: tail =>
      sort(lessOrEqual(r, tail)) ++ List(r) ++ sort(greater(r, tail))
  }
}

object Functions {

  val costPerPerson: Room => Double = {
    case room => room.price / room.capacity
  }

  def filter(predicate: Room => Boolean): List[Room] => List[Room] = {
    case rooms => rooms.filter(predicate)
  }

  val pickAvailable: List[Room] => List[Room] = filter(!_.booked)

  val filterWithView: List[Room] => List[Room] = filter(_.view)

  val sortByRating: List[Room] => List[Room] =
    (rooms: List[Room]) => QuickSort.sort(rooms)

  val proposeBest: Booking => Room =
    ((b: Booking) => b.rooms) >>> pickAvailable >>> filterWithView >>> sortByRating >>> (rooms => rooms(0))

}
