package ffgg

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

  val pickAvailable: List[Room] => List[Room] = ???
  val filterWithView: List[Room] => List[Room] = ???
  val sortByRating: List[Room] => List[Room] = ???

  // available & with View & has best ranting
  val proposeBest: Booking => Room = ???

}
