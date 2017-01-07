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

}
