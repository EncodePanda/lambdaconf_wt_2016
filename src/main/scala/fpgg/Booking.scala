package ffgg

import scala.math.{Ordering => SOrdering}
import java.time.LocalDate

import scalaz._
import Scalaz._

object Domain {

  type NoPpl = Int
  type ReservationId = Int
  type Price = Double

  case class Period(from: LocalDate, to: LocalDate)
  case class Guest(firstName: String, lastName: String)
  case class Reservation(id: ReservationId, period: Period, guest: Guest)

  case class Room(
    no: String,
    floor: Int,
    view: Boolean,
    capacity: Int,
    price: Price,
    rating: Double,
    booked: List[Reservation])

  case class Booking(rooms: List[Room] = List.empty[Room])
}

object Functions {

  import Domain._

  implicit def roomOrdering: SOrdering[Room] = new SOrdering[Room] {
    def compare(r1: Room, r2: Room): Int = (r1.rating - r2.rating).toInt
  }.reverse

  val costPerPerson: Option[Room] => Option[Double] = {
    case maybeRoom => maybeRoom.map(room => room.price / room.capacity)
  }
  val pickAvailable: (Period, List[Room]) => List[Room] = {
    case(period, rooms) => rooms.filter { room =>
      !room.booked.map(_.period).contains(period)
    }
  }
  val filterWithView: List[Room] => List[Room] =
    (rooms: List[Room]) => rooms.filter(_.view)
  val filterCanAccomodate: (NoPpl, List[Room]) => List[Room] =
    (noPpl, rooms: List[Room]) => rooms.filter(_.capacity >= noPpl)
  val sortByRating: List[Room] => List[Room] =
    (rooms: List[Room]) => rooms.sorted

  val proposeBest: (Booking, Period, NoPpl) => Option[Room] = {
    case (Booking(rooms), period, noPpl) => {
      val pickForPeriod: List[Room] => List[Room] = pickAvailable.curried(period)
      val filterForNoPpl: List[Room] => List[Room] = filterCanAccomodate.curried(noPpl)

      val propose: List[Room] => Option[Room] =
        pickForPeriod >> filterWithView >> filterForNoPpl >> sortByRating >> {
          case r::tail => Some(r)
          case Nil => None
        }
      propose(rooms)
    }
  }

  val costPerPersonForBest: (Booking, Period, NoPpl) => Option[Double] =
    Function.untupled(proposeBest.tupled >>> costPerPerson)
}

object Sandbox extends App {

  import Functions._
  import Domain._

  val booking = Booking(List(
    Room("1", 0, view = true, capacity = 5, price = 100, rating = 3.2, booked = Nil),
    Room("2", 0, view = true, capacity = 3, price = 150, rating = 9.2, booked = Nil),
    Room("3", 0, view = false, capacity = 3, price = 120, rating = 8.4, booked = Nil),
    Room("4", 0, view = true, capacity = 4, price = 140, rating = 7.2, booked = Nil),
    Room("5", 0, view = true, capacity = 4, price = 140, rating = 4.6, booked = Nil)
  ))
  val period = Period(LocalDate.of(2017,1,8), LocalDate.of(2017,1,12))

  println(proposeBest(booking, period, 3))
  println(costPerPersonForBest(booking, period, 3))
}

