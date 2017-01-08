package ffgg

import language.higherKinds
import scala.math.{Ordering => SOrdering}
import java.time.LocalDate

import scalaz._, scalaz.concurrent.Task
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

  def costPerPerson[F[_]](implicit f: Functor[F]): F[Room] => F[Double] = {
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
    Function.untupled(proposeBest.tupled >>> costPerPerson[Option])
}

object Functions2 {

  import Domain._

  val isAffordable: (Room, Price) => Boolean =
    (r: Room, p: Price) => r.price <= p

  def affordableFor[F[_] : Functor](room: F[Room], price: Price): F[Boolean] = ???

  
}

object Sandbox extends App {

  import Functions._
  import Domain._

  type Error[A] = String \/ A
  def parse(json: String): Error[Room] = ???
  val cpp: Error[Double] = costPerPerson[Error].apply(parse("json goes here"))

  def fetchFromDB(no: String): Task[Room] = ???
  val cpp2: Task[Double] = costPerPerson[Task].apply(fetchFromDB("1c"))

}

