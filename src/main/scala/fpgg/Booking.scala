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

  trait Event
  case class RoomFetched(no: String) extends Event
  case class RoomAdded(no: String) extends Event
  case class ReservationMade(id: ReservationId) extends Event

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

  case class Booking(
    var rooms: List[Room] = List.empty[Room],
    var events: List[Event] = List.empty[Event]
  )
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
    case (Booking(rooms, _), period, noPpl) => {
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
  import Functions._

  val isAffordable: (Room, Price) => Boolean =
    (r: Room, p: Price) => r.price <= p

  def affordableFor[F[_] : Applicative](room: F[Room], price: Price): F[Boolean] = 
    (room |@| price.point[F])(isAffordable)

  def bestFor[F[_]: Bind](
    booking: F[Booking],
    fetchPeriod: Booking => F[Period],
    fetchNoPpl: Booking => F[NoPpl]
  ): F[Option[Room]] = for {
    b <- booking
    p <- fetchPeriod(b)
    n <- fetchNoPpl(b)
  } yield proposeBest(b, p, n)
  
}

object DealingWithChangingState {

  import Domain._

  class RoomGenerator {
    def generateRoom(
      no: String,
      floor: Int,
      view: Boolean,
      capacity: Int
    ): Room =
      Room(no, floor, view, capacity, capacity * 100, 10.0, booked = List.empty[Reservation])
  }

  /*
   * Corrupted approach
   */
  class BookingService(booking: Booking, roomGenerator: RoomGenerator) {

    def addRoom(
      no: String,
      floor: Int,
      view: Boolean,
      capacity: Int
    ): Room = {
      val room = roomGenerator.generateRoom(no, floor, view, capacity)
      booking.events = RoomAdded(no) :: booking.events
      booking.rooms = room :: booking.rooms
      room
    }

    def currentReservationId: ReservationId =
      booking.rooms.flatMap(_.booked.map(_.id)).foldLeft(0)(Math.max)

    def fetchRoom(no: String): Option[Room] = {
      booking.events = RoomFetched(no) :: booking.events
      booking.rooms.filter(_.no == no).headOption
    }

    def book(
      room: Room,
      period: Period,
      guest: Guest,
      reservationId: ReservationId): Unit = {
      booking.events = ReservationMade(reservationId) :: booking.events
      val reservation = Reservation(reservationId, period, guest)
      val updatedRoom = room.copy(booked = reservation :: room.booked)
      booking.rooms = updatedRoom :: booking.rooms.filter(_ != room)
    }

    def bookVip(
      no: String,
      floor: Int,
      view: Boolean,
      capacity: Int,
      period: Period
    )(guest: Guest): ReservationId = {
      val maybeRoom: Option[Room] = fetchRoom(no)
      val room: Room = maybeRoom.getOrElse(addRoom(no, floor, view, capacity))
      val reservationId = currentReservationId + 1
      book(room, period, guest, reservationId)
      reservationId
    }
  }
}
