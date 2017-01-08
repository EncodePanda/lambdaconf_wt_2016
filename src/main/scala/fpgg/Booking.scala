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
    rooms: List[Room] = List.empty[Room],
    events: List[Event] = List.empty[Event]
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

  object RoomGenerator {
    def generateRoom(
      no: String,
      floor: Int,
      view: Boolean,
      capacity: Int
    ): Room =
      Room(no, floor, view, capacity, capacity * 100, 10.0, booked = List.empty[Reservation])
  }

  object BookingService {

    def addRoom(booking: Booking)(no: String,
      floor: Int,
      view: Boolean,
      capacity: Int
    ): (Booking, Room) = {
      val room = RoomGenerator.generateRoom(no, floor, view, capacity)
      val newBooking = booking.copy(
        rooms = room :: booking.rooms,
        events = RoomAdded(no) :: booking.events
      )
      (newBooking, room)
    }

    def currentReservationId(booking: Booking): ReservationId =
      booking.rooms.flatMap(_.booked.map(_.id)).foldLeft(0)(Math.max)

    def fetchRoom(booking: Booking)(no: String): (Booking, Option[Room]) = {
      val newBooking = booking.copy(
        events = RoomFetched(no) :: booking.events
      )
      val fetched = booking.rooms.filter(_.no == no).headOption
      (newBooking, fetched)
    }

    def book(booking: Booking)(
      room: Room,
      period: Period,
      guest: Guest,
      reservationId: ReservationId
    ): (Booking, Unit) = {
      val reservation = Reservation(reservationId, period, guest)
      val updatedRoom = room.copy(booked = reservation :: room.booked)

      val newBooking = booking.copy (
        events = ReservationMade(reservationId) :: booking.events,
        rooms = updatedRoom :: booking.rooms.filter(_ != room)
      )
      (newBooking, ())
    }

    def bookVip(booking: Booking)(
      no: String,
      floor: Int,
      view: Boolean,
      capacity: Int,
      period: Period
    )(guest: Guest): (Booking, ReservationId) = {
      val (nb1, maybeRoom) = fetchRoom(booking)(no)
      val (nb2, room) = maybeRoom match {
        case Some(r) => (nb1, r)
        case None => addRoom(nb1)(no, floor, view, capacity)
      }
      val reservationId = currentReservationId(nb2) + 1
      val (nb3, _) = book(nb2)(room, period, guest, reservationId)
      (nb3, reservationId)
    }
  }
}
