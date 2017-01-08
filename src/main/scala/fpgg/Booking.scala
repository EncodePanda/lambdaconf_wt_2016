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
  type GuestId = Int

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

    def addRoom[F[_] : Monad](no: String,
      floor: Int,
      view: Boolean,
      capacity: Int
    ): StateT[F, Booking, Room] = StateT[F, Booking, Room]({
      case booking: Booking => {
        val room = RoomGenerator.generateRoom(no, floor, view, capacity)
        val newBooking = booking.copy(
          rooms = room :: booking.rooms,
          events = RoomAdded(no) :: booking.events
        )
        ((newBooking, room)).point[F]
      }
    })

    def currentReservationId[F[_] : Monad]: StateT[F, Booking, ReservationId] =
      StateT[F, Booking, ReservationId]({
        case booking =>
          ((booking, booking.rooms.flatMap(_.booked.map(_.id)).foldLeft(0)(Math.max))).point[F]
      })

    def fetchRoom[F[_] : Monad](no: String): StateT[F, Booking, Option[Room]] =
      StateT[F, Booking, Option[Room]]({
        case booking => {
          val newBooking = booking.copy(
            events = RoomFetched(no) :: booking.events
          )
          val fetched = booking.rooms.filter(_.no == no).headOption
            ((newBooking, fetched)).point[F]
        }
      })

    def book[F[_] : Monad](
      room: Room, period: Period, guest: Guest, reservationId: ReservationId
    ): StateT[F, Booking, Unit] = StateT[F, Booking, Unit]({
      case booking => {
        val reservation = Reservation(reservationId, period, guest)
        val updatedRoom = room.copy(booked = reservation :: room.booked)

        val newBooking = booking.copy (
          events = ReservationMade(reservationId) :: booking.events,
          rooms = updatedRoom :: booking.rooms.filter(_ != room)
        )
          ((newBooking, ())).point[F]
      }
    })

    def bookVip[F[_] : Monad](
      no: String,
      floor: Int,
      view: Boolean,
      capacity: Int,
      period: Period
    )(guest: Guest): StateT[F, Booking, ReservationId] = for {
      maybeRoom <- fetchRoom[F](no)
      room <- maybeRoom match {
        case Some(r) => r.point[StateT[F, Booking, ?]]
        case None => addRoom[F](no, floor, view, capacity)
      }
      currentId <- currentReservationId[F]
      reservationId = currentId + 1
      _ <- book[F](room, period, guest, reservationId)

    } yield reservationId

  }
}

object IOOperations {
  import Domain._

  object InMemoryDB {
    var booking: Booking = new Booking()
    var guests: Map[GuestId, Guest] = Map(1 -> Guest("Pawel", "Szulc"))
  }

  val fetchBooking: () => Task[Booking] = () => Task.delay {
    InMemoryDB.booking
  }
  val updateBooking: Booking => Task[Unit] = (booking: Booking) => Task.delay {
    InMemoryDB.booking = booking
  }
  val findGuest: GuestId => Task[Guest] = (id: GuestId) => Task.delay {
    InMemoryDB.guests(id)
  }
}

object Sandbox extends App {

  import Domain._
  import DealingWithChangingState.BookingService._
  import IOOperations._

  val period = Period(LocalDate.of(2017, 1, 8), LocalDate.of(2017, 1, 12))

  val recipe: StateT[Task, Booking, List[ReservationId]] = for {
    guest <- findGuest(1).liftM[StateT[?[_], Booking, ?]]
    resId1 <- bookVip[Task]("101", floor = 1, view = true, capacity = 5, period)(guest)
    resId2 <- bookVip[Task]("102", floor = 1, view = true, capacity = 5, period)(guest)
  } yield List(resId1, resId2)

  val app: Task[List[ReservationId]] = for {
    booking <- fetchBooking()
    result <- recipe.run(booking)
    (modifiedBooking, reservations) = result
    _ <- updateBooking(modifiedBooking)
  } yield reservations

  val reservations: List[ReservationId] = app.unsafePerformSync
  println(s"reservation ids: $reservations")

}
