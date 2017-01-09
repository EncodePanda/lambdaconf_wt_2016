package ffgg

import scalaz._
import Scalaz._

object MonadTransformers {

  type Error[A] = String \/ A

  def calculate(input: String): Error[Option[Int]] = ???

  val plus1: Error[Option[Int]] = for {
    maybeValue <- calculate("some")
  } yield {
    for {
      v <- maybeValue
    } yield (v + 1)
  }


}

