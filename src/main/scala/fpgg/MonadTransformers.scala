package ffgg

import scalaz._
import Scalaz._

object MonadTransformers {

  def calculate(input: String): Option[Int] = ???

  val plus1: Option[Int] = for {
    v <- calculate("some")
  } yield (v + 1)

}

