package ffgg

import scalaz._
import Scalaz._

object MonadTransformers {

  type Error[A] = String \/ A

  def calculate(input: String): Error[Option[Int]] = ???

  val plus1: Option[Int] = ???

}

