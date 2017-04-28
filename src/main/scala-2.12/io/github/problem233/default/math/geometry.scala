package io.github.problem233.default.math

import io.github.problem233.default._

import scala.concurrent._
import scala.concurrent.duration.Duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Problem233
  */
object geometry {

  def pythagoreanTriple(m: BigInt, n: BigInt) = {
    require(m != n)
    Vector((m * m - n * n).abs, 2 * m * n, m * m + n * n) sortWith (_ < _) in (x => (x(0), x(1), x(2)))
  }

  def pythagoreanTriples = {
    def pythagoreanTriples1D(m: BigInt, n: BigInt): Stream[(BigInt, BigInt, BigInt)] =
      if (coprime(m, n)) pythagoreanTriple(n, m) #:: pythagoreanTriples1D(m, n + 2)
      else pythagoreanTriples1D(m, n + 2)
    def pythagoreanTriples2D(m: BigInt): Stream[Stream[(BigInt, BigInt, BigInt)]] =
      pythagoreanTriples1D(m, m + 1) #:: pythagoreanTriples2D(m + 1)
    pythagoreanTriples2D(1)
  }

  def searchPythagoreanTriple(num: BigInt) =
    Await.result(Future.sequence(pythagoreanTriples takeWhile {
      case (x, _, _) #:: _ => x <= num
      case _ => false
    } map (x => Future {
      x takeWhile {
        case (n, _, _) => n <= num
        case _ => false
      } filter {
        case (a, b, c) => num % a == 0 || num % b == 0 || num % c == 0
        case _ => false
      } map {
        case (a, b, c) =>
          (num / (if (num % a == 0) a else if (num % b == 0) b else c)) in (m => (a * m, b * m, c * m))
        case n => n
      }
    })), Inf).flatten.toList

}
