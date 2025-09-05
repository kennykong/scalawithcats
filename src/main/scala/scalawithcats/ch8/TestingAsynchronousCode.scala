package scalawithcats.ch8


import cats.{Applicative, Id}

import scala.concurrent
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, TimeUnit}

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}

trait TestUptimeClient extends UptimeClient[Id] {
  def getUptime(hostname: String): Int
}

import cats.instances.future._ // for Applicative
import cats.instances.list._ // for Traverse
import cats.syntax.traverse._ // for traverse
import scala.concurrent.ExecutionContext.Implicits.global
//import cats.syntax.functor
import cats.implicits.toFunctorOps

class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}


class TestUptimeClientFutureImpl(hosts: Map[String, Int]) extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int] =
    Future.successful(hosts.getOrElse(hostname, 0))
}

class TestUptimeClientImpl(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Int =
    hosts.getOrElse(hostname, 0)
}


object TestingAsynchronousCode extends App {

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClientImpl(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  //  def testTotalUptimeFuture() = {
  //    val hosts = Map("host1" -> 10, "host2" -> 6)
  //    val client = new TestUptimeClientFutureImpl(hosts)
  //    val service = new UptimeService(client)
  //    val actual = service.getTotalUptime(hosts.keys.toList)
  //    val expected = hosts.values.sum
  //    assert(actual == expected)
  //  }

  testTotalUptime()
  //  testTotalUptimeFuture()
}
