import concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import cats.implicits._
import cats._

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Int =
    hosts.getOrElse(hostname, 0)
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

// Unit test
def testTotalUptime() = {
  val hosts = Map("host1" -> 10, "host2" -> 6)
  val client = new TestUptimeClient(hosts)
  val service = new UptimeService(client)
  val actual = service.getTotalUptime(hosts.keys.toList)
  val expected = hosts.values.sum
  assert(actual == expected)
}
