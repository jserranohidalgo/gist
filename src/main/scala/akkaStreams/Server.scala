import java.nio.file.Paths

import scala.concurrent._
import scala.concurrent.duration._

import akka.{ Done, NotUsed }
import akka.actor.ActorSystem
import akka.util.ByteString

import akka.stream._
import akka.stream.scaladsl._

import com.typesafe.config._

object Server extends App{

  implicit val system: ActorSystem = ActorSystem("QuickStart") // , ConfigFactory.load(customConf))

  implicit val ec = system.dispatcher

  import Tcp._

  import akka.stream.scaladsl.Framing

  val connections: Source[IncomingConnection, Future[ServerBinding]] =
    Tcp().bind("127.0.0.1", 8888)

  val f: Future[Done] = connections.runForeach{ connection: IncomingConnection =>
    import connection._

    connection.handleWith(Flow[ByteString]
      .via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 256, allowTruncation = true))
      .map(_.utf8String)
      .via(Flow[String].takeWhile(_ != "BYE").map(_ + "!"))
      // merge in the initial banner after parser
      .merge(Source.single(s"Welcome to: $localAddress, you are: $remoteAddress!"))
      .map(_ + "\n")
      .map(ByteString(_))): NotUsed
    ()
  }

  println(f)
  f.onComplete(_ => system.terminate)
}
