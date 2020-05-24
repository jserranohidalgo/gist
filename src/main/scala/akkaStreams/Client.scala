import java.nio.file.Paths

import scala.concurrent._
import scala.concurrent.duration._

import akka.{ Done, NotUsed }
import akka.actor.ActorSystem
import akka.util.ByteString

import akka.stream._
import akka.stream.scaladsl._

import com.typesafe.config._

import scala.io.StdIn.readLine

object Client extends App{
  implicit val system: ActorSystem = ActorSystem("QuickStart")
  implicit val ec = system.dispatcher

  val connection =
    Tcp().outgoingConnection("127.0.0.1", 8888)
      .join(Flow[ByteString]
        .via(Framing.delimiter(ByteString("\n"),
                maximumFrameLength = 256, allowTruncation = true))
        .map(_.utf8String)
        .map(text => println("Server: " + text))
        .map(_ => readLine(">> "))
        .takeWhile(_ != "q")
        .concat(Source.single("BYE"))
        .map(elem => ByteString(s"$elem\n")))

  //val connected = connection.run()


  //connected.onComplete(_ => system.terminate)
}
