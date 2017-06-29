// Start writing your ScalaFiddle code here


import cats.syntax.flatMap._

object MonadTransInterpretation{

  // Your effect API
  trait IO[P[_]]{
    def read(): P[String]
    def write(s: String): P[Unit]
  }

  object IO{
    def apply[P[_]](implicit F: IO[P]) = F
  }

  // Your error API
  trait Error[P[_]]{
    def fail[A](): P[A]
  }

  object Error{
    def apply[P[_]](implicit E: Error[P]) = E
  }

  import cats.Monad

  // Your imperative business logic with errors

  import cats.syntax.flatMap._, cats.syntax.functor._

  def echo[P[_]: IO: Error: Monad](): P[String] = for{
    s <- IO[P].read()
    _ <- if (s == "") Error[P].fail()
      else Monad[P].pure(()) // skip
    _ <- IO[P].write(s)
  } yield s

  // Interpretation

  object Stack1{
    import scala.concurrent.{ExecutionContext, Future}
    import cats.data.ReaderT

    type Stack[T] = ReaderT[Future,ExecutionContext,T]

    implicit val StackIO = new IO[Stack]{
      def read() = ReaderT{ implicit sc =>
        Future("hi")
      }
      def write(s: String) = ReaderT{ implicit sc =>
        Future(())
      }
    }

    implicit val StackError = new Error[Stack]{
      def fail[A]() = ReaderT{ implicit sc =>
        Future.failed(throw new RuntimeException())
      }
    }

    // Can't reuse the default ReaderT monad.
    implicit val StackMonad = new Monad[Stack]{
      import cats.instances.future._

      def pure[A](a: A) = ReaderT{ implicit sc =>
        Monad[Future].pure(a)
      }

      def flatMap[A,B](a: Stack[A])(f: A => Stack[B]) =
        ReaderT{ implicit sc: ExecutionContext =>
          Monad[Future].flatMap(a.run(sc))(f andThen (_.run(sc)))
        }

      def tailRecM[A,B](a: A)(f: A => Stack[Either[A,B]]): Stack[B] =
        ReaderT{ implicit sc: ExecutionContext =>
          Monad[Future].tailRecM(a)(f andThen (_.run(sc)))
        }
    }

    def asyncEcho(): ReaderT[Future, ExecutionContext, String] =
      echo[Stack]()
  }

}