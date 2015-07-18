package cs0ip.result

import java.util.NoSuchElementException

import scala.language.implicitConversions
import scala.util.control.{NonFatal, ControlThrowable}
import scala.util.{Failure, Success, Try}

sealed trait Res[+A] {
  def isOk: Boolean

  def isErr: Boolean

  final def isEmpty: Boolean = isErr

  final def isDefined: Boolean = isOk

  def get: A

  def getOrElse[B >: A](e: => B): B

  def orElse[B >: A](e: => Res[B]): Res[B]

  def or[B >: A](f: => Res[B]): Res[B] = orElse(f)

  def orMap[B >: A](f: => B): Res[B]

  def and[B](f: => Res[B]): Res[B]

  def andMap[B](f: => B): Res[B]

  def toOption: Option[A]

  def toEither: Either[Err, A]

  def toTry: Try[A]

  def map[B](f: A => B): Res[B]

  def flatMap[B](f: A => Res[B]): Res[B]

  def foreach[U](f: A => U): Unit

  def filter(p: A => Boolean): Res[A]

  final def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  class WithFilter(p: A => Boolean) {
    def map[U](f:     A => U): Res[U]           = Res.this filter p map f
    def flatMap[U](f: A => Res[U]): Res[U]      = Res.this filter p flatMap f
    def foreach[U](f: A => U): Unit             = Res.this filter p foreach f
    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }

  def mapOk[B](f: Ok[A] => Res[B]): Res[B]

  def mapErr[B >: A](f: Err => Res[B]): Res[B]
}

object Res {
  def apply(x: Boolean, ifFalse: => Err.Er): Res[Boolean] =
    if (x) Ok.True
    else Err(ifFalse)

  def apply(x: Boolean): Res[Boolean] = apply(x, "не выполнено условие")

  def apply[A](x: Option[A]): Res[A] = x match {
    case Some(v) => Ok(v)
    case None => Err("")
  }

  def apply[A](x: Either[Err, A]): Res[A] = x match {
    case Right(v) => Ok(v)
    case Left(err) => err
  }

  def apply[A](x: Try[A], ifErr: (Throwable) => Res[A]): Res[A] = x match {
    case Success(v) => Ok(v)
    case Failure(t) => ifErr(t)
  }

  def apply[A](x: Try[A]): Res[A] = apply(x, t => Err(t))

  def apply[A](x: Try[A], err: => Err.Er): Res[A] = apply(x, _ => Err(err))

  def safe[A](f: => A, ifErr: (Throwable) => Res[A]): Res[A] =
    try {
      Ok(f)
    } catch {
      case NonFatal(e) => ifErr(e)
    }

  def safe[A](f: => A): Res[A] =
    safe(f, e => Err(e))

  def safe[A](f: => A, err: => Err.Er): Res[A] =
    safe(f, _ => Err(err))
}

final class Ok[+A] private (protected val value: A) extends Res[A] {
  def isOk: Boolean = true

  def isErr: Boolean = false

  def get: A = value

  def getOrElse[B >: A](e: => B): B = value

  def orElse[B >: A](e: => Res[B]): Res[B] = this

  def orMap[B >: A](f: => B): Res[B] = this

  def and[B](f: => Res[B]): Res[B] = f

  def andMap[B](f: => B): Res[B] = Ok(f)

  def toOption: Option[A] = Some(value)

  def toEither: Either[Err, A] = Right(value)

  def toTry: Try[A] = Success(value)

  def map[B](f: A => B): Res[B] = Ok(f(value))

  def flatMap[B](f: A => Res[B]): Res[B] = f(value)

  def foreach[U](f: A => U): Unit = f(value)

  def filter(p: A => Boolean): Res[A] =
    if (p(value)) this
    else Err(new NoSuchElementException("значение не удовлетворяет условиям фильтра"))

  def mapOk[B](f: Ok[A] => Res[B]): Res[B] = f(this)

  def mapErr[B >: A](f: Err => Res[B]): Res[B] = this
}

object Ok {
  def apply[A](value: A): Ok[A] = new Ok(value)

  def apply(u: Unit): Ok[Unit] = Unit

  def apply(b: Boolean): Ok[Boolean] =
    if (b) True
    else False

  def unapply[A](x: Res[A]): x.type = x

  val Unit: Ok[Unit] = new Ok(scala.Unit)

  val True: Ok[Boolean] = new Ok(true)

  val False: Ok[Boolean] = new Ok(false)
}

final class Err private (
  val lst: List[Err.Er],
  val ex: Option[Throwable]
)
  extends Res[Nothing]
{
  def er: Err.Er = lst.head

  def msg: String = er.msg

  def isOk: Boolean = false

  def isErr: Boolean = true

  def get: Nothing = throw new NoSuchElementException("Err")

  def getOrElse[B >: Nothing](e: => B): B = e

  def orElse[B >: Nothing](e: => Res[B]): Res[B] = e

  def orMap[B >: Nothing](f: => B): Res[B] = Ok(f)

  def and[B](f: => Res[B]): Res[B] = this

  def andMap[B](f: => B): Res[B] = this

  def toOption: Option[Nothing] = None

  def toEither: Either[Err, Nothing] = Left(this)

  def toTry: Try[Nothing] = {
    val t = ex match {
      case Some(e) => e
      case None => new Exception(msg)
    }
    Failure(t)
  }

  def map[B](f: (Nothing) => B): Res[B] = this

  def flatMap[B](f: (Nothing) => Res[B]): Res[B] = this

  def foreach[U](f: (Nothing) => U): Unit = ()

  def filter(p: (Nothing) => Boolean): Res[Nothing] = this

  def mapOk[B](f: Ok[Nothing] => Res[B]): Res[B] = this

  def mapErr[B >: Nothing](f: Err => Res[B]): Res[B] = f(this)

  def copy(
    lst: List[Err.Er] = this.lst,
    ex: Option[Throwable] = this.ex
  ) = new Err(lst, ex)

  def push(err: Err.Er): Err = copy(lst = err :: this.lst)

  def replace(er: Err.Er): Err = lst match {
    case head :: tail => copy(lst = er :: tail)
    case _ => throw new MatchError("список сообщений пуст")
  }

  def mapEr(f: Err.Er => Err.Er): Err =
    replace(f(er))

  def mapMsg(f: String => String): Err =
    replace(f(msg))

  def withEx(e: Option[Throwable]): Err =
    copy(ex = e)

  def throwExOr(t: => Throwable): Nothing = ex match {
    case Some(e) => throw e
    case None => throw t
  }
}

object Err {
  trait Er {
    def msg: String
  }

  object Er {
    implicit def fromString(s: String): Er = Msg(s)

    def unapply(x: Er): Option[String] = Some(x.msg)
  }

  class Msg(val msg: String) extends Er

  object Msg {
    def apply(msg: String): Msg = new Msg(msg)
    def unapply(x: Er): Option[String] = Some(x.msg)
  }

  def apply(er: Er): Err = apply(er, Some(new Exception))

  def apply(ex: Throwable, err: Er): Err = apply(err, Some(ex))

  def apply(ex: Throwable): Err = {
    val msg = {
      val m = ex.getMessage
      if (m == null) "" else m
    }
    apply(msg, Some(ex))
  }

  def apply(er: Er, ex: Option[Throwable]): Err = new Err(er :: Nil, ex)

  def noTrace(er: Er): Err = apply(er, None)
}

class Exit[A] {
  private class Control(val value: A) extends ControlThrowable {
    def owner: Exit[A] = Exit.this
  }

  def go(v: A): Nothing = throw new Control(v)

  def apply(f: Exit[A] => A): A = {
    try {
      f(this)
    } catch {
      case e: Control if e.owner eq this => e.value
      case e: Throwable => throw e
    }
  }
}

object Exit {
  def apply[A]: Exit[A] = new Exit[A]

  def as[A](mark: A): Exit[A] = apply[A]
}
