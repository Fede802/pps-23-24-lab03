package u03

import u03.Optionals.Optional

object Sequences:

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    extension (l: Sequence[Int])
      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _          => 0

      def min: Optional[Int] = l match
        case Cons(h1, Nil()) => Optional.Just(h1)
        case Cons(h1, t1) =>
          t1 match
            case Cons(h2, t2) if h1 < h2 => Cons(h1, t2).min
            case _                       => t1.min
        case Nil() => Optional.Empty()

    extension [A](l: Sequence[A])
      def map[B](mapper: A => B): Sequence[B] =
        l.flatMap(k => Cons(mapper(k), Nil()))

      def filter(pred: A => Boolean): Sequence[A] =
        l.flatMap(k =>
          pred(k) match
            case true => Cons(k, Nil())
            case _    => Nil()
        )

      def zip[B](second: Sequence[B]): Sequence[(A, B)] =
        (l, second) match
          case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zip(t2))
          case _                            => Nil()

      def take(n: Int): Sequence[A] = l match
        case Cons(h, t) if n > 0 => Cons(h, t.take(n - 1))
        case _                   => Nil()

      def concat(l2: Sequence[A]): Sequence[A] = l match
        case Cons(h, t) => Cons(h, t.concat(l2))
        case _          => l2

      def flatMap[B](mapper: A => Sequence[B]): Sequence[B] =
        l match
          case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))
          case _          => Nil()

      def foldLeft[B](a: B)(f: (B, A) => B): B = l match
        case Cons(h, t) => t.foldLeft(f(a, h))(f)
        case Nil()      => a
  end Sequence
import Sequences.*
import Sequence.*

enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)

def courses(s: Sequence[Person]): Sequence[String] =
  s.flatMap[String]((t) =>
    t match
      case Person.Teacher(name, course) => Cons(course, Nil())
      case _                            => Nil()
  )

object Streams:
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _          => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _                => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] =
      stream match
        case Cons(head, tail) if (pred(head())) =>
          cons(head(), filter(tail())(pred))
        case Cons(head, tail) => filter(tail())(pred)
        case _                => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 =>
        cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] =
      stream match
        case Cons(head, tail) if pred(head()) =>
          cons(head(), takeWhile(tail())(pred))
        case _ => Empty()

    def fill[A](n: Int)(x: A): Stream[A] = n match
      case 0 => Empty()
      case _ => cons(x, fill(n - 1)(x))

    def iterateSeries(f: Int => Int)(i: Int): Stream[Int] =
      cons(f(i), iterateSeries(f)(i + 1))
