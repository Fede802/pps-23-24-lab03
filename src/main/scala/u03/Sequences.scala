package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u02.AnonymousFunctions.h
import scala.annotation.transparentTrait

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    // def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
    //   case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
    //   case Nil()      => Nil()

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] =
      flatMap(l)(k => Cons(mapper(k), Nil()))

    // def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
    //   case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
    //   case Cons(_, t)            => filter(t)(pred)
    //   case Nil()                 => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] =
      flatMap(l1)(k =>
        pred(k) match
          case true => Cons(k, Nil())
          case _    => Nil()
      )

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
      (first, second) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
        case _                            => Nil()

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0 => Cons(h, take(t)(n - 1))
      case _                   => Nil()

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(h, t) => Cons(h, concat(t, l2))
      case _          => l2

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      l match
        case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
        case _          => Nil()

    // def min(l: Sequence[Int]): Optional[Int] = l match
    //   case Cons(head, tail) if filter(l)(_ < head) == Nil() =>
    //     Optional.Just(head)
    //   case Cons(head, tail) => min(filter(l)(_ < head))
    //   case Nil()            => Optional.Empty()

    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(head, tail) if filter(l)(_ < head) == Nil() =>
        Optional.Just(head)
      case Cons(head, tail) => min(filter(l)(_ < head))
      case Nil()            => Optional.Empty()

@main def trySequences =
  import Sequences.*
  val l =
    Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 60

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52

  // take
  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  println(take(lst)(2)) // Cons (10 , Cons (20 , Nil ()))
  println(take(lst)(0)) // Nil ()
  println(take(lst)(5)) // Cons (10 , Cons (20 , Cons (30 , Nil ())))

  // zip
  val lst1 = Cons(10, Cons(20, Cons(30, Nil())))
  val lst2 = Cons("a", Cons("b", Cons("c", Nil())))
  println(
    zip(lst1, lst2)
  ) // Cons ((10 ,a), Cons ((20 ,b), Cons ((30 ,c), Nil ())))

  // concat
  val lst3 = Cons(10, Cons(20, Nil()))
  val lst4 = Cons(30, Cons(40, Nil()))
  println(
    concat(lst3, lst4)
  ) // Cons (10 , Cons (20 , Cons (30 , Cons (40 , Nil ))))

  // flatmap
  println(
    flatMap(lst)(v => Cons(v + 1, Nil()))
  ) // Cons (11 , Cons (21 , Cons (31 , Nil ())))
  println(flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))
  // Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))
