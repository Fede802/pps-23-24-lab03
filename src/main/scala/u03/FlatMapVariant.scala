package u03

object FlatMapVariant:

  trait FilteredMap[T[_]]:
    def mapAndFilter[A, B](t: T[A])(
        mapper: A => Optional[B]
    ): T[B]

  object FilteredMap:
    extension [A, T[_]: FilteredMap](a: T[A])
      def mapAndFilter[B](mapper: A => Optional[B]): T[B] =
        summon[FilteredMap[T]].mapAndFilter(a)(mapper)

  import Sequences.*

  given FilteredMap[Sequence] with
    def mapAndFilter[A, B](t: Sequence[A])(
        mapper: A => Optional[B]
    ): Sequence[B] =
      t.flatMap(
        mapper(_) match
          case Optional.Just(a) => Sequence.Cons(a, Sequence.Nil())
          case _                => Sequence.Nil()
      )

  import Person.*
  import FilteredMap.*
  import FilteredMap.given

  def courses(s: Sequence[Person]): Sequence[String] =
    println("using")
    s.mapAndFilter(_ match
      case Person.Teacher(name, course) => Optional.Just(course)
      case _                            => Optional.Empty()
    )
