package u03

import org.junit.*
import org.junit.Assert.*

class TasksTest:
  import Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, Nil().sum)
    assertEquals(60, l.sum)

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.map(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), l.map(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), l.filter(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), l.filter(_ != 20))

  @Test def testTake() =
    assertEquals(Cons(10, Cons(20, Nil())), l.take(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), l.take(3))
    assertEquals(Nil(), l.take(0))
    assertEquals(Nil(), Nil().take(2))

  @Test def testZip() =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(
      Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))),
      l.zip(l2)
    )
    assertEquals(Nil(), l.zip(Nil()))
    assertEquals(Nil(), Nil().zip(l2))
    assertEquals(Nil(), Nil().zip(Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(
      Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))),
      l.concat(l2)
    )
    assertEquals(Cons(40, Cons(50, Nil())), Nil().concat(l2))

  @Test def testFlatMap() =
    assertEquals(
      Cons(11, Cons(21, Cons(31, Nil()))),
      l.flatMap(v => Cons(v + 1, Nil()))
    )
    assertEquals(Nil(), Nil().flatMap(v => Cons(v, Nil())))

  import Optionals.Optional.*

  @Test def testMin() =
    assertEquals(Just(10), l.min)
    assertEquals(Just(1), Cons(1, Nil()).min)
    assertEquals(Empty(), Nil().min)

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, lst.foldLeft(0)(_ - _))
    assertEquals("3715", lst.foldLeft("")(_ + _))

  @Test def testListifyCorusesWithoutTeachers() =
    val s = Sequence.Cons(Person.Student("mario", 1), Nil())
    assertEquals(Nil(), courses(Nil()))
    assertEquals(Nil(), courses(s))

  
  import Person.*

  @Test def testListifyCoursesWithTeachers() =
    val c1 = "PPS"
    val c2 = "PCD"
    val s = Cons(
      Student("Rossi", 2),
      Cons(
        Teacher("Viroli", c1),
        Cons(Student("Bianchi", 1), Cons(Teacher("Ricci", c2), Nil()))
      )
    )
    assertEquals(Cons(c1, Cons(c2, Nil())), courses(s))

  import Streams.*
  @Test def testSeriesIterator(): Unit =
    def pellGen(n: Int): Int = n match
      case n if n > 1 => 2 * pellGen(n - 1) + pellGen(n - 2)
      case 1          => 1
      case _          => 0

    val pell: Stream[Int] = Stream.iterateSeries(pellGen(_))(0)

    assertEquals(
      Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil()))))),
      Stream.toList(Stream.take(pell)(5))
    )
