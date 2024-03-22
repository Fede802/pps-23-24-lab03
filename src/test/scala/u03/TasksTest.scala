package u03

import org.junit.*
import org.junit.Assert.*

class TasksTest:

  // Tasks – part 1 (lists) && Tasks – part 2 (more on lists) (without es 3)
  // svolto da solo
  import Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, Nil().sum)
    assertEquals(60, l.sum)

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.map(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), l.map(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), l.filter(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), l.filter(_ != 20))

  @Test def testTake(): Unit =
    assertEquals(Cons(10, Cons(20, Nil())), l.take(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), l.take(3))
    assertEquals(Nil(), l.take(0))
    assertEquals(Nil(), Nil().take(2))

  @Test def testZip(): Unit =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(
      Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))),
      l.zip(l2)
    )
    assertEquals(Nil(), l.zip(Nil()))
    assertEquals(Nil(), Nil().zip(l2))
    assertEquals(Nil(), Nil().zip(Nil()))

  @Test def testConcat(): Unit =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(
      Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))),
      l.concat(l2)
    )
    assertEquals(Cons(40, Cons(50, Nil())), Nil().concat(l2))

  @Test def testFlatMap(): Unit =
    assertEquals(
      Cons(11, Cons(21, Cons(31, Nil()))),
      l.flatMap(v => Cons(v + 1, Nil()))
    )
    assertEquals(Nil(), Nil().flatMap(v => Cons(v, Nil())))

  @Test def testMin(): Unit =
    assertEquals(Optional.Just(10), l.min)
    assertEquals(Optional.Just(1), Cons(1, Nil()).min)
    assertEquals(Optional.Empty(), Nil().min)

  @Test def testFoldLeft(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, lst.foldLeft(0)(_ - _))
    assertEquals("3715", lst.foldLeft("")(_ + _))

  @Test def testmapAndFilter(): Unit =
    val lst1 = Cons(2, Cons(3, Cons(4, Cons(5, Nil()))))
    val lst2 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil())))))
    val expectedList = Cons(4, Cons(8, Nil()))
    val mapper: Int => Optional[Int] = x =>
      x % 2 match
        case 0 => Optional.Just(x * 2)
        case _ => Optional.Empty()
    assertEquals(expectedList, lst1.mapAndFilter(mapper))
    assertEquals(expectedList, lst2.mapAndFilter(mapper))

  // Tasks – part 2 (more on lists) es 3
  // svolto da solo
  import Person.*

  @Test def testListifyCorusesWithoutTeachers(): Unit =
    val s = Sequence.Cons(Person.Student("mario", 1), Nil())
    assertEquals(Nil(), courses(Nil()))
    assertEquals(Nil(), courses(s))

  @Test def testListifyCoursesWithTeachers(): Unit =
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

  // Tasks – part 3 (streams)
  // svolto da solo
  import Streams.*
  import Stream.*

  @Test def testIterate(): Unit =
    val str1 = Stream.iterate(0)(_ + 1)
    assertEquals(
      Cons(0, Cons(1, Cons(2, Cons(3, Nil())))),
      toList(Stream.take(str1)(4))
    )

  @Test def testStreamMap(): Unit =
    val str1 = Stream.iterate(0)(_ + 1)
    val str2 = Stream.map(str1)(_ + 1)
    assertEquals(
      Cons(1, Cons(2, Cons(3, Cons(4, Nil())))),
      toList(Stream.take(str2)(4))
    )

  @Test def testStreamFilter(): Unit =
    val str1 = Stream.iterate(0)(_ + 1)
    val str2 = Stream.filter(str1)(x => x % 2 == 1)
    assertEquals(
      Cons(1, Cons(3, Cons(5, Cons(7, Nil())))),
      toList(Stream.take(str2)(4))
    )

  @Test def takeWhile(): Unit =
    val str1 = Stream.iterate(0)(_ + 1)
    val str2 = Stream.takeWhile(str1)(_ < 5)
    assertEquals(
      Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))),
      Stream.toList(str2)
    )

  @Test def testFill(): Unit =
    assertEquals(
      Cons("a", Cons("a", Cons("a", Nil()))),
      Stream.toList(Stream.fill(3)("a"))
    )

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

  @Test def testPellGenerator(): Unit =
    val pell: Stream[Int] = Stream.pellSeries()
    assertEquals(
      Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil()))))),
      Stream.toList(Stream.take(pell)(5))
    )
