package u03

import org.junit.*
import org.junit.Assert.*

class FlatMapVariantTest {

  import Sequences.*
  import Sequence.*
  import FlatMapVariant.FilteredMap.*
  import FlatMapVariant.FilteredMap.given

  @Test def testmapAndFilter(): Unit =
    val lst1 = Cons(2, Cons(3, Cons(4, Cons(5, Nil()))))
    val lst2 = Cons(1, lst1)
    val expectedList = Cons(4, Cons(8, Nil()))
    val mapper: Int => Optional[Int] = x =>
      x % 2 match
        case 0 => Optional.Just(x * 2)
        case _ => Optional.Empty()
    assertEquals(expectedList, lst1.mapAndFilter(mapper))
    assertEquals(expectedList, lst2.mapAndFilter(mapper))

  import Person.*

  @Test def testListifyCorusesWithoutTeachers(): Unit =
    val s = Sequence.Cons(Person.Student("Bianchi", 1), Nil())
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
}
