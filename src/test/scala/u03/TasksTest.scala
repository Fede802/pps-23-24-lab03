package u03

import org.junit.*
import org.junit.Assert.*
import u02.AlgebraicDataTypes.Person
import u03.TasksPart2.courses

class TaskPart2Test {
    
    import Sequences.*
    import Sequence.*
    import u02.Modules.Person.*
    

    @Test def testListifyCorusesWithoutTeachers() =
        val s = Cons(Student("mario",1),Nil())
        assertEquals(Nil(), courses(Nil()))
        assertEquals(Nil(), courses(s))

    @Test def testListifyCoursesWithTeachers() =
        val c1 ="PPS"
        val c2 = "PCD"
        val s = Cons(Student("Rossi",2),Cons(Teacher("Viroli", c1), Cons(Student("Bianchi",1), Cons(Teacher("Ricci", c2), Nil()))))
        assertEquals(Cons(c1, Cons(c2, Nil())), courses(s))


}
