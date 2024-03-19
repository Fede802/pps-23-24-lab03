package u03

object TasksPart2:
    import Sequences.*
    import Sequence.*
    import u02.Modules.*

    // def courses(s: Sequence[Person]): Sequence[String] = 
    //     map[Person, String](filter(s)((p) => p match
    //         case Person.Student(name, year) => false
    //         case Person.Teacher(name, course) => true
    //     ))((t) => t match
    //         case Person.Teacher(name, course) => course
    //     )

    // def courses(s: Sequence[Person]): Sequence[String] = 
    //     flatMap[Person, String](filter(s)((p) => p match
    //         case Person.Student(name, year) => false
    //         case Person.Teacher(name, course) => true
    //     ))((t) => t match
    //         case Person.Teacher(name, course) => Cons(course, Nil())
    //     )

    def courses(s: Sequence[Person]): Sequence[String] = 
        flatMap[Person, String](s)((t) => t match
            case Person.Teacher(name, course) => Cons(course, Nil())
            case _ => Nil()
        )
    

