package example

object BasicTest {

  object literals {

    val check_intLit = 140
    val check_stringLit = "hello"
    val check_floatLit = 160.7

  }

  object refs {

    def basicDef = 6
    def basicDefEmptyParams() = 7
    val basicVal = 5

    val check_basicDefRef = basicDef
    val check_basicDefRefEmptyParams = basicDefEmptyParams
    val check_basicDefRefEmptyParamsCall = basicDefEmptyParams()
    val check_valRef = basicVal

  }

  object applies {

    def myInc(x: Int): Int = x + 1

    val check_fnCall = myInc(5)

    def myId[T](x: T): T = x

    val check_polyTypeApply = myId[String]("hello")

    val check_polyFnCall = myId("hello")

    def createList[T](x: T): List[T] = List(x)

    val check_createList = createList[Int](3)
    val check_createListPoly = createList("hello")

    def buildListTuple[A, B](x: A, y: B): (List[A], B) = (List(x), y)

    val check_buildListTuple = buildListTuple[Int, String](3, "hello")
    val check_buildListTuplePoly = buildListTuple(6, "world")

  }

  object selects {

    class Y {

      def m: String = "hello"

    }

    class X {
      val a: Int = 3
      def b: List[Int] = List(4)
      def f(x: Int): Int = x + 1
      def g[T](x: T): List[T] = List(x)
      val y: Y = new Y
    }

    val myX = new X

    val check_select = myX.a
    val check_selectMore = myX.b
    val check_selectRepeated = myX.y.m

  }

}
