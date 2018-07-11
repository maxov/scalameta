package example

object BasicTest {

  object literals {

    val check_intLit = 140
    val check_stringLit = "hello"
    val check_floatLit = 160.7

  }

  object defs {

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

  }

}
