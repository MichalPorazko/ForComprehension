object Main {

  def main(args: Array[String]): Unit = {

    println(Methods.function2(Some(1)))
    println(Methods.function2(Some(5)))
    println(Methods.function2(None))

  }

}
