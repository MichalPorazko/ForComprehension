object Methods {

  def findTheAuthor(book: Book): Option[Author] =
    book.author match
      case  Some(author) => Some(author)
      case None => None


  def findTheAuthorName(book: Book): Option[String] =
    book.author.map(author => author.name)


  def oddIsbnUpperCase(book: Book): Option[String] =
    book.isbn.map(isbn => isbn.zipWithIndex.filter{
      case (c, i) => c.isUpper && (i % 2 == 0)}
      .map(_._1).mkString
    )

  def authorBibliogaphy(book: Book): Option[List[String]] =
    book.author.map(_.bibliography).flatten


  /*
  * The flatMap function is used to handle Option types and should return an Option.
  * In your case, author.age >= age results in a Boolean, not an Option[Boolean].
  * */


  def ifAuthorOldEnough(book: Book, age: Int): Option[Boolean] =
    book.author.flatMap(author =>
      if (author.age > age) Some(true)
      else None)

  def authorsBibliogarphy(book: Option[Book]): Option[List[String]] =
    book.flatMap{ book => book.author.flatMap{ author => author.bibliography}}


  // using for comprehension
  def authorsBibliogarphyFC(book: Option[Book]): Option[List[String]] =
    for {
      b <- book
      author <- b.author
      bibliography <- author.bibliography
    } yield bibliography


  def function1(n: Int): Option[Int] =
    if (n < 5) Some(n * 2)
    else None

  def function2(optA: Option[Int]): Option[Int] =
    for {
      a <- optA
      b <- function1(a)
      c <- Some(5 * b)
    } yield c


  //using for comprehension
  def ifAuthorOldEnoughFC(book: Book, age: Int): Option[String] =
    for {
      author <- book.author
      if (author.age > age)
    } yield author.name


  def wrongUsageOfGet(a: Option[Int]): Int =
    if (a.isDefined) a.get
    else 0

  def properUsageWithOutGet(a: Option[Int]): Int =
    a.getOrElse(0)


// if the book has a author that has a blbiography
  def ifTheAuthorHasBibliography(book: Option[Book]): Option[Book] =
    for {
      b <- book
      author <- b.author
      if author.bibliography.isDefined
    } yield b




}
