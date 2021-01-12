package fdm

object phantom_types {

  /**
   * EXERCISE 1
   *
   * Add a phantom type parameter to `Socket`, which can keep track of the state of the socket:
   * either `Created` or `Connected`. Use this type parameter in the methods below to improve their
   * type safety.
   */
  type Created
  type Connected
  trait Socket[S]

  def createSocket(): Socket[Created]                                            = ???
  def connectSocket(address: String, socket: Socket[Created]): Socket[Connected] = ???
  def readSocket(socket: Socket[Connected]): Array[Byte]                         = ???

  /**
   * EXERCISE 2
   *
   * Introduce a type parameter to this data type to model whether a `Path` is a file, a directory,
   * or either a file or a directory. Use this to improve the type safety of the `readFile` and
   * `listDirectory` methods.
   *
   * Note: In order to ensure safety, you will have to make the constructors of `Path` private, so
   * that outside code cannot call those constructors with just any type parameter. This is a
   * requirement of using phantom types properly.
   */
  type File
  type Directory
  sealed trait Path[S]
  object Path {
    case object Root                                                       extends Path[Directory]
    final case class ChildOf private (path: Path[Directory], name: String) extends Path[Either[File, Directory]]
  }

  def readFile(path: Path[File]): String                                   = ???
  def listDirectory(path: Path[File]): List[Path[Either[File, Directory]]] = ???

  /**
   * EXERCISE 3
   *
   * Phantom types work well with intersection types (`with` in Scala 2.x). They have many
   * wide-ranging applications, including making builder patterns safer.
   *
   * Introduce a phantom type parameter for `PersonBuilder`, and arrange such that the setters
   * add a new type into a type intersection, and that the build method requires both age and name
   * to be set in order to build the person.
   *
   * Note: As before, you must make the constructors of the data type with a phantom type parameter
   * private, so they cannot be called from outside code.
   */
  type SetAge
  type SetName
  case class PersonBuilder[S] private (age: Option[Int], name: Option[String]) {
    def age(v: Int): PersonBuilder[SetAge] = copy(age = Some(v))

    def name(s: String): PersonBuilder[SetName] = copy(name = Some(s))
  }
  final case class Person private (name: String, age: Int)

  def build(personBuilder: PersonBuilder[SetAge with SetName]): Person =
    Person(personBuilder.name.get, personBuilder.age.get)
}
