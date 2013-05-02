import java.io.File
package object week6 {
  def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1 * xy._2).sum

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map {case (x, y) => x * y}.sum

  def scalarProduct3(xs: Vector[Double], ys: Vector[Double]): Double =
    (for ((x, y) <- xs zip ys) yield x * y).sum

  val dictionaryPath = List("forcomp", "linuxwords.txt")

  /**
   * Get a child of a file. For example,
   * 
   *   subFile(homeDir, "b", "c")
   * 
   * corresponds to ~/b/c
   */
  def subFile(file: File, children: String*) = {
    children.foldLeft(file)((file, child) => new File(file, child))
  }

  /**
   * Get a resource from the `src/main/resources` directory. Eclipse does not copy
   * resources to the output directory, then the class loader cannot find them.
   */
  def resourceAsStreamFromSrc(resourcePath: List[String]): Option[java.io.InputStream] = {
    val classesDir = new File(getClass.getResource(".").toURI)
    println("Classes dir: " + classesDir)
    val projectDir = classesDir.getParentFile.getParentFile.getParentFile.getParentFile
    println("Project dir: " + projectDir)
    val resourceFile = subFile(projectDir, ("workspace" :: "hello-project" :: "src" :: "main" :: "resources" :: resourcePath): _*)
    println("Resource file: " + resourceFile)
    if (resourceFile.exists)
      Some(new java.io.FileInputStream(resourceFile))
    else
      None
  }

  def loadDictionary = {
    val wordstream = Option {
      getClass.getClassLoader.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}