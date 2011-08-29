import sbt._
import java.io.File

class ScrabbleProject(info: ProjectInfo) extends DefaultProject(info)
{
  /* scalatest */
  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

  /* point it to local scala version */
  override def localScala = 
	defineScala("scala-2.9.1.RC3", new File("/usr/local/scala-2.9.1.RC3/")) :: 
	Nil
  
}
