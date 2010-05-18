import sbt._

class AllForGoodProject(info: ProjectInfo) extends DefaultWebProject(info) {
  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

  // val scalatools_snapshot = "Scala Tools Snapshot" at "http://scala-tools.org/repo-snapshots/"
  val scalatools_release = "Scala Tools Release" at "http://scala-tools.org/repo-releases/"

  val liftVersion = "2.0-scala280-SNAPSHOT"

  override def libraryDependencies = Set(
    "net.liftweb" % "lift-webkit" % liftVersion % "compile->default",
    "net.liftweb" % "lift-mapper" % liftVersion % "compile->default",
    "net.liftweb" % "lift-testkit" % liftVersion % "compile->default",
    "org.apache.lucene" % "lucene-core" % "3.0.1" % "compile",

    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test",
    "com.h2database" % "h2" % "1.2.134",
    "junit" % "junit" % "4.7" % "test",
    "org.scala-tools.testing" % "specs_2.8.0.Beta1" % "1.6.4" % "test",

    "joda-time" % "joda-time" % "1.6"
  ) ++ super.libraryDependencies

}
