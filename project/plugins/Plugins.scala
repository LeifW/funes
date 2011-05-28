import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val coffeeScriptSbtRepo = "coffeeScript sbt repo" at "http://repo.coderlukes.com"
  val coffeeScript = "org.coffeescript" % "coffee-script-sbt-plugin" % "1.0"
  val jcoffeescript = "org.jcoffeescript" % "jcoffeescript" % "1.0" from "http://cloud.github.com/downloads/yeungda/jcoffeescript/jcoffeescript-1.0.jar"
  val retronymSnapshotRepo = "retronym's repo" at "http://retronym.github.com/repo/releases"
  val onejarSBT = "com.github.retronym" % "sbt-onejar" % "0.2"
}

// vim: set ts=4 sw=4 et:
