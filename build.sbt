import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

name := "evogame"

version := "1.0"

scalaVersion := "2.11.8"

workbenchSettings

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

bootSnippet := "evogame.Evogame().main();"

refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)
