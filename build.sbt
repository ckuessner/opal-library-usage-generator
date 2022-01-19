name := "opal-generate-library-usage"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.15"

libraryDependencies += "de.opal-project" % "framework_2.12" % "4.0.0"
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1"

assembly / assemblyJarName := "usagegen.jar"
assembly / assemblyMergeStrategy := discardModuleInfoMergeStrategy

lazy val discardModuleInfoMergeStrategy: (String => sbtassembly.MergeStrategy) = {
  case "module-info.class" => MergeStrategy.discard
  case other => MergeStrategy.defaultMergeStrategy(other)
}