import sbt.file
import sbtassembly.AssemblyPlugin.autoImport.assembly

name := "opal-generate-library-usage"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.15"

lazy val root: Project = project
  .in(file("."))
  .settings(
    libraryDependencies += "de.opal-project" % "framework_2.12" % "4.0.0",
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1",

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.11",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test",

    libraryDependencies += "io.get-coursier" %% "coursier" % "2.1.0-M5-18-gfebf9838c",

    assembly / assemblyJarName := "usagegen.jar",
    assembly / assemblyExcludedJars := {
      val cp = (assembly / fullClasspath).value
      cp.filter { f =>
        Seq("coursier", "scalatest", "jniutils", "windows-jni-utils", "windows-ansi", "jansi", "plexus")
          .exists { excluded => f.data.getName.contains(excluded) }
      }
    },
    assembly / assemblyMergeStrategy := discardModuleInfoMergeStrategy,

    Test / compile := (Test / compile).dependsOn(
      instancesearcherTestResources / copyJarToTestResources,
      interfacesTestResources / copyJarToTestResources
    ).value
  )

lazy val discardModuleInfoMergeStrategy: (String => sbtassembly.MergeStrategy) = {
  case "module-info.class" => MergeStrategy.discard
  case other => MergeStrategy.defaultMergeStrategy(other)
}

val copyJarToTestResources = taskKey[Unit]("Copy jar from packageBin to root test resources")

val instancesearcherTestResources = project.in(file("testfiles/instancesearcher/"))
  .settings(
    Compile / javacOptions ++= Seq("-source", "8", "-target", "8"),
    copyJarToTestResources := {
      (Compile / compile).value
      val sourceFile = (Compile / packageBin).value
      doCopyJarToTestResources(sourceFile)
    },
  )

val interfacesTestResources = project.in(file("testfiles/interfaces/"))
  .settings(
    Compile / javacOptions ++= Seq("-source", "8", "-target", "8"),
    copyJarToTestResources := {
      (Compile / compile).value
      val sourceFile = (Compile / packageBin).value
      doCopyJarToTestResources(sourceFile)
    },
  )

def doCopyJarToTestResources(sourceFile: File): Unit = {
  val targetFileName = sourceFile.getName.replaceFirst("_\\d\\..*-SNAPSHOT", "")
  val targetFile = root.base / "src/test/resources" / targetFileName
  IO.copyFile(sourceFile, targetFile, CopyOptions(overwrite = true, preserveLastModified = false, preserveExecutable = false))
}