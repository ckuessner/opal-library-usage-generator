package de.ckuessner.opal.usagegen.smoketests

import coursier._
import de.ckuessner.opal.usagegen.UsageGeneratorCli
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import java.nio.file.Files

class RealWorldJarLoadAndRunTest extends AnyFlatSpec with BeforeAndAfterAll {

  var log4jJars: Seq[File] = _
  val dependencies: Array[Dependency] = Array(
    dep"org.apache.logging.log4j:log4j-core:2.17.2",
    dep"com.google.guava:guava:31.1-jre",
    dep"com.fasterxml.jackson.core:jackson-core:2.13.2",
  )
  var libraries: Map[Dependency, (File, List[File])] = _

  override def beforeAll: Unit = {
    libraries = dependencies.map(libraryName =>
      Fetch().addDependencies(libraryName).run() match {
        case library :: dependencies => libraryName -> (library, dependencies)
      }).toMap

    Console.out.flush()
    Console.err.flush()
  }

  behavior of "UsageGeneratorCli"

  private def run(dep: Dependency): Unit = {
    val outputFile = Files.createTempFile(s"usagegen-smoketest-output-${dep.module.name.value}", ".jar")
    libraries(dep) match {
      case (libraryFile, libDepFiles) => {
        val args: Array[String] =
          Array("-rf") ++ (
            if (libDepFiles.nonEmpty) Array("--runtime-jars", libDepFiles.map(_.getAbsolutePath).mkString(","))
            else Array.empty[String]
            ) ++ Array(libraryFile.getAbsolutePath, outputFile.toFile.getAbsolutePath)

        println("Running " + args.mkString(" "))
        UsageGeneratorCli.main(args)
        Console.out.flush()
        Console.err.flush()
      }
    }
  }

  it should "not fail for library log4j" in {
    run(dependencies(0))
  }

  it should "not fail for library guava" in {
    run(dependencies(1))
  }

  it should "not fail for library jackson-core" in {
    run(dependencies(2))
  }

}
