package de.ckuessner.opal.usagegen.smoketests

import coursier._
import de.ckuessner.opal.usagegen.UsageGeneratorCli
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.file.Files

class RealWorldJarLoadAndRunTest extends AnyFunSuite {

  var log4jJars: Seq[File] = _
  val dependencies: Array[Dependency] = Array(
    dep"org.apache.commons:commons-collections4:4.4",
    //dep"org.apache.logging.log4j:log4j-core:2.17.2", // TODO: This runs forerver
    //dep"com.google.guava:guava:31.1-jre",            // TODO: This also runs forever
    dep"com.fasterxml.jackson.core:jackson-core:2.13.2",
    dep"org.slf4j:slf4j-nop:2.0.0-alpha7",
    dep"javax.xml.bind:jaxb-api:2.3.1"
  )
  val libraries: Map[Dependency, (File, List[File])] = dependencies.map(libraryName =>
    Fetch().addDependencies(libraryName).run() match {
      case library :: dependencies => libraryName -> (library, dependencies)
    }).toMap

  private def run(dep: Dependency): Unit = {
    Console.out.flush()
    Console.err.flush()

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
      }
    }

    Console.out.flush()
    Console.err.flush()
  }

  for ((dep, _) <- libraries) {
    test(s"Running with library ${dep.module.name.value} should not throw an exception") {
      run(dep)
    }
  }
}
