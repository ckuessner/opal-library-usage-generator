package de.ckuessner.opal.usagegen.smoketests

import de.ckuessner.opal.usagegen.UsageGeneratorCli
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.file.{Files, Paths}

class LoadAndRunTestJars extends AnyFlatSpec {

  def testProject(testProjectJarBaseName: String): Unit = {
    val jarFile = Paths.get(getClass.getResource(s"/$testProjectJarBaseName.jar").toURI)

    val outputFile = Files.createTempFile(s"usagegen-smoketest-output-", ".jar")

    val args: Array[String] =
      Array("-rf") ++ Array(jarFile.toFile.getAbsolutePath, outputFile.toFile.getAbsolutePath)

    println("Running " + args.mkString(" "))
    UsageGeneratorCli.main(args)
  }

  "Interface Test Jar" should "not fail" in {
    testProject("interfacesTestResources")
  }

  "Instance Searcher Test Jar" should "not fail" in {
    testProject("instancesearcherTestResources")
  }

}
