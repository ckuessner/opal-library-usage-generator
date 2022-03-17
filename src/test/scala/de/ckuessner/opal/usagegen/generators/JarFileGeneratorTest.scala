package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.ClassByteCode
import de.ckuessner.opal.usagegen.generators.JarFileGeneratorTest.{testClass, zipFileToEntryList}
import org.opalj.ba.METHODS
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.{contain, exist}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import java.io.File
import java.nio.file.{FileAlreadyExistsException, Files, Path}
import java.util.zip.{ZipEntry, ZipFile}
import scala.language.{implicitConversions, postfixOps}

class JarFileGeneratorTest extends AnyFlatSpec with BeforeAndAfterAll {
  var tempPath: Path = _;

  override protected def beforeAll(): Unit = {
    tempPath = Files.createTempDirectory(getClass.getName)
  }

  override protected def afterAll(): Unit = {
    tempPath.toFile.delete()
  }

  "JarFileGenerator" should "create jar file with only MANIFEST, if no classes specified" in {
    val jarFile = tempPath.resolve("test1.jar").toFile
    JarFileGenerator.writeClassFilesToJarFile(
      jarFile,
      List.empty,
      overwriteOutFile = false,
      None
    )

    jarFile should exist

    zipFileToEntryList(jarFile, ignoreDirectories = true).map(_.getName) shouldEqual List("META-INF/MANIFEST.MF")

    jarFile.delete()
  }

  it should "not overwrite file only overwriteOutFile = false" in {
    val testFilePath = tempPath.resolve("test2.jar")
    val testFile = testFilePath.toFile
    testFile shouldNot exist
    testFile.createNewFile()
    Files.writeString(testFilePath, "Not a jar")
    testFile should exist

    assertThrows[FileAlreadyExistsException] {
      JarFileGenerator.writeClassFilesToJarFile(
        testFile,
        List.empty,
        overwriteOutFile = false,
        None
      )
    }

    Files.readString(testFilePath) shouldEqual "Not a jar"

    JarFileGenerator.writeClassFilesToJarFile(
      testFile,
      List.empty,
      overwriteOutFile = true,
      None
    )

    assert(new ZipFile(testFile) != null)

    testFile.delete()
  }

  it should "create jar file with classes in specified packages" in {
    val jarFile = tempPath.resolve("test3.jar").toFile
    JarFileGenerator.writeClassFilesToJarFile(
      jarFile,
      List(testClass),
      overwriteOutFile = false,
      None
    )

    val entryNames = zipFileToEntryList(jarFile, ignoreDirectories = false).map(_.getName)
    entryNames should contain("META-INF/")
    entryNames should contain("META-INF/MANIFEST.MF")
    entryNames should contain("testpackage/")
    entryNames should contain("testpackage/TestClass.class")

    jarFile.delete()
  }
}

object JarFileGeneratorTest {

  import org.opalj.ba.{CLASS, PUBLIC}

  def zipFileToEntryList(file: File, ignoreDirectories: Boolean): List[ZipEntry] = {
    val zipFile = new ZipFile(file)
    val entries = collection.JavaConverters.enumerationAsScalaIterator(zipFile.entries())

    val filtered =
      if (ignoreDirectories) entries.filterNot(_.isDirectory)
      else entries

    filtered.toList
  }

  val testClass: ClassByteCode = ClassByteCode(
    "testpackage/TestClass",
    CLASS(
      accessModifiers = PUBLIC SUPER,
      thisType = "testpacakge/TestClass",
      methods = METHODS()
    )
  )

  implicit def classToByteCode(clazz: CLASS[_]): Array[Byte] = {
    org.opalj.bc.Assembler(clazz.toDA._1)
  }
}
