package de.ckuessner.opal.usagegen.generators

import org.opalj.ba.CLASS

import java.io.File
import java.nio.file.{FileAlreadyExistsException, FileSystems, Files, Path}

object JarFileGenerator {
  def writeClassFilesToJarFile(outputFilePath: Path, classes: Iterable[CLASS[_]], overwriteOutFile: Boolean): Unit = {
    writeClassfilesToJarFile(outputFilePath.toFile, classes, overwriteOutFile)
  }

  def writeClassfilesToJarFile(outputFile: File, classes: Iterable[CLASS[_]], overwriteOutFile: Boolean): Unit = {
    if (!outputFile.getName.matches(".*\\.(jar|zip)"))
      throw new IllegalArgumentException("outputFile must end with .jar or .zip")

    if (outputFile.exists()) {
      if (overwriteOutFile) {
        outputFile.delete()
      } else {
        throw new FileAlreadyExistsException(outputFile.toString)
      }
    }

    val env = java.util.Map.of("create", "true")
    val zipFs = FileSystems.newFileSystem(outputFile.toPath, env)
    val zipRoot = zipFs.getPath("/")

    val jarManifest = new java.util.jar.Manifest()
    jarManifest.getMainAttributes.put(java.util.jar.Attributes.Name.MANIFEST_VERSION, "1.0")

    // Add jar manifest to jar
    val metaInfDir = zipRoot.resolve("META-INF")
    Files.createDirectory(metaInfDir)
    val manifestOutputStream = Files.newOutputStream(metaInfDir.resolve("MANIFEST.MF"))
    jarManifest.write(manifestOutputStream)
    manifestOutputStream.close()

    // Write class files and directories to jar
    for (classFile <- classes.map(elem => elem.toDA._1)) {
      val rawClassFile: Array[Byte] = org.opalj.bc.Assembler(classFile)
      val fqClassName = classFile.thisType.asJVMType

      // Ensure that the directory exists in jar
      Files.createDirectories(zipRoot.resolve(fqClassName).getParent)
      // Write class to jar
      Files.write(zipRoot.resolve(fqClassName + ".class"), rawClassFile)
    }

    zipFs.close()
  }
}
