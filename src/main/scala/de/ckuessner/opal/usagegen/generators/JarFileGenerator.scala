package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.ClassByteCode

import java.io.File
import java.nio.file.{FileAlreadyExistsException, FileSystems, Files}

object JarFileGenerator {
  def writeClassFilesToJarFile(outputFile: File,
                               classes: Iterable[ClassByteCode],
                               overwriteOutFile: Boolean,
                               mainClass: Option[String] = None
                              ): Unit = {

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

    if (mainClass.isDefined) {
      jarManifest.getMainAttributes.put(java.util.jar.Attributes.Name.MAIN_CLASS, mainClass.get)
    }

    // Add jar manifest to jar
    val metaInfDir = zipRoot.resolve("META-INF")
    Files.createDirectory(metaInfDir)
    val manifestOutputStream = Files.newOutputStream(metaInfDir.resolve("MANIFEST.MF"))
    jarManifest.write(manifestOutputStream)
    manifestOutputStream.close()

    // Write class files and directories to jar
    for (clazz <- classes) {
      val jvmType = clazz.jvmClassName
      // Ensure that the directory exists in jar
      Files.createDirectories(zipRoot.resolve(jvmType).getParent)
      // Write class to jar
      Files.write(zipRoot.resolve(jvmType + ".class"), clazz.byteCode)
    }

    zipFs.close()
  }
}
