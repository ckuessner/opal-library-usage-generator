package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.packageAndClassToJvmClassName
import org.opalj.ba.{CLASS, METHODS, PUBLIC}

object ClassGenerator {
  def generatePublicClass(packageName: String,
                          className: String,
                          methods: METHODS[_]
                         ): CLASS[_] = {

    val classTypeString = packageAndClassToJvmClassName(packageName, className)

    CLASS(
      accessModifiers = PUBLIC.SUPER,
      thisType = classTypeString,
      methods = methods
    )
  }
}
