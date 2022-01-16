package de.ckuessner.opal.usagegen.generators

import org.opalj.ba.{CLASS, METHODS, PUBLIC}

object ClassGenerator {
  def generatePublicClass(packageName: Option[String],
                          className: String,
                          methods: METHODS[_]
                         ): CLASS[_] = {

    val classTypeString = packageName match {
      case Some(pkgName) => pkgName + "/" + className
      case None => className
    }

    CLASS(
      accessModifiers = PUBLIC.SUPER,
      thisType = classTypeString,
      methods = methods
    )
  }
}
