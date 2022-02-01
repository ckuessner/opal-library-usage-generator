package de.ckuessner.opal.usagegen

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.packageAndClassToJvmClassName
import de.ckuessner.opal.usagegen.generators.ClassGenerator
import org.opalj.ba.{CLASS, METHODS}
import org.opalj.collection.immutable.RefArray

sealed trait GeneratedClass {
  def packageName: String

  def className: String

  def methods: Set[GeneratedMethod]

  def jvmClassName: String = packageAndClassToJvmClassName(packageName, className)

  def asClass: CLASS[_] = {
    val methods = METHODS(RefArray._UNSAFE_from(this.methods.map(_.methodBody).toArray))
    ClassGenerator.generatePublicClass(packageName, className, methods)
  }

}

case class SinkClass(packageName: String, className: String, sinkMethods: Set[SinkMethod]) extends GeneratedClass {
  def methods: Set[GeneratedMethod] = sinkMethods.toSet
}

case class CallerClass(packageName: String, className: String, callerMethods: Set[CallerMethod]) extends GeneratedClass {
  def methods: Set[GeneratedMethod] = callerMethods.toSet
}
