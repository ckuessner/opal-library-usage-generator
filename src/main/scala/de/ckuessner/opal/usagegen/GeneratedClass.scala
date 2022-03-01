package de.ckuessner.opal.usagegen

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.packageAndClassToJvmClassName
import de.ckuessner.opal.usagegen.generators.ClassGenerator
import org.opalj.ba.{CLASS, METHODS}
import org.opalj.collection.immutable.RefArray

sealed trait GeneratedClass {
  def packageName: String

  def className: String

  def methods: RefArray[GeneratedMethod]

  def jvmClassName: String = packageAndClassToJvmClassName(packageName, className)

  def asClass: CLASS[_] = {
    val methods = METHODS(RefArray._UNSAFE_from(this.methods.map(_.methodBody).toArray))
    ClassGenerator.generatePublicClass(packageName, className, methods)
  }

}

case class SinkClass(packageName: String, className: String, sinkMethods: RefArray[SinkMethod]) extends GeneratedClass {
  def methods: RefArray[GeneratedMethod] = sinkMethods
}

case class CallerClass(packageName: String, className: String, callerMethods: RefArray[CallerMethod]) extends GeneratedClass {
  def methods: RefArray[GeneratedMethod] = callerMethods
}

case class InstanceProviderClass(packageName: String, className: String, instanceProviderMethods: RefArray[InstanceProviderMethod]) extends GeneratedClass {
  def methods: RefArray[GeneratedMethod] = instanceProviderMethods
}