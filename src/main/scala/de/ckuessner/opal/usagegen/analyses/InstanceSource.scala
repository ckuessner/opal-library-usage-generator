package de.ckuessner.opal.usagegen.analyses

import org.opalj.br
import org.opalj.br.{ClassFile, Field, Method, ObjectType}

sealed trait InstanceSource {
  def instanceType: ObjectType

  def isParameterless: Boolean

  def sourceClassFile: br.ClassFile
}

case class ConstructorInstanceSource(instanceType: ObjectType, constructorMethod: Method) extends InstanceSource {
  override def isParameterless: Boolean = constructorMethod.parameterTypes.isEmpty

  override def sourceClassFile: ClassFile = constructorMethod.classFile
}

case class StaticFieldInstanceSource(instanceType: ObjectType, field: Field) extends InstanceSource {
  override def isParameterless: Boolean = true

  override def sourceClassFile: ClassFile = field.classFile
}

case class StaticMethodInstanceSource(instanceType: ObjectType, staticMethod: Method) extends InstanceSource {
  override def isParameterless: Boolean = staticMethod.parameterTypes.isEmpty

  override def sourceClassFile: ClassFile = staticMethod.classFile
}

