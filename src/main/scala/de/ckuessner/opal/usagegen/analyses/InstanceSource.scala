package de.ckuessner.opal.usagegen.analyses

import de.ckuessner.opal.usagegen.{ConcreteSubclass, ConstructorMethod}
import org.opalj.br.{ClassFile, Field, Method, ObjectType}

sealed trait InstanceSource {
  def instanceType: ObjectType

  def isInstanceSourceParameterless: Boolean

  def sourcePackage: String
}

case class ConstructorInstanceSource(instanceType: ObjectType, constructorMethod: Method) extends InstanceSource {
  override def isInstanceSourceParameterless: Boolean = constructorMethod.parameterTypes.isEmpty

  def sourceClassFile: ClassFile = constructorMethod.classFile

  override def sourcePackage: String = sourceClassFile.thisType.packageName
}

case class StaticFieldInstanceSource(instanceType: ObjectType, field: Field) extends InstanceSource {
  override def isInstanceSourceParameterless: Boolean = true

  def sourceClassFile: ClassFile = field.classFile

  override def sourcePackage: String = sourceClassFile.thisType.packageName
}

case class StaticMethodInstanceSource(instanceType: ObjectType, staticMethod: Method) extends InstanceSource {
  override def isInstanceSourceParameterless: Boolean = staticMethod.parameterTypes.isEmpty

  def sourceClassFile: ClassFile = staticMethod.classFile

  override def sourcePackage: String = sourceClassFile.thisType.packageName
}

case class StubSubclassInstanceSource(instanceType: ObjectType,
                                      stubClass: ConcreteSubclass,
                                      stubClassConstructor: ConstructorMethod) extends InstanceSource {

  override def isInstanceSourceParameterless: Boolean = stubClassConstructor.methodId.descriptor.equals("()V")

  override def sourcePackage: String = stubClass.packageName
}

