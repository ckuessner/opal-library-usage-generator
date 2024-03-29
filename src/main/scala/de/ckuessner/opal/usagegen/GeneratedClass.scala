package de.ckuessner.opal.usagegen

import de.ckuessner.opal.usagegen.analyses.{InstanceSource, StubSubclassInstanceSource}
import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.packageAndClassToJvmClassName
import de.ckuessner.opal.usagegen.generators.classes.{ClassGenerator, EntryPointClassGenerator}
import org.opalj.ba.{CLASS, METHODS, PUBLIC}
import org.opalj.br.ClassFile
import org.opalj.collection.immutable.RefArray

sealed trait GeneratedClass {
  def packageName: String

  def className: String

  def methods: RefArray[GeneratedMethod]

  def jvmClassName: String = packageAndClassToJvmClassName(packageName, className)

  def asCLASS: CLASS[_] = {
    val methods = METHODS(RefArray._UNSAFE_from(this.methods.map(_.methodBody).toArray))
    ClassGenerator.generatePublicClass(packageName, className, methods)
  }

  def instanceSources: Seq[InstanceSource] = Seq.empty
}

case class SinkClass(packageName: String, className: String, sinkMethods: RefArray[SinkMethod]) extends GeneratedClass {
  def methods: RefArray[GeneratedMethod] = sinkMethods
}

case class EntryPointClass(packageName: String, className: String, mainMethod: AuxiliaryMethod, entryPointMethod: EntryPointMethod) extends GeneratedClass {
  override def methods: RefArray[GeneratedMethod] = RefArray(mainMethod, entryPointMethod)
}

case class CallerClass(packageName: String, className: String, callerMethods: RefArray[CallerMethod], entryPointMethod: EntryPointMethod) extends GeneratedClass {
  def methods: RefArray[GeneratedMethod] = RefArray[GeneratedMethod](entryPointMethod) ++ callerMethods
}

object CallerClass {
  def apply(packageName: String, className: String, callerMethods: RefArray[CallerMethod]): CallerClass = {
    CallerClass(
      packageName, className, callerMethods,
      EntryPointClassGenerator.generateCallerClassEntryPointMethod(packageName, className, callerMethods)
    )
  }
}

case class InstanceProviderClass(packageName: String,
                                 className: String,
                                 instanceProviderMethods: RefArray[InstanceProviderMethod]
                                ) extends GeneratedClass {

  def methods: RefArray[GeneratedMethod] = instanceProviderMethods
}

case class ConcreteSubclass(packageName: String,
                            className: String,
                            abstractSuperClass: ClassFile,
                            concreteStubMethods: RefArray[GeneratedMethod],
                            constructorMethods: RefArray[ConstructorMethod]
                           ) extends GeneratedClass {

  override def asCLASS: CLASS[_] = {
    val methods: METHODS[_] = METHODS(RefArray._UNSAFE_from(this.methods.map(_.methodBody).toArray))
    val classTypeString = packageAndClassToJvmClassName(packageName, className)

    CLASS(
      accessModifiers = PUBLIC.SUPER,
      thisType = classTypeString,
      methods = methods,
      superclassType =
        if (abstractSuperClass.isInterfaceDeclaration) Some("java/lang/Object")
        else Some(abstractSuperClass.thisType.fqn),
      interfaceTypes =
        if (abstractSuperClass.isInterfaceDeclaration) RefArray(abstractSuperClass.thisType.fqn)
        else RefArray.empty
    )
  }

  override def methods: RefArray[GeneratedMethod] = concreteStubMethods ++ constructorMethods

  override def instanceSources: RefArray[StubSubclassInstanceSource] = {
    // This includes the constructors of the concrete subclass
    // This doesn't include any methods and fields
    // (although if non-static methods were considered in InstanceSearcher this would make sense)
    constructorMethods.map(constructorMethod =>
      StubSubclassInstanceSource(abstractSuperClass.thisType, this, constructorMethod)
    )
  }

}

case class AuxiliaryClass(packageName: String, className: String, methods: RefArray[GeneratedMethod]) extends GeneratedClass