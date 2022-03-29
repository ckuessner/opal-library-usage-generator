package de.ckuessner.opal.usagegen.generators.classes

import de.ckuessner.opal.usagegen.{ConcreteStubMethod, ConcreteSubclass, ConstructorMethod, FullMethodIdentifier}
import org.opalj.ba.{CODE, CodeElement, METHOD, PUBLIC}
import org.opalj.br.instructions.{ALOAD, ALOAD_0, INVOKESPECIAL, RETURN}
import org.opalj.br.{ClassFile, FieldType, Method}

import scala.collection.mutable

/**
 * Generator for concrete subclasses of abstract classes.
 */
object ConcreteSubclassGenerator {
  def generateConcreteSubclass(abstractClass: ClassFile): Option[ConcreteSubclass] = {
    // If the abstractClass doesn't have any non-private constructors, no subclass can be generated
    if (abstractClass.constructors.forall(_.isPrivate)) {
      return None
    }

    val destinationPackage = abstractClass.thisType.packageName
    val generatedClassName = abstractClass.thisType.simpleName + "___GENERATED_CONCRETE_SUBCLASS"

    val constructorMethods = abstractClass.methods.filter(_.isConstructor).map(
      superConstructor => generateConstructorMethod(destinationPackage, generatedClassName, superConstructor)
    )

    // Override abstract methods with stubs
    val overriddenMethods = abstractClass.methods.filter(_.isAbstract)
      .map(abstractMethod => ConcreteStubMethod(
        FullMethodIdentifier(
          destinationPackage, generatedClassName, abstractMethod.name, abstractMethod.descriptor.toJVMDescriptor
        )))

    Some(ConcreteSubclass(
      destinationPackage,
      generatedClassName,
      abstractClass,
      overriddenMethods,
      constructorMethods
    ))
  }

  private def generateConstructorMethod(packageName: String, className: String, superConstructor: Method): ConstructorMethod = {
    // Call super constructor passing all parameters
    val instructions = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    instructions += ALOAD_0 // Load reference to this (this as in self reference)
    superConstructor.parameterTypes.foreachWithIndex {
      case (_: FieldType, index: Int) => instructions += ALOAD.canonicalRepresentation(index + 1)
    }
    instructions += INVOKESPECIAL(superConstructor.classFile.thisType, isInterface = false, "<init>", superConstructor.descriptor)
    instructions += RETURN

    ConstructorMethod(
      FullMethodIdentifier(packageName, className, "<init>", superConstructor.descriptor.toJVMDescriptor),
      METHOD(
        PUBLIC,
        "<init>",
        superConstructor.descriptor.toJVMDescriptor,
        CODE(instructions.result())
      )
    )
  }
}
