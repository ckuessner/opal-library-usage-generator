package de.ckuessner.opal.usagegen.generators.parameters

import de.ckuessner.opal.usagegen.analyses._
import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.{tryCatchBlock, unqualifiedNameIllegalCharsPattern}
import de.ckuessner.opal.usagegen.generators.parameters.InstanceProviderGenerator.instanceProviderMethodName
import de.ckuessner.opal.usagegen.{FullMethodIdentifier, InstanceProviderClass, InstanceProviderMethod}
import org.opalj.ba.{CODE, CodeElement, METHOD, PUBLIC}
import org.opalj.br.instructions.{ACONST_NULL, ARETURN, DUP, GETSTATIC, INVOKESPECIAL, INVOKESTATIC, NEW}
import org.opalj.br.{MethodDescriptor, ObjectType}
import org.opalj.collection.immutable.RefArray

import scala.collection.mutable

class InstanceProviderGenerator(private val parameterGenerator: ParameterGenerator,
                                private val providerClassBaseName: String
                               ) {

  private val methodsPerClass = 500

  def generateInstanceProviderClasses(instanceSources: Iterable[InstanceSource]): InstanceProviderClasses = {
    val generatedClasses = instanceSources.groupBy(_.sourcePackage).flatMap { case (packageName, instanceSources) =>
      val methods = instanceSources
        .zipWithIndex // To ensure that the provider method name is unique, a unique index is attached
        .map { case (instanceSource, counter) =>
          val providerClassName = s"$providerClassBaseName$$c${counter / methodsPerClass}"
          val providerMethodName = instanceProviderMethodName(instanceSource, counter)
          val providerMethodDescriptor = MethodDescriptor.withNoArgs(instanceSource.instanceType).toJVMDescriptor
          InstanceProviderMethod(
            FullMethodIdentifier(packageName, providerClassName, providerMethodName, providerMethodDescriptor),
            generateInstanceProviderMethod(providerMethodName, instanceSource),
            instanceSource
          )
        }

      methods.groupBy(_.methodId.simpleClassName)
        .map { case (providerClassName, methods) =>
          InstanceProviderClass(
            packageName, providerClassName, RefArray._UNSAFE_from(methods.toArray)
          )
        }
    }

    InstanceProviderClasses(RefArray._UNSAFE_from(generatedClasses.toArray))
  }

  def generateInstanceProviderMethod(providerMethodName: String, instanceSource: InstanceSource): METHOD[_] = {
    METHOD(
      PUBLIC.STATIC,
      providerMethodName,
      MethodDescriptor.withNoArgs(instanceSource.instanceType).toJVMDescriptor,
      CODE(generateInstanceCreationByteCode(instanceSource))
    )
  }

  private def generateInstanceCreationByteCode(instanceSource: InstanceSource): Array[CodeElement[Nothing]] = {
    val code: Array[CodeElement[Nothing]] = instanceSource match {
      case ConstructorInstanceSource(objectType, constructorMethod) =>
        val code = mutable.ArrayBuilder.make[CodeElement[Nothing]]()
        code += NEW(objectType)
        code += DUP
        code ++= parameterGenerator.generateAllParameters(constructorMethod)
        code += INVOKESPECIAL(
          objectType,
          isInterface = false,
          constructorMethod.name,
          constructorMethod.descriptor
        )
        code += ARETURN
        code.result()

      case StubSubclassInstanceSource(_, stubClass, constructorMethod) =>
        val code = mutable.ArrayBuilder.make[CodeElement[Nothing]]()
        code += NEW(stubClass.jvmClassName)
        code += DUP
        MethodDescriptor.apply(constructorMethod.methodId.descriptor).parameterTypes.foreach { paramType =>
          code ++= parameterGenerator.generateParameter(paramType)
        }
        code += INVOKESPECIAL(
          stubClass.jvmClassName,
          isInterface = false,
          constructorMethod.methodId.methodName,
          constructorMethod.methodId.descriptor,
        )
        code += ARETURN
        code.result()

      case StaticFieldInstanceSource(_, field) => Array(
        GETSTATIC(field.classFile.thisType, field.name, field.fieldType),
        ARETURN
      )

      case StaticMethodInstanceSource(_, staticMethod) =>
        val code = mutable.ArrayBuilder.make[CodeElement[Nothing]]()
        code ++= parameterGenerator.generateAllParameters(staticMethod)
        code += INVOKESTATIC(
          staticMethod.classFile.thisType,
          isInterface = false,
          staticMethod.name,
          staticMethod.descriptor
        )
        code += ARETURN
        code.result()
    }

    tryCatchBlock(code, Seq(ACONST_NULL, ARETURN), Symbol("exception in instance source"))
  }
}

object InstanceProviderGenerator {
  def apply(parameterGenerator: ParameterGenerator,
            providerClassName: String
           ): InstanceProviderGenerator = {
    new InstanceProviderGenerator(parameterGenerator, providerClassName)
  }

  private def instanceProviderMethodName(instanceSource: InstanceSource, uniqueIndex: Int): String = {
    val verb = instanceSource match {
      case ConstructorInstanceSource(_, _) => "using_constructor"
      case StaticFieldInstanceSource(_, _) => "using_field"
      case StaticMethodInstanceSource(_, _) => "using_method"
      case StubSubclassInstanceSource(_, _, _) => "using_stub_subclass"
    }

    val sanitizedClassName = unqualifiedNameIllegalCharsPattern.matcher(
      instanceSource.instanceType.simpleName.replace("?", "??")
    ).replaceAll("?")

    s"${verb}__${sanitizedClassName}__$uniqueIndex"
  }
}

case class InstanceProviderClasses(providerClasses: RefArray[InstanceProviderClass]) {
  lazy val typeToProviderMethodMap: Map[ObjectType, Seq[InstanceProviderMethod]] = {
    providerClasses
      .flatMap(instanceProviderClass => instanceProviderClass.instanceProviderMethods)
      .groupBy(_.instanceType)
  }
}
