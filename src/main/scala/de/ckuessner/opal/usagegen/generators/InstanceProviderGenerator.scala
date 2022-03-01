package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.analyses.{ConstructorInstanceSource, InstanceSource, StaticFieldInstanceSource, StaticMethodInstanceSource}
import de.ckuessner.opal.usagegen.generators.InstanceProviderGenerator.instanceProviderMethodName
import de.ckuessner.opal.usagegen.{FullMethodIdentifier, InstanceProviderClass, InstanceProviderMethod}
import org.opalj.ba.{CATCH, CODE, InstructionElement, METHOD, PUBLIC, TRY, TRYEND}
import org.opalj.br.instructions.{ACONST_NULL, ARETURN, DUP, GETSTATIC, INVOKESPECIAL, INVOKESTATIC, NEW}
import org.opalj.br.{FieldTypes, MethodDescriptor, ObjectType}
import org.opalj.collection.immutable.RefArray

import scala.collection.mutable

class InstanceProviderGenerator(private val parameterProvider: FieldTypes => Seq[InstructionElement],
                                private val providerClassName: String
                               ) {

  def generateInstanceProviderClasses(instanceSources: Iterable[InstanceSource]): InstanceProviderClasses = {
    val generatedClasses = instanceSources.groupBy(_.sourceClassFile.thisType.packageName) // One class per package
      .map { case (packageName, instanceSources) =>
        val methods = instanceSources
          .zipWithIndex // To ensure that the provider method name is unique, a unique index is attached
          .map { case (instanceSource, index) =>
            val providerMethodName = instanceProviderMethodName(instanceSource, index)
            val providerMethodDescriptor = MethodDescriptor.withNoArgs(instanceSource.instanceType).toJVMDescriptor
            InstanceProviderMethod(
              FullMethodIdentifier(packageName, providerClassName, providerMethodName, providerMethodDescriptor),
              generateInstanceProviderMethod(providerMethodName, instanceSource),
              instanceSource
            )
          }

        InstanceProviderClass(
          packageName, providerClassName, RefArray._UNSAFE_from(methods.toArray)
        )
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

  private def generateInstanceCreationByteCode(instanceSource: InstanceSource): Array[InstructionElement] = {
    instanceSource match {
      case ConstructorInstanceSource(objectType, constructorMethod) =>
        val code = mutable.ArrayBuilder.make[InstructionElement]()
        code += NEW(objectType)
        code += DUP
        code ++= parameterProvider(constructorMethod.parameterTypes)
        code += INVOKESPECIAL(
          objectType,
          isInterface = false,
          constructorMethod.name,
          constructorMethod.descriptor
        )
        code += ARETURN
        code.result()

      case StaticFieldInstanceSource(_, field) => Array(
        GETSTATIC(field.classFile.thisType, field.name, field.fieldType),
        ARETURN
      )

      case StaticMethodInstanceSource(_, staticMethod) =>
        val code = mutable.ArrayBuilder.make[InstructionElement]()

        val exceptionSymbol = Symbol("method call to instance source method")
        TRY(exceptionSymbol)
        code ++= parameterProvider(staticMethod.parameterTypes)
        code += INVOKESTATIC(
          staticMethod.classFile.thisType,
          isInterface = false,
          staticMethod.name,
          staticMethod.descriptor
        )
        code += ARETURN
        TRYEND(exceptionSymbol)

        CATCH(exceptionSymbol, 0)
        code += ACONST_NULL
        code += ARETURN

        code.result()
    }
  }
}

object InstanceProviderGenerator {
  def apply(parameterProvider: FieldTypes => Seq[InstructionElement],
            providerClassName: String
           ): InstanceProviderGenerator = {
    new InstanceProviderGenerator(parameterProvider, providerClassName)
  }

  private def instanceProviderMethodName(instanceSource: InstanceSource, uniqueIndex: Int): String = {
    val verb = instanceSource match {
      case ConstructorInstanceSource(_, _) => "using_constructor"
      case StaticFieldInstanceSource(_, _) => "using_field"
      case StaticMethodInstanceSource(_, _) => "using_method"
    }

    s"${verb}__${instanceSource.instanceType.simpleName}__$uniqueIndex"
  }
}

case class InstanceProviderClasses(providerClasses: RefArray[InstanceProviderClass]) {
  lazy val typeToProviderMethodMap: Map[ObjectType, Seq[InstanceProviderMethod]] = {
    providerClasses
      .flatMap(instanceProviderClass => instanceProviderClass.instanceProviderMethods)
      .groupBy(_.instanceType)
  }
}