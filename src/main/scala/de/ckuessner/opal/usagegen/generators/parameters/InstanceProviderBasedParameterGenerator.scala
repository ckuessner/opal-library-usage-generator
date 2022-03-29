package de.ckuessner.opal.usagegen.generators.parameters

import de.ckuessner.opal.usagegen._
import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.{packageAndClassToJvmClassName, unqualifiedNameIllegalCharsPattern}
import de.ckuessner.opal.usagegen.generators.parameters.DefaultValueGenerator.defaultValueForFieldType
import de.ckuessner.opal.usagegen.generators.parameters.ParameterGenerator.selectFirstNonNullParameterFromInstanceProviderMethods
import org.opalj.ba.{CODE, CodeElement, METHOD, PUBLIC}
import org.opalj.br.instructions.{ACONST_NULL, INVOKESTATIC}
import org.opalj.br.{FieldType, Method, MethodDescriptor, ObjectType}
import org.opalj.collection.immutable.RefArray

class InstanceProviderBasedParameterGenerator(val packageName: String,
                                              val className: String,
                                              providers: Map[ObjectType, Seq[InstanceProviderMethod]]) extends ParameterGenerator {

  private val preferParameterlessSourcesOrdering: Ordering[InstanceProviderMethod] =
    (x: InstanceProviderMethod, y: InstanceProviderMethod) =>
      if (x.instanceSource.isInstanceSourceParameterless == y.instanceSource.isInstanceSourceParameterless) 0
      else if (x.instanceSource.isInstanceSourceParameterless && y.instanceSource.isInstanceSourceParameterless) -1
      else 1

  override val generatedClasses: Seq[GeneratedClass] = Seq(AuxiliaryClass(
    packageName, className,
    RefArray._UNSAFE_from(
      providers.map { case (objectType: ObjectType, instanceProviderMethods) =>
        generateParameterSelectionMethod(objectType, instanceProviderMethods)
      }.toArray
    )))

  private def methodNameForType(objectType: ObjectType): String =
    unqualifiedNameIllegalCharsPattern.matcher(objectType.toJVMTypeName).replaceAll("_")

  private def generateParameterSelectionMethod(objectType: ObjectType, providerMethods: Seq[InstanceProviderMethod]): AuxiliaryMethod = {
    val methodName = methodNameForType(objectType)
    val signature = MethodDescriptor.withNoArgs(objectType).toJVMDescriptor
    AuxiliaryMethod(
      FullMethodIdentifier(packageName, className, methodName, signature),
      METHOD(
        PUBLIC.STATIC,
        methodName,
        signature,
        CODE(
          selectFirstNonNullParameterFromInstanceProviderMethods(providerMethods.sorted(preferParameterlessSourcesOrdering))
        )
      )
    )
  }

  override def generateParameter(parameterType: FieldType, parameterIndex: Int, calledMethod: Method): Array[CodeElement[Nothing]] = {
    generateParameter(parameterType)
  }

  override def generateParameter(parameterType: FieldType): Array[CodeElement[Nothing]] = {
    val defaultValue: Array[CodeElement[Nothing]] = defaultValueForFieldType(parameterType)

    if (parameterType.isObjectType) {
      val methodCall = INVOKESTATIC(
        packageAndClassToJvmClassName(packageName, className),
        isInterface = false,
        methodNameForType(parameterType.asObjectType),
        MethodDescriptor.withNoArgs(parameterType.asObjectType).toJVMDescriptor
      )
      if (!defaultValue(0).equals(CodeElement.instructionToInstructionElement(ACONST_NULL))) {
        val bytecode = new Array[CodeElement[Nothing]](defaultValue.length + 1)
        bytecode(0) = methodCall
        defaultValue.copyToArray(bytecode, 1)
        return bytecode
      } else {
        return Array(methodCall)
      }
    }

    defaultValue
  }
}
