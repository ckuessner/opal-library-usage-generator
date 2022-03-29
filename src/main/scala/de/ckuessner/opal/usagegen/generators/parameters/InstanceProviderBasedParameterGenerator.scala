package de.ckuessner.opal.usagegen.generators.parameters

import de.ckuessner.opal.usagegen._
import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.unqualifiedNameIllegalCharsPattern
import de.ckuessner.opal.usagegen.generators.parameters.DefaultValueGenerator.defaultValueForFieldType
import de.ckuessner.opal.usagegen.generators.parameters.ParameterGenerator.selectFirstNonNullParameterFromInstanceProviderMethods
import org.opalj.ba.{CODE, CodeElement, METHOD, PUBLIC}
import org.opalj.br.instructions.INVOKESTATIC
import org.opalj.br.{FieldType, Method, MethodDescriptor, ObjectType}
import org.opalj.collection.immutable.RefArray

class InstanceProviderBasedParameterGenerator(val parameterGeneratorPackageName: String,
                                              val parameterGeneratorClassBaseName: String,
                                              providers: Map[ObjectType, Seq[InstanceProviderMethod]]
                                             ) extends ParameterGenerator {

  // TODO: This number could be tweaked
  // This is set so low as to avoid contant pool overflows
  private val providersPerClass: Int = 10

  private val typeToProviderMethodMap: Map[ObjectType, AuxiliaryMethod] = {
    providers.zipWithIndex.map { case ((objectType, providerMethods), generatedMethodCounter) =>
      val classNumber = generatedMethodCounter / providersPerClass
      val className = s"$parameterGeneratorClassBaseName$$c$classNumber"
      objectType -> generateParameterSelectionMethod(objectType, providerMethods, parameterGeneratorPackageName, className)
    }
  }

  private val preferParameterlessSourcesOrdering: Ordering[InstanceProviderMethod] =
    (x: InstanceProviderMethod, y: InstanceProviderMethod) =>
      if (x.instanceSource.isInstanceSourceParameterless == y.instanceSource.isInstanceSourceParameterless) 0
      else if (x.instanceSource.isInstanceSourceParameterless && y.instanceSource.isInstanceSourceParameterless) -1
      else 1


  override val generatedClasses: Iterable[GeneratedClass] =
    typeToProviderMethodMap
      .values
      .groupBy(_.methodId.simpleClassName)
      .map { case (className, methods) =>
        AuxiliaryClass(parameterGeneratorPackageName, className,
          RefArray._UNSAFE_from(methods.toArray)
        )
      }

  private def methodNameForType(objectType: ObjectType): String =
    unqualifiedNameIllegalCharsPattern.matcher(objectType.fqn).replaceAll("_")

  private def generateParameterSelectionMethod(objectType: ObjectType,
                                               providerMethods: Seq[InstanceProviderMethod],
                                               packageName: String,
                                               className: String
                                              ): AuxiliaryMethod = {

    val methodName = methodNameForType(objectType)
    val signature = MethodDescriptor.withNoArgs(objectType).toJVMDescriptor
    AuxiliaryMethod(
      FullMethodIdentifier(packageName, className, methodName, signature),
      METHOD(
        PUBLIC.STATIC,
        methodName,
        signature,
        CODE(
          selectFirstNonNullParameterFromInstanceProviderMethods(
            providerMethods.sortWith(
              (x: InstanceProviderMethod, y: InstanceProviderMethod) =>
                x.instanceSource.isInstanceSourceParameterless && !y.instanceSource.isInstanceSourceParameterless
            ).take(100), // TODO: This is probably not the best strategy, but avoids huge methods
            // TODO: It would is also more performant to take 100 before sorting
            defaultValueForFieldType(objectType)
          )
        )
      )
    )
  }

  override def generateParameter(parameterType: FieldType, parameterIndex: Int, calledMethod: Method): Array[CodeElement[Nothing]] = {
    generateParameter(parameterType)
  }

  override def generateParameter(parameterType: FieldType): Array[CodeElement[Nothing]] = {
    if (parameterType.isObjectType) {
      typeToProviderMethodMap.get(parameterType.asObjectType) match {
        case Some(providerMethod) =>
          return Array(
            INVOKESTATIC(
              providerMethod.methodId.jvmClassName,
              isInterface = false,
              methodNameForType(parameterType.asObjectType),
              MethodDescriptor.withNoArgs(parameterType.asObjectType).toJVMDescriptor
            ))
        case None => System.err.println("InstanceProviderBasedParameterGenerator: " + parameterType + " is requested but not known")
      }
    }

    defaultValueForFieldType(parameterType)
  }
}
