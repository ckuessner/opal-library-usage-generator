package de.ckuessner.opal.usagegen.generators.parameters

import de.ckuessner.opal.usagegen._
import org.opalj.ba.{CodeElement, LabelElement}
import org.opalj.br.instructions.{ACONST_NULL, ALOAD_0, ARETURN, ASTORE_0, IFNONNULL, INVOKESTATIC}
import org.opalj.br.{FieldType, Method, ObjectType}

import scala.collection.mutable

trait ParameterGenerator {

  /**
   * These are the classes that are generated by the parameter generator itself.
   *
   * If none are generated, this is Seq.empty
   *
   * @return The generated classes
   */
  def generatedClasses: Iterable[GeneratedClass] = Seq.empty

  /**
   * Generate code that creates all parameters for a method call.
   *
   * @param calledMethod The method that is called with the parameters provided by this method.
   * @return
   */
  def generateAllParameters(calledMethod: Method): Array[CodeElement[Nothing]] = {
    val code = mutable.ArrayBuilder.make[CodeElement[Nothing]]()
    calledMethod.parameterTypes.foreachWithIndex {
      case (parameterType: FieldType, index: Int) => code ++= generateParameter(parameterType, index, calledMethod)
    }
    code.result()
  }

  /**
   * Generate code that creates a parameter for a method call.
   *
   * NOTE: The returned bytecode is expected not to use any local variables!
   *
   * The parameterIndex and calledMethod can be used for context aware parameter generation.
   *
   * @param parameterType  The type of the generated parameter
   * @param parameterIndex The index of the parameter in the methods parameter list
   * @param calledMethod   The method that is called with the parameter provided by this method.
   * @return Bytecode instructions that generate the parameter at runtime
   */
  def generateParameter(parameterType: FieldType, parameterIndex: Int, calledMethod: Method): Array[CodeElement[Nothing]] = {
    // Simply use the method that doesn't use any information from the called method.
    // This method can be used to generate parameters in a context aware manner.
    generateParameter(parameterType)
  }

  /**
   * Generate code that creates a parameter for an unspecified method call.
   *
   * NOTE: The returned bytecode is expected not to use any local variables!
   *
   * @param parameterType The type of the generated parameter
   * @return Bytecode instructions that generate the parameter at runtime
   */
  def generateParameter(parameterType: FieldType): Array[CodeElement[Nothing]]

  /**
   * Generate instance for instance method call.
   *
   * If no instance can be generated, this simply pushes null onto the stack.
   *
   * NOTE: The returned bytecode is expected not to use any local variables.
   *
   * @param objectType The type of the generated instance
   * @return The bytecode that generates an object of the provided object type
   *
   */
  def generateInstance(objectType: ObjectType): Array[CodeElement[Nothing]] = {
    generateParameter(objectType)
  }

  /**
   * Generate instance for instance method call.
   *
   * If no instance can be generated, this simply pushes null onto the stack.
   * The calledInstanceMethod parameter can be used for context aware instance creation
   *
   * NOTE: The returned bytecode is expected not to use any local variables.
   *
   * @param objectType           The type of the generated instance
   * @param calledInstanceMethod The method that will be called on the generated instance
   * @return The bytecode that generates an object of the provided object type
   *
   */
  def generateInstance(objectType: ObjectType, calledInstanceMethod: Method): Array[CodeElement[Nothing]] = {
    generateParameter(objectType)
  }
}

object ParameterGenerator {
  def selectFirstNonNullParameterFromInstanceProviderMethods(instanceProviderMethods: Seq[InstanceProviderMethod],
                                                             fallbackValueInstructions: Array[CodeElement[Nothing]]
                                                            ): Array[CodeElement[Nothing]] = {

    // Call instance provider methods until we have a non-null reference
    val methodBody = mutable.ArrayBuilder.make[CodeElement[Nothing]]

    val returnInstanceSymbol = Symbol("RETURN INSTANCE")
    methodBody += ACONST_NULL
    methodBody += ASTORE_0
    instanceProviderMethods.foreach { instanceProviderMethod =>
      methodBody += INVOKESTATIC(
        instanceProviderMethod.methodId.jvmClassName,
        isInterface = false,
        instanceProviderMethod.methodId.methodName,
        instanceProviderMethod.methodId.descriptor
      )
      methodBody += ASTORE_0
      methodBody += ALOAD_0
      methodBody += IFNONNULL(returnInstanceSymbol) // Return value, if non null
    }

    // Store fallback value in local var 0
    methodBody ++= fallbackValueInstructions
    methodBody += ASTORE_0

    methodBody += LabelElement(returnInstanceSymbol)
    methodBody += ALOAD_0 // This might be null
    methodBody += ARETURN

    return methodBody.result()
  }
}
