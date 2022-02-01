package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.FullMethodIdentifier
import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.{defaultValueForFieldType, storeInstruction}
import org.opalj.ba.CodeElement._
import org.opalj.ba.{CATCH, CODE, CodeElement, InstructionElement, METHOD, PUBLIC, TRY, TRYEND}
import org.opalj.br._
import org.opalj.br.instructions.{ALOAD, INVOKESTATIC, RETURN}

import scala.collection.mutable

object MethodCallGenerator {
  /**
   * Generates the bytecode for calling the specified _static_ method.
   *
   * @param callerMethodName The name of the generated method.
   * @param calledMethod     The method that is called by the generated method.
   * @param sinkId           The identifier of the sink method.
   * @param exceptionSinkId  The identifier of the exception sink method.
   * @return The instructions of the caller method body
   */
  def generateStaticMethodCallerMethod(callerMethodName: String,
                                       calledMethod: Method,
                                       sinkId: FullMethodIdentifier,
                                       exceptionSinkId: FullMethodIdentifier): METHOD[_] = {

    // Check if really static, (non-abstract is implied by static)
    if (calledMethod.isNotStatic) {
      throw new IllegalArgumentException("method to call must be static")
    }

    val methodBody = mutable.ArrayBuilder.make[CodeElement[Nothing]]

    // Reference types need to be stored as locals, as they are passed to sink after consumption by method call.
    // This means that we construct the default values for reference types first and store them as locals.
    calledMethod.descriptor.parameterTypes
      .filter(parameter => parameter.isReferenceType)
      .foreachWithIndex { case (paramType, localVarIndex) =>
        // Construct default value
        methodBody ++= defaultValueForFieldType(paramType).map(InstructionElement)
        // Store constructed value
        methodBody += storeInstruction(paramType, localVarIndex)
      }

    // Load/construct parameters for method call
    {
      var localVarIndex = 0 // Stores the next index for a reference type parameter
      for (paramType <- calledMethod.descriptor.parameterTypes) {
        paramType match {
          case _: ReferenceType =>
            // Load parameter (since it's a reference type and was already constructed in the prior step)
            methodBody += ALOAD(localVarIndex)
            localVarIndex += 1
          case _: BaseType =>
            // Place default value on stack (base types aren't constructed in prior step)
            methodBody ++= defaultValueForFieldType(paramType).map(InstructionElement)
        }
      }
    }

    // Surround method call in try-catch
    val methodCallExceptionSymbol = Symbol("Library Method Call")
    methodBody += TRY(methodCallExceptionSymbol)

    // Call method of tested library
    methodBody += INVOKESTATIC(
      declaringClass = calledMethod.classFile.thisType,
      isInterface = false,
      name = calledMethod.name,
      methodDescriptor = calledMethod.descriptor
    )

    // The return value (if non-void) is on the stack after the method call

    // After execution of the above method call, the reference type parameters need to be passed to sink -> load them.
    methodBody ++= loadStoredReferenceTypeParametersFromLocalVariables(calledMethod.descriptor.parameterTypes)

    // Call sink, passing return value of called method and all pass-by-reference parameters
    val sinkMethodSignature = sinkId.signature
    methodBody += INVOKESTATIC(sinkId.jvmClassName, isInterface = false, sinkId.methodName, sinkMethodSignature)

    // Simply return, the stack should now be empty
    methodBody += RETURN

    // Method call done, end try "block"
    methodBody += TRYEND(methodCallExceptionSymbol)

    // Handle Exception: pass everything to exception sink
    methodBody += CATCH(methodCallExceptionSymbol, 0)
    methodBody ++= loadStoredReferenceTypeParametersFromLocalVariables(calledMethod.descriptor.parameterTypes)

    methodBody += INVOKESTATIC(
      exceptionSinkId.jvmClassName,
      isInterface = false,
      exceptionSinkId.methodName,
      exceptionSinkId.signature
    )

    methodBody += RETURN

    METHOD(
      PUBLIC.STATIC,
      callerMethodName,
      "()V",
      CODE(methodBody.result())
    )
  }

  private def loadStoredReferenceTypeParametersFromLocalVariables(parameterList: FieldTypes,
                                                                  localVariableOffset: Int = 0
                                                                 ): Seq[CodeElement[Nothing]] = {
    parameterList
      .filter(parameter => parameter.isReferenceType)
      .zipWithIndex
      .map[CodeElement[Nothing]] { case (_: FieldType, localVarIndex: Int) =>
        instructionToInstructionElement(ALOAD(localVarIndex + localVariableOffset))
      }
  }
}
