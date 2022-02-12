package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.FullMethodIdentifier
import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.{defaultValueForFieldType, tryCatchBlock}
import org.opalj.ba.CodeElement._
import org.opalj.ba.{CODE, CodeElement, METHOD, PUBLIC}
import org.opalj.br._
import org.opalj.br.instructions.{ALOAD, ALOAD_0, ASTORE, ASTORE_0, DUP, INVOKESPECIAL, INVOKESTATIC, NEW, RETURN}

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

    // Load default parameters onto stack, store reference types as locals to pass to sink after call
    methodBody ++= createDefaultParametersAndStoreReferenceTypesToLocalVariables(calledMethod.parameterTypes)

    // Call method of tested library
    val methodCallCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    methodCallCode += INVOKESTATIC(
      declaringClass = calledMethod.classFile.thisType,
      isInterface = false,
      name = calledMethod.name,
      methodDescriptor = calledMethod.descriptor
    )

    // The return value (if non-void) is on the stack after the method call
    methodCallCode ++= loadReferenceTypeParametersAndCallSinkAndReturn(calledMethod.parameterTypes, sinkId)

    // Handle Exception: pass everything to exception sink
    val exceptionHandlingCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]()
    exceptionHandlingCode += ASTORE_0
    exceptionHandlingCode += ALOAD_0
    exceptionHandlingCode ++= loadReferenceTypeParametersAndCallSinkAndReturn(calledMethod.parameterTypes, exceptionSinkId)

    methodBody ++= tryCatchBlock(
      methodCallCode.result(),
      exceptionHandlingCode.result(),
      Symbol("Method Call")
    )

    METHOD(
      PUBLIC.STATIC,
      callerMethodName,
      "()V",
      CODE(methodBody.result())
    )
  }

  /**
   * Generates the bytecode for constructing an object without calling a constructor
   *
   * @param callerMethodName The name of the generated method.
   * @param classFile        The classFile of the constructed object
   * @param sinkId           The identifier of the sink method.
   * @return The instructions of the caller method body
   */
  def generateAllocationWithoutCallingConstructorMethod(callerMethodName: String,
                                                        classFile: ClassFile,
                                                        sinkId: FullMethodIdentifier,
                                                        exceptionSinkId: FullMethodIdentifier
                                                       ): METHOD[_] = {

    val constructionCode = Array[CodeElement[Nothing]](
      // Only create object using new, the static initializer is called implicitly
      NEW(classFile.thisType),
      // Pass reference to sink
      INVOKESTATIC(
        sinkId.jvmClassName,
        isInterface = false,
        sinkId.methodName,
        sinkId.signature
      ),
      RETURN
    )

    val exceptionHandlerCode = Array[CodeElement[Nothing]](
      INVOKESTATIC(
        exceptionSinkId.jvmClassName,
        isInterface = false,
        exceptionSinkId.methodName,
        exceptionSinkId.signature
      ),
      RETURN
    )

    val methodBody = tryCatchBlock(constructionCode, exceptionHandlerCode, Symbol("static initializer exceptions"))

    METHOD(
      PUBLIC.STATIC,
      callerMethodName,
      "()V",
      CODE(methodBody)
    )
  }

  /**
   * Generates the bytecode for calling the specified instance method.
   *
   * @param callerMethodName  The name of the generated method.
   * @param calledConstructor The constructor method that is called by the generated method.
   * @param sinkId            The identifier of the sink method.
   * @param exceptionSinkId   The identifier of the exception sink method.
   * @return The instructions of the caller method body
   */
  def generateConstructorCallerMethod(callerMethodName: String,
                                      calledConstructor: Method,
                                      sinkId: FullMethodIdentifier,
                                      exceptionSinkId: FullMethodIdentifier
                                     ): METHOD[_] = {

    if (!calledConstructor.isConstructor) {
      throw new IllegalArgumentException(calledConstructor + " is not a constructor")
    }

    val methodBody = mutable.ArrayBuilder.make[CodeElement[Nothing]]

    methodBody += NEW(calledConstructor.classFile.thisType)
    methodBody += DUP
    methodBody ++= createDefaultParametersAndStoreReferenceTypesToLocalVariables(calledConstructor.parameterTypes)

    val callConstructorAndSinkCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    callConstructorAndSinkCode += INVOKESPECIAL(
      calledConstructor.classFile.thisType,
      isInterface = false,
      "<init>",
      calledConstructor.descriptor
    )

    callConstructorAndSinkCode += DUP

    // The reference to the constructed object is on the stack (because of DUP)
    // The reference type parameters need to be passed to sink -> load them.
    callConstructorAndSinkCode ++= loadReferenceTypeParametersAndCallSinkAndReturn(
      calledConstructor.parameterTypes,
      sinkId
    )

    // Handle Exception: pass everything to exception sink (including exception, but not uninitialized object)
    val exceptionHandlingCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    exceptionHandlingCode += ASTORE_0
    exceptionHandlingCode += ALOAD_0
    exceptionHandlingCode ++= loadReferenceTypeParametersAndCallSinkAndReturn(
      calledConstructor.parameterTypes,
      exceptionSinkId
    )

    methodBody ++= tryCatchBlock(
      callConstructorAndSinkCode.result(),
      exceptionHandlingCode.result(),
      Symbol("Constructor")
    )

    METHOD(
      PUBLIC.STATIC,
      callerMethodName,
      "()V",
      CODE(methodBody.result())
    )
  }

  private def loadReferenceTypeParametersAndCallSinkAndReturn(parameters: FieldTypes,
                                                              sinkId: FullMethodIdentifier,
                                                              localVariableIndexOffset: Int = 1
                                                             ): Array[CodeElement[Nothing]] = {
    val code = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    code ++= loadStoredReferenceTypeParametersFromLocalVariables(parameters, localVariableIndexOffset)
    code += INVOKESTATIC(
      sinkId.jvmClassName,
      isInterface = false,
      sinkId.methodName,
      sinkId.signature
    )
    code += RETURN
    code.result()
  }

  private def createDefaultParametersAndStoreReferenceTypesToLocalVariables(parameterList: FieldTypes,
                                                                            localVariableIndexOffset: Int = 1
                                                                           ): Seq[CodeElement[Nothing]] = {
    var localVarIndex = localVariableIndexOffset
    val methodBody = mutable.ArrayBuilder.make[CodeElement[Nothing]]()

    // Load/construct parameters for method call
    for (paramType <- parameterList) {
      paramType match {
        case _: ReferenceType =>
          methodBody ++= defaultValueForFieldType(paramType)
          methodBody += ASTORE(localVarIndex)
          methodBody += ALOAD(localVarIndex)
          localVarIndex += 1
        case _: BaseType =>
          // Place default value on stack (base types aren't constructed in prior step)
          methodBody ++= defaultValueForFieldType(paramType)
      }
    }

    methodBody.result()
  }

  private def loadStoredReferenceTypeParametersFromLocalVariables(parameterList: FieldTypes,
                                                                  localVariableIndexOffset: Int = 1
                                                                 ): Seq[CodeElement[Nothing]] = {
    parameterList
      .filter(parameter => parameter.isReferenceType)
      .zipWithIndex
      .map[CodeElement[Nothing]] { case (_: FieldType, localVariableIndex: Int) =>
        instructionToInstructionElement(ALOAD(localVariableIndex + localVariableIndexOffset))
      }
  }
}
