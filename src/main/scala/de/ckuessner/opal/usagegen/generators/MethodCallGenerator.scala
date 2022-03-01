package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.tryCatchBlock
import de.ckuessner.opal.usagegen.generators.DefaultValueLoadingGenerator.defaultValueForFieldType
import de.ckuessner.opal.usagegen.{FullMethodIdentifier, InstanceProviderMethod}
import org.opalj.ba.CodeElement._
import org.opalj.ba.{CODE, CodeElement, LabelElement, METHOD, PUBLIC}
import org.opalj.br._
import org.opalj.br.instructions.{ALOAD, ALOAD_0, ALOAD_1, ASTORE, ASTORE_0, ASTORE_1, DUP, IFNONNULL, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, LoadLocalVariableInstruction, NEW, RETURN, StoreLocalVariableInstruction}
import org.opalj.log.{GlobalLogContext, OPALLogger}

import scala.collection.mutable

class MethodCallGenerator(private val instanceProviderMap: Map[ObjectType, Seq[InstanceProviderMethod]]) {
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
    methodBody ++= createDefaultParametersAndStoreReferenceTypesToLocalVariables(calledMethod.parameterTypes, 1)

    // Call method of tested library
    val methodCallCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    methodCallCode += INVOKESTATIC(
      declaringClass = calledMethod.classFile.thisType,
      isInterface = false,
      name = calledMethod.name,
      methodDescriptor = calledMethod.descriptor
    )

    // The return value (if non-void) is on the stack after the method call
    methodCallCode ++= loadStoredReferenceTypeParametersAndCallSinkAndReturn(calledMethod.parameterTypes, sinkId, 1)

    // Handle Exception: pass everything to exception sink
    val exceptionHandlingCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]()
    exceptionHandlingCode += ASTORE_0
    exceptionHandlingCode += ALOAD_0
    exceptionHandlingCode ++= loadStoredReferenceTypeParametersAndCallSinkAndReturn(calledMethod.parameterTypes, exceptionSinkId, 1)

    methodBody ++= tryCatchBlock(
      methodCallCode.result(),
      exceptionHandlingCode.result(),
      Symbol("method call exception")
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

    val methodBody = tryCatchBlock(constructionCode, exceptionHandlerCode, Symbol("static initializer exception"))

    METHOD(
      PUBLIC.STATIC,
      callerMethodName,
      "()V",
      CODE(methodBody)
    )
  }

  /**
   * Generates the bytecode for calling the specified constructor method.
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
    methodBody ++= createDefaultParametersAndStoreReferenceTypesToLocalVariables(
      calledConstructor.parameterTypes,
      localVariableIndexOffset = 1
    )

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
    callConstructorAndSinkCode ++= loadStoredReferenceTypeParametersAndCallSinkAndReturn(
      calledConstructor.parameterTypes,
      sinkId,
      localVariableIndexOffset = 1
    )

    // Handle Exception: pass everything to exception sink (including exception, but not uninitialized object)
    val exceptionHandlingCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    exceptionHandlingCode += ASTORE_0
    exceptionHandlingCode += ALOAD_0
    exceptionHandlingCode ++= loadStoredReferenceTypeParametersAndCallSinkAndReturn(
      calledConstructor.parameterTypes,
      exceptionSinkId,
      localVariableIndexOffset = 1
    )

    methodBody ++= tryCatchBlock(
      callConstructorAndSinkCode.result(),
      exceptionHandlingCode.result(),
      Symbol("constructor exception")
    )

    METHOD(
      PUBLIC.STATIC,
      callerMethodName,
      "()V",
      CODE(methodBody.result())
    )
  }

  /**
   * Generates the bytecode for calling the specified instance method.
   *
   * @param callerMethodName The name of the generated method.
   * @param instanceMethod   The constructor method that is called by the generated method.
   * @param sinkId           The identifier of the sink method.
   * @param exceptionSinkId  The identifier of the exception sink method.
   * @return The instructions of the caller method body
   */
  def generateInstanceMethodCallerMethod(callerMethodName: String,
                                         instanceMethod: Method,
                                         sinkId: FullMethodIdentifier,
                                         exceptionSinkId: FullMethodIdentifier
                                        ): METHOD[_] = {

    if (instanceMethod.isStatic) {
      throw new IllegalArgumentException(instanceMethod + " is not an instance method")
    }

    val methodBody = mutable.ArrayBuilder.make[CodeElement[Nothing]]

    // Call instance provider methods until we have a non-null reference to call the instance method on
    val objectType = instanceMethod.classFile.thisType
    val instanceProviders = instanceProviderMap.get(objectType)
    if (instanceProviders.isEmpty || instanceProviders.get.isEmpty) {
      OPALLogger.warn("InstanceMethodCallGeneration", "No runtime instance providers found for " + objectType)(GlobalLogContext)
    }

    val methodCallBeginLabel = Symbol("method call begin")
    instanceProviders.get.foreach { instanceProviderMethod =>
      methodBody += INVOKESTATIC(
        instanceProviderMethod.methodId.jvmClassName,
        isInterface = false,
        instanceProviderMethod.methodId.methodName,
        instanceProviderMethod.methodId.signature
      )
      methodBody += ASTORE_0
      methodBody += ALOAD_0
      methodBody += IFNONNULL(methodCallBeginLabel) // Jump to method call, if we have a reference
    }
    methodBody += RETURN // We don't have an instance, so we cannot call this instance method
    // TODO: Add logging? -> We could log that the instance method wasn't called, when we don't get a non-null reference to the instance through one of the providers

    // Start of the instance method call code
    methodBody += LabelElement(methodCallBeginLabel)
    // Get reference to callee object as first parameter to sink
    methodBody += ALOAD_0
    // Load parameters for instance method  (starting at local variable 2 because we need to store reference to callee object (in 0) and exception (in 1))
    methodBody ++= createDefaultParametersAndStoreReferenceTypesToLocalVariables(instanceMethod.parameterTypes, localVariableIndexOffset = 2)

    val instanceMethodAndSinkCall = mutable.ArrayBuilder.make[CodeElement[Nothing]]

    // Invoke instance method
    // Call instance method on callee object
    instanceMethodAndSinkCall += INVOKEVIRTUAL(instanceMethod.classFile.thisType, instanceMethod.name, instanceMethod.descriptor)
    // Load return value as second parameter for sink (if exists)
    if (!instanceMethod.descriptor.returnType.isVoidType) {
      instanceMethodAndSinkCall += StoreLocalVariableInstruction(instanceMethod.descriptor.returnType.asFieldType, 1)
      instanceMethodAndSinkCall += ALOAD_0
      instanceMethodAndSinkCall += LoadLocalVariableInstruction(instanceMethod.descriptor.returnType.asFieldType, 1)
    } else {
      // Load reference of callee object in order to pass it to sink
      instanceMethodAndSinkCall += ALOAD_0
    }
    // Load references of object type parameters, pass callee reference and the loaded references to sink, return
    instanceMethodAndSinkCall ++= loadStoredReferenceTypeParametersAndCallSinkAndReturn(instanceMethod.parameterTypes, sinkId, localVariableIndexOffset = 2)

    // Handle Exception: pass everything to exception sink (including callee instance and exception)
    val exceptionHandlingCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    exceptionHandlingCode += ASTORE_1 // Store exception in local var 1
    exceptionHandlingCode += ALOAD_0 // Load callee object reference from local var 0
    exceptionHandlingCode += ALOAD_1 // Load exception
    exceptionHandlingCode ++= loadStoredReferenceTypeParametersAndCallSinkAndReturn(
      instanceMethod.parameterTypes,
      exceptionSinkId,
      localVariableIndexOffset = 2
    )

    methodBody ++= tryCatchBlock(
      instanceMethodAndSinkCall.result(),
      exceptionHandlingCode.result(),
      Symbol("instance method call exception")
    )

    METHOD(
      PUBLIC.STATIC,
      callerMethodName,
      "()V",
      CODE(methodBody.result())
    )
  }

  private def createDefaultParametersAndStoreReferenceTypesToLocalVariables(parameterList: FieldTypes,
                                                                            localVariableIndexOffset: Int
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
                                                                  localVariableIndexOffset: Int
                                                                 ): Seq[CodeElement[Nothing]] = {
    parameterList
      .filter(parameter => parameter.isReferenceType)
      .zipWithIndex
      .map[CodeElement[Nothing]] { case (_: FieldType, localVariableIndex: Int) =>
        instructionToInstructionElement(ALOAD(localVariableIndex + localVariableIndexOffset))
      }
  }

  private def loadStoredReferenceTypeParametersAndCallSinkAndReturn(parameters: FieldTypes,
                                                                    sinkId: FullMethodIdentifier,
                                                                    localVariableIndexOffset: Int
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
}
