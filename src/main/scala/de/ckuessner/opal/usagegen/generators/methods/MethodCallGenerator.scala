package de.ckuessner.opal.usagegen.generators.methods

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.tryCatchBlock
import de.ckuessner.opal.usagegen.generators.parameters.ParameterGenerator
import de.ckuessner.opal.usagegen.{CallerMethod, ConcreteSubclass, FullMethodIdentifier, SinkMethod}
import org.opalj.ba.CodeElement._
import org.opalj.ba.{CODE, CodeElement, METHOD, PUBLIC}
import org.opalj.br._
import org.opalj.br.instructions.{ALOAD, ALOAD_0, ALOAD_1, ASTORE, ASTORE_0, ASTORE_1, DUP, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, LoadLocalVariableInstruction, NEW, RETURN, StoreLocalVariableInstruction}
import org.opalj.collection.immutable.RefArray

import scala.collection.mutable

class MethodCallGenerator(private val parameterGenerator: ParameterGenerator,
                          private val stubSubclasses: Map[ObjectType, ConcreteSubclass]) {

  def generateCallerMethod(method: Method,
                           callerMethodId: FullMethodIdentifier,
                           sink: SinkMethod,
                           exceptionSink: SinkMethod): CallerMethod = {

    val callerMethod =
      if (method.isConstructor) {
        generateConstructorCallerMethod(
          callerMethodName = callerMethodId.methodName,
          calledConstructor = method,
          sink.methodId,
          exceptionSink.methodId
        )
      } else if (method.isStatic) {
        generateStaticMethodCallerMethod(
          callerMethodName = callerMethodId.methodName,
          calledMethod = method,
          sink.methodId,
          exceptionSink.methodId
        )
      } else if (method.isNotStatic) {
        generateInstanceMethodCallerMethod(
          callerMethodName = callerMethodId.methodName,
          instanceMethod = method,
          sinkId = sink.methodId,
          exceptionSinkId = exceptionSink.methodId
        )
      } else {
        // This is unreachable, since a method is either static, or non-static. But is left here as a placeholder.
        ???
      }

    CallerMethod(
      callerMethodId,
      callerMethod,
      sink,
      exceptionSink
    )
  }

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
    methodBody ++= createParametersAndStoreObjectParametersToLocalVariables(calledMethod, 1)

    // Call method of tested library
    val methodCallCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    methodCallCode += INVOKESTATIC(
      declaringClass = calledMethod.classFile.thisType,
      isInterface = false,
      name = calledMethod.name,
      methodDescriptor = calledMethod.descriptor
    )

    // The return value (if non-void) is on the stack after the method call
    methodCallCode ++= loadObjectParametersAndCallSinkAndReturn(calledMethod.parameterTypes, sinkId, 1)

    // Handle Exception: pass everything to exception sink
    val exceptionHandlingCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]()
    exceptionHandlingCode += ASTORE_0
    exceptionHandlingCode += ALOAD_0
    exceptionHandlingCode ++= loadObjectParametersAndCallSinkAndReturn(calledMethod.parameterTypes, exceptionSinkId, 1)

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

    // TODO: This method is currently unused, can be removed if testing static initializers separately is not required

    val constructionCode = Array[CodeElement[Nothing]](
      // Only create object using new, the static initializer is called implicitly
      if (classFile.isAbstract) {
        // TODO: This might fail
        NEW(stubSubclasses(classFile.thisType).jvmClassName)
      } else {
        NEW(classFile.thisType)
      },
      // Pass reference to sink
      INVOKESTATIC(
        sinkId.jvmClassName,
        isInterface = false,
        sinkId.methodName,
        sinkId.descriptor
      ),
      RETURN
    )

    val exceptionHandlerCode = Array[CodeElement[Nothing]](
      INVOKESTATIC(
        exceptionSinkId.jvmClassName,
        isInterface = false,
        exceptionSinkId.methodName,
        exceptionSinkId.descriptor
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

    // Only used if class is abstract
    val stubSubClass = stubSubclasses.get(calledConstructor.classFile.thisType)

    val methodBody = mutable.ArrayBuilder.make[CodeElement[Nothing]]

    // If the class is abstract, use the concrete subclass stub
    if (calledConstructor.classFile.isAbstract) {
      // TODO: Maybe add explicit error handling if stubSubClass is None
      methodBody += NEW(stubSubClass.get.jvmClassName)
    } else {
      methodBody += NEW(calledConstructor.classFile.thisType)
    }
    methodBody += ASTORE_0 // Store object ref to local var 0
    methodBody += ALOAD_0 // Load it again for constructor call
    // Load constructor parameters, storing references to local vars
    methodBody ++= createParametersAndStoreObjectParametersToLocalVariables(
      calledConstructor,
      localVariableIndexStart = 1
    )

    val callConstructorAndSinkCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    callConstructorAndSinkCode += INVOKESPECIAL( // Call constructor on real class or delegate stub class
      if (calledConstructor.classFile.isAbstract) {
        if (!stubSubClass.get.constructorMethods.exists(_.methodId.descriptor.equals(calledConstructor.descriptor.toJVMDescriptor)))
          throw new RuntimeException(s"Concrete stub subclass $stubSubClass doesn't implement <init>${calledConstructor.descriptor}")
        else
          stubSubClass.get.jvmClassName
      } else {
        calledConstructor.classFile.thisType.toJVMTypeName
      },
      isInterface = false,
      "<init>",
      calledConstructor.descriptor.toJVMDescriptor
    )

    callConstructorAndSinkCode += ALOAD_0 // Load reference to constructed object

    // The reference to the constructed object is on the stack (because of ALOAD_0)
    // The reference type parameters need to be passed to sink -> load them.
    // calling sink and RETURN is already handled by method below
    callConstructorAndSinkCode ++= loadObjectParametersAndCallSinkAndReturn(
      calledConstructor.parameterTypes,
      sinkId,
      localVariableIndexOffset = 1
    )

    // Handle Exception: pass everything to exception sink (including exception, but not uninitialized object)
    val exceptionHandlingCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    exceptionHandlingCode += ASTORE_0 // Overrides reference to created object (that is unusable, because of exception)
    exceptionHandlingCode += ALOAD_0
    exceptionHandlingCode ++= loadObjectParametersAndCallSinkAndReturn(
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

    methodBody ++= createInstanceAndStoreInLocalVar(instanceMethod, 0)

    // Get reference to callee object
    methodBody += ALOAD_0
    // Load parameters for instance method  (starting at local variable 2 because we need to store reference to callee object (in 0) and exception (in 1))
    methodBody ++= createParametersAndStoreObjectParametersToLocalVariables(instanceMethod, localVariableIndexStart = 2)

    // Body of try-catch block containing method & sink call
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
    instanceMethodAndSinkCall ++= loadObjectParametersAndCallSinkAndReturn(instanceMethod.parameterTypes, sinkId, localVariableIndexOffset = 2)

    // Handle Exception: pass everything to exception sink (including callee instance and exception)
    val exceptionHandlingCode = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    exceptionHandlingCode += ASTORE_1 // Store exception in local var 1
    exceptionHandlingCode += ALOAD_0 // Load callee object reference from local var 0
    exceptionHandlingCode += ALOAD_1 // Load exception
    exceptionHandlingCode ++= loadObjectParametersAndCallSinkAndReturn(
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

  private def createInstanceAndStoreInLocalVar(calledMethod: Method,
                                               localVarIndex: Int
                                              ): Array[CodeElement[Nothing]] = {

    val instantiationCode = parameterGenerator.generateInstance(calledMethod.classFile.thisType, calledMethod)
    val code = new Array[CodeElement[Nothing]](instantiationCode.length + 1)
    code(instantiationCode.length) = ASTORE.canonicalRepresentation(localVarIndex)
    code
  }

  private def createParametersAndStoreObjectParametersToLocalVariables(calledMethod: Method,
                                                                       localVariableIndexStart: Int
                                                                      ): Array[CodeElement[Nothing]] = {
    var localVarIndex = localVariableIndexStart
    val methodBody = mutable.ArrayBuilder.make[CodeElement[Nothing]]()

    // Load/construct parameters for method call
    for (paramType <- calledMethod.descriptor.parameterTypes) {
      paramType match {
        case _: ReferenceType =>
          methodBody ++= parameterGenerator.generateParameter(paramType) // TODO: Change to generation with context
          methodBody += ASTORE(localVarIndex)
          methodBody += ALOAD(localVarIndex)
          localVarIndex += 1
        case _: BaseType =>
          // Place default value on stack (base types aren't constructed in prior step)
          methodBody ++= parameterGenerator.generateParameter(paramType) // TODO: Change to generation with context
      }
    }

    methodBody.result()
  }

  private def loadObjectParametersFromLocalVariables(parameterList: FieldTypes,
                                                     localVariableIndexOffset: Int
                                                    ): RefArray[CodeElement[Nothing]] = {
    parameterList
      .filter(parameter => parameter.isReferenceType)
      .zipWithIndex
      .map[CodeElement[Nothing]] { case (_: FieldType, localVariableIndex: Int) =>
        ALOAD(localVariableIndex + localVariableIndexOffset)
      }
  }

  private def loadObjectParametersAndCallSinkAndReturn(parameters: FieldTypes,
                                                       sinkId: FullMethodIdentifier,
                                                       localVariableIndexOffset: Int
                                                      ): Array[CodeElement[Nothing]] = {
    val code = mutable.ArrayBuilder.make[CodeElement[Nothing]]
    code ++= loadObjectParametersFromLocalVariables(parameters, localVariableIndexOffset)
    code += INVOKESTATIC(
      sinkId.jvmClassName,
      isInterface = false,
      sinkId.methodName,
      sinkId.descriptor
    )
    code += RETURN
    code.result()
  }
}
