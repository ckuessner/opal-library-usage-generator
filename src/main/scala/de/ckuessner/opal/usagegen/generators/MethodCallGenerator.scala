package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.{defaultValueForFieldType, generateSinkMethodSignature, storeInstruction}
import org.opalj.br.instructions.{ALOAD, INVOKESTATIC, LabeledInstruction, RETURN}
import org.opalj.br.{BaseType, ClassFile, Method, ReferenceType}

import scala.collection.mutable.ListBuffer

object MethodCallGenerator {
  /**
   * Generates the bytecode for calling the specified _static_ method.
   *
   * @param classFileOfCalledMethod The `ClassFile` of the class the called method belongs to.
   * @param calledMethod            The Method on the called class.
   * @param sinkClassName           The name of the sink class that is called after calling the method.
   * @param sinkMethodName          The name of the sink method on the sink class.
   * @return The instructions of the caller method body
   */
  def generateStaticMethodCaller(classFileOfCalledMethod: ClassFile,
                                 calledMethod: Method,
                                 sinkClassName: String,
                                 sinkMethodName: String): Seq[LabeledInstruction] = {

    // Check if really static, (non-abstract is implied by static)
    if (calledMethod.isNotStatic) {
      throw new IllegalArgumentException("method to call must be static")
    }

    val methodBody = ListBuffer.empty[LabeledInstruction]

    // Reference types need to be stored as locals, as they are passed to sink after consumption by method call.
    // This means that we construct the default values for reference types first and store them as locals.
    calledMethod.descriptor.parameterTypes
      .filter(parameter => parameter.isReferenceType)
      .foreachWithIndex { case (paramType, localVarIndex) =>
        // Construct default value
        methodBody ++= defaultValueForFieldType(paramType)
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
            methodBody ++= defaultValueForFieldType(paramType)
        }
      }
    }

    // Call method of tested library
    methodBody += INVOKESTATIC(
      declaringClass = classFileOfCalledMethod.thisType,
      isInterface = false,
      name = calledMethod.name,
      methodDescriptor = calledMethod.descriptor
    )

    // The return value (if non-void) is on the stack after the method call

    // After execution of the above method call, the reference type parameters need to be passed to sink -> load them.
    calledMethod.descriptor.parameterTypes
      .filter(parameter => parameter.isReferenceType)
      .foreachWithIndex { case (_, localVarIndex) =>
        methodBody += ALOAD(localVarIndex)
      }

    // Call sink, passing return value of called method and all pass-by-reference parameters
    val sinkMethodSignature = generateSinkMethodSignature(calledMethod.returnType, calledMethod.parameterTypes)
    methodBody += INVOKESTATIC(sinkClassName, isInterface = false, sinkMethodName, sinkMethodSignature)

    // Simply return, the stack should now be empty
    methodBody += RETURN
    methodBody
  }
}
