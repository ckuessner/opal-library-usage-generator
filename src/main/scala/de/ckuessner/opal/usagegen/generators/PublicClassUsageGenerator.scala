package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers._
import org.opalj.br._
import org.opalj.br.instructions._

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

object PublicClassUsageGenerator {
  def generatePublicStaticMethodCall(classFileOfCalledMethod: ClassFile,
                                     calledMethod: Method,
                                     sinkClassName: String,
                                     sinkMethodName: String): Seq[LabeledInstruction] = {

    if (!(calledMethod.isPublic && calledMethod.isStatic && calledMethod.isNotAbstract)) {
      throw new IllegalArgumentException("method must be public, static and not abstract")
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
