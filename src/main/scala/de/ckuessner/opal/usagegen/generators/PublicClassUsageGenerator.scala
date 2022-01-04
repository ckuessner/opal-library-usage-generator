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

    // Instructions to initialize parameters
    calledMethod.descriptor.parameterTypes.foreachWithIndex { case (paramType, index) =>
      // Construct default value
      methodBody ++= defaultValueForFieldType(paramType)
      // Store default value
      methodBody += storeInstruction(paramType, index)
    }

    // Load all parameters before call
    calledMethod.descriptor.parameterTypes.foreachWithIndex { case (paramType, index) =>
      methodBody += loadInstruction(paramType, index)
    }

    // Call tested API method
    methodBody += INVOKESTATIC(classFileOfCalledMethod.thisType, isInterface = false, calledMethod.name, calledMethod.descriptor)

    // Load parameters of called method. Pass-by-value parameters are not loaded (e.g., int, double, etc.), because they
    // aren't changed by called method. The return value (if it exists is also passed to the sink method)
    calledMethod.parameterTypes.foreachWithIndex { case (paramType, index) =>
      paramType match {
        case _: ReferenceType => methodBody += ALOAD(index)
        case _: BaseType => // Ignored, because parameter is passed-by-value
      }
    }

    // Call sink, passing return value of called method and all pass-by-reference parameters
    val sinkMethodSignature = generateSinkMethodSignature(calledMethod.returnType, calledMethod.parameterTypes)
    methodBody += INVOKESTATIC(sinkClassName, isInterface = false, sinkMethodName, sinkMethodSignature)

    methodBody += RETURN
    methodBody
  }
}
