package de.ckuessner.opal.usagegen.generators

import org.opalj.br._
import org.opalj.br.instructions._

import scala.collection.mutable.ListBuffer

object ByteCodeGenerationHelpers {
  def storeInstruction(theType: Type, index: Int): StoreLocalVariableInstruction = {
    theType match {
      case _: ReferenceType => ASTORE(index)
      case _: IntLikeType => ISTORE(index)
      case _: DoubleType => DSTORE(index)
      case _: FloatType => FSTORE(index)
      case _: LongType => LSTORE(index)
      case _: BooleanType => ISTORE(index)
    }
  }

  def loadInstruction(theType: Type, index: Int): LoadLocalVariableInstruction = {
    theType match {
      case _: ReferenceType => ALOAD(index)
      case _: IntLikeType => ILOAD(index)
      case _: DoubleType => DLOAD(index)
      case _: FloatType => FLOAD(index)
      case _: LongType => LLOAD(index)
      case _: BooleanType => ILOAD(index)
    }
  }

  def defaultValueForFieldType(fieldType: FieldType): Seq[LabeledInstruction] = {
    val code = ListBuffer.empty[LabeledInstruction]

    fieldType match {
      case _: ObjectType =>
        // push null onto stack
        code.append(ACONST_NULL)
      case arrayType: ArrayType => {
        code ++= (1 to arrayType.dimensions).map(_ => ICONST_0)

        // Nested array
        if (arrayType.dimensions > 1) {
          code += MULTIANEWARRAY(arrayType, arrayType.dimensions)
        } else {
          // create 1d array with size 0
          code += (arrayType.elementType match {
            case referenceType: ReferenceType => ANEWARRAY(referenceType)
            case baseType: BaseType => NEWARRAY(baseType.atype)
          })
        }
      }
      case _: IntLikeType => code += ICONST_0
      case _: DoubleType => code += DCONST_0
      case _: FloatType => code += FCONST_0
      case _: LongType => code += LCONST_0
      case _: BooleanType => code += ICONST_0
    }

    code
  }

  def generateMethodSignature(method: Method): String = {
    generateMethodSignature(method.returnType, method.parameterTypes)
  }

  def generateMethodSignature(returnType: Type, parameters: Seq[Type]): String = {
    // Signature of the sink method
    val descriptor = new StringBuilder()
    descriptor += '('
    parameters.foreach(theType =>
      descriptor ++= theType.toJVMTypeName
    )
    descriptor += ')'
    descriptor ++= returnType.toJVMTypeName
    descriptor.toString
  }

  def generateSinkMethodSignature(apiMethodReturnType: Type, apiMethodParameterTypes: Seq[Type]): String = {
    // Add parameters to sink method that are not base types (i.e., ignore int, double, ...)
    val referenceTypeParams = apiMethodParameterTypes.filter {
      case _: ReferenceType => true
      case _: BaseType => false
    }

    var sinkMethodParams = Seq.empty[Type]
    // Add return value of called method to sink, if not void
    if (!apiMethodReturnType.isVoidType) {
      sinkMethodParams = apiMethodReturnType +: referenceTypeParams
    } else {
      sinkMethodParams = referenceTypeParams
    }

    // returnType is void, since sinks only consume values but don't return any
    generateMethodSignature(VoidType, sinkMethodParams)
  }

}
