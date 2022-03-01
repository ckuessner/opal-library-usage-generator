package de.ckuessner.opal.usagegen.generators

import org.opalj.ba.InstructionElement
import org.opalj.br._
import org.opalj.br.instructions.{ACONST_NULL, ANEWARRAY, DCONST_0, FCONST_0, GETSTATIC, ICONST_0, INVOKESTATIC, LCONST_0, LoadString_W, MULTIANEWARRAY, NEWARRAY}

import scala.collection.mutable.ArrayBuffer

object DefaultValueLoadingGenerator {
  def defaultValuesForFieldTypes(fieldTypes: FieldTypes): Seq[InstructionElement] = {
    fieldTypes.map(fieldType => defaultValueForFieldType(fieldType)).flatten
  }

  private def defaultValueForNumericType(numericType: NumericType): InstructionElement = numericType match {
    case _: IntLikeType => ICONST_0
    case _: DoubleType => DCONST_0
    case _: FloatType => FCONST_0
    case _: LongType => LCONST_0
  }

  private def unboxNumericObjectType(objectType: ObjectType): BaseType = {
    objectType match {
      case ObjectType.Boolean => BooleanType
      case ObjectType.Byte => ByteType
      case ObjectType.Character => CharType
      case ObjectType.Short => ShortType
      case ObjectType.Integer => IntegerType
      case ObjectType.Long => LongType
      case ObjectType.Float => FloatType
      case ObjectType.Double => DoubleType
    }
  }

  private def defaultValueForObjectType(objectType: ObjectType): Array[InstructionElement] = objectType match {
    case ObjectType.String => Array(LoadString_W(""))

    case ObjectType.Boolean => Array(GETSTATIC("java/lang/Boolean", "FALSE", "java/lang/Boolean"))

    case ObjectType.Integer | ObjectType.Byte | ObjectType.Short | ObjectType.Character | ObjectType.Long | ObjectType.Float | ObjectType.Double => {
      val unboxedType = unboxNumericObjectType(objectType)
      Array(
        // Load constant
        objectType match {
          case ObjectType.Long => LCONST_0
          case ObjectType.Float => FCONST_0
          case ObjectType.Double => DCONST_0
          case _ => ICONST_0
        },
        // primitive to object boxing
        INVOKESTATIC(
          declaringClass = objectType.toJVMTypeName,
          isInterface = false,
          methodName = "valueOf",
          methodDescriptor = s"(${unboxedType.toJVMTypeName}L${objectType.toJVMTypeName};"
        )
      )
    }

    // Fallback: use null
    case _ => Array(ACONST_NULL)
  }

  def defaultValueForFieldType(fieldType: FieldType): Array[InstructionElement] = {
    if (fieldType.isObjectType) {
      return defaultValueForObjectType(fieldType.asObjectType)
    }

    val code = ArrayBuffer.empty[InstructionElement]
    fieldType match {
      case arrayType: ArrayType => {
        code ++= (1 to arrayType.dimensions).map(_ => InstructionElement(ICONST_0))

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
      case numericType: NumericType => code += defaultValueForNumericType(numericType)
      case _: BooleanType => code += ICONST_0
    }

    code.toArray
  }

}
