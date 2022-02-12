package de.ckuessner.opal.usagegen.generators

import org.opalj.ba.{CATCH, CodeElement, InstructionElement, TRY, TRYEND}
import org.opalj.br._
import org.opalj.br.instructions._

import java.util.regex.Pattern
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

  def tryCatchBlock(tryBlock: Iterable[CodeElement[Nothing]],
                    catchBlock: Iterable[CodeElement[Nothing]],
                    exceptionSymbol: Symbol,
                    handlerTablePosition: Int = 0,
                    handlerType: Option[ObjectType] = None
                    ): Array[CodeElement[Nothing]] = {

    val bytecode = mutable.ArrayBuilder.make[CodeElement[Nothing]]

    bytecode += TRY(exceptionSymbol)
    bytecode ++= tryBlock
    bytecode += TRYEND(exceptionSymbol)
    bytecode += CATCH(exceptionSymbol, handlerTablePosition, handlerType)
    bytecode ++= catchBlock

    bytecode.result()
  }


  def defaultValueForFieldType(fieldType: FieldType): Array[InstructionElement] = {
    val code = ArrayBuffer.empty[InstructionElement]

    fieldType match {
      case _: ObjectType =>
        // push null onto stack
        code.append(ACONST_NULL)
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
      case _: IntLikeType => code += ICONST_0
      case _: DoubleType => code += DCONST_0
      case _: FloatType => code += FCONST_0
      case _: LongType => code += LCONST_0
      case _: BooleanType => code += ICONST_0
    }

    code.toArray
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

  /**
   * Translates the package/class to the FQN on the JVM.
   *
   * E.g.: packageName="java.lang", className="String" -> "java/lang/String"
   *
   * @param packageName The package name in dot-notation or ´""´ for default package
   * @param className   The name of the class in the package
   * @return The FQN class string in slash-notation
   */
  def packageAndClassToJvmClassName(packageName: String = "", className: String): String = {
    if (packageName.isEmpty) className
    else packageName.replace(".", "/") + "/" + className
  }

  val unqualifiedNameIllegalCharsPattern: Pattern = Pattern.compile("[.;\\[/<>]")
  val fqnIllegalCharsPattern: Pattern = Pattern.compile("[;\\[/<>]")

  def generateCallerMethodName(classFile: ClassFile, method: Method, uniqueMethodId: Int): String = {
    // Restrictions on names described in https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.2.2
    val sb = new StringBuilder()
    val className = classFile.thisType.simpleName
    val sanitizedClassName = unqualifiedNameIllegalCharsPattern.matcher(className.replace("?", "??")).replaceAll("?")
    sb.append(sanitizedClassName)
    sb.append("__")
    if (method.isConstructor) {
      sb.append("_init_")
    } else
      sb.append(unqualifiedNameIllegalCharsPattern.matcher(method.name).replaceAll("_"))
    sb.append("__")
    sb.append(uniqueMethodId)
    sb.toString()
  }
}
