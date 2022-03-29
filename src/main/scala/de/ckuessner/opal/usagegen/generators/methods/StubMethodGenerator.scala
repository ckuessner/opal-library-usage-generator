package de.ckuessner.opal.usagegen.generators.methods

import de.ckuessner.opal.usagegen.FullMethodIdentifier
import de.ckuessner.opal.usagegen.generators.parameters.DefaultValueGenerator
import org.opalj.ba.{CODE, CodeElement, METHOD, PUBLIC}
import org.opalj.br.instructions.{RETURN, ReturnInstruction}
import org.opalj.br.{CTIntType, FieldType, Type, VoidType}

// TODO: This could also be changed to use a parameterGenerator that is specified on construction
object StubMethodGenerator {
  def generateStubMethod(methodId: FullMethodIdentifier): METHOD[_] = {
    METHOD(PUBLIC, methodId.methodName, methodId.descriptor,
      CODE(
        returnDefaultValue(methodId.methodDescriptor.returnType)
      )
    )
  }

  private def returnDefaultValue(returnValue: Type): Array[CodeElement[Nothing]] = {
    returnValue match {
      case _: VoidType => Array(RETURN)

      case fieldType: FieldType =>
        val valueCode = DefaultValueGenerator.defaultValueForFieldType(returnValue.asFieldType)
        val methodBody = new Array[CodeElement[Nothing]](valueCode.length + 1)
        valueCode.copyToArray(methodBody)
        methodBody(valueCode.length) = ReturnInstruction(fieldType)
        methodBody

      case _: CTIntType => ??? // This is not a valid return type
    }
  }
}
