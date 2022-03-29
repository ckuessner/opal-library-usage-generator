package de.ckuessner.opal.usagegen.generators.parameters

import de.ckuessner.opal.usagegen.generators.parameters.DefaultValueGenerator.defaultValueForFieldType
import org.opalj.ba.CodeElement
import org.opalj.br.FieldType

object DefaultValueParameterGenerator extends ParameterGenerator {
  override def generateParameter(parameterType: FieldType): Array[CodeElement[Nothing]] = {
    defaultValueForFieldType(parameterType)
  }
}
