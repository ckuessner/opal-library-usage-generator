package de.ckuessner.opal.usagegen.generators.parameters

import de.ckuessner.opal.usagegen.generators.parameters.DefaultValueGenerator.defaultValueForFieldType
import org.opalj.ba.CodeElement
import org.opalj.br.{FieldType, Method, ObjectType}

object DefaultValueParameterGenerator extends ParameterGenerator {
  override def generateParameter(parameterType: FieldType): Array[CodeElement[Nothing]] = {
    defaultValueForFieldType(parameterType)
  }

  // While returning the result of generateParameter here might work for boxed types, this is not useful in
  // general, as the DefaultValueParameterGenerator almost always just relies on ACONST_NULL for object types
  override def generateInstance(objectType: ObjectType): Option[Array[CodeElement[Nothing]]] = None

  override def generateInstance(objectType: ObjectType, calledInstanceMethod: Method): Option[Array[CodeElement[Nothing]]] = None
}
