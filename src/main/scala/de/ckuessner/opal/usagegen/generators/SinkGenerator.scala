package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.generateSinkMethodSignature
import org.opalj.ba.{CLASS, CODE, METHOD, METHODS, PUBLIC}
import org.opalj.br._
import org.opalj.br.instructions.RETURN
import org.opalj.collection.immutable.RefArray

import scala.language.postfixOps

object SinkGenerator {
  def generateSinkClass(sinkClassName: String, methods: Seq[(String, ClassFile, Method)]): CLASS[_] = {
    CLASS(
      accessModifiers = PUBLIC SUPER,
      thisType = sinkClassName,
      methods = new METHODS(generateSinkMethods(methods))
    )
  }

  private def generateSinkMethods[T](methods: Seq[(String, ClassFile, Method)]): RefArray[METHOD[T]] = {
    RefArray._UNSAFE_from(
      methods.map { case (methodName, _, apiMethod) =>
        generateSinkMethod(methodName, apiMethod)
      }.toArray
    )
  }

  @inline
  private def generateSinkMethod(methodName: String, apiMethod: Method): METHOD[_] = {
    val descriptor = generateSinkMethodSignature(apiMethod.returnType, apiMethod.parameterTypes)

    METHOD(
      PUBLIC STATIC,
      methodName,
      descriptor,
      CODE(RETURN)
    )
  }
}