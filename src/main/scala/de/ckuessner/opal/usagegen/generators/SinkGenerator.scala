package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.generateMethodSignature
import de.ckuessner.opal.usagegen.{CallerClass, SinkClass}
import org.opalj.ba.{CLASS, CODE, METHOD, METHODS, PUBLIC}
import org.opalj.br._
import org.opalj.br.instructions.RETURN
import org.opalj.collection.immutable.RefArray

import scala.language.postfixOps

object SinkGenerator {
  /**
   * Generate a Sink class with the given className and packageName using the sink methods of the callerClasses.
   * The packageName and className of the sinks in the caller classes are not checked!
   *
   * @param packageName   The name of the package containing the sink class.
   * @param className     The name of the sink class.
   * @param callerClasses The source of the sink methods.
   * @return A SinkClass containing the sink methods from callerClasses.
   */
  def generateSinkClass(packageName: String, className: String, callerClasses: Iterable[CallerClass]): SinkClass = {
    val sinkMethods = callerClasses
      .flatMap(_.callerMethods)
      .flatMap(callerMethod => {
        List(callerMethod.sink, callerMethod.exceptionSink)
      }).toSet

    SinkClass(packageName, className, sinkMethods)
  }

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

  def generateSinkMethod(methodName: String, apiMethod: Method): METHOD[_] = {
    val descriptor = generateSinkMethodSignature(apiMethod.returnType, apiMethod.parameterTypes)

    METHOD(
      PUBLIC STATIC,
      methodName,
      descriptor,
      CODE(RETURN)
    )
  }

  def generateExceptionSinkMethod(methodName: String, apiMethod: Method): METHOD[_] = {
    val descriptor = generateSinkMethodSignature(Type(classOf[Throwable]), apiMethod.parameterTypes)

    METHOD(
      PUBLIC STATIC,
      methodName,
      descriptor,
      CODE(RETURN)
    )
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