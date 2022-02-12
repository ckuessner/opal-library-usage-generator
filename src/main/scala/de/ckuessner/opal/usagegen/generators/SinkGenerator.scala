package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.generateMethodSignature
import de.ckuessner.opal.usagegen.{CallerClass, FullMethodIdentifier, SinkClass, SinkMethod}
import org.opalj.ba.{CODE, METHOD, PUBLIC}
import org.opalj.br._
import org.opalj.br.instructions.RETURN

import scala.collection.mutable
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

  def generateSinkMethod(sinkClassPackage: String,
                         sinkClassName: String,
                         sinkMethodName: String,
                         apiMethod: Method
                        ): SinkMethod = {

    var returnType = apiMethod.returnType
    // If the method is a constructor method, pass the instantiated object to the sink
    if (apiMethod.isConstructor) {
      returnType = apiMethod.classFile.thisType
    }

    val descriptor = generateSinkMethodSignature(returnType, apiMethod.parameterTypes)
    generateSinkMethod(sinkClassPackage, sinkClassName, sinkMethodName, descriptor)
  }

  def generateExceptionSinkMethod(sinkClassPackage: String,
                                  sinkClassName: String,
                                  sinkMethodName: String,
                                  apiMethod: Method
                                 ): SinkMethod = {

    val descriptor = generateSinkMethodSignature(Type(classOf[Throwable]), apiMethod.parameterTypes)
    generateSinkMethod(sinkClassPackage, sinkClassName, sinkMethodName, descriptor)
  }

  private def generateSinkMethod(sinkClassPackage: String,
                                 sinkClassName: String,
                                 sinkMethodName: String,
                                 descriptor: String
                                ): SinkMethod = {

    // Sink method body
    val sinkMethodBody = METHOD(
      PUBLIC STATIC,
      sinkMethodName,
      descriptor,
      CODE(RETURN)
    )

    // Sink method identifiers
    val sinkMethodId = FullMethodIdentifier(sinkClassPackage, sinkClassName, sinkMethodName, descriptor)

    SinkMethod(sinkMethodId, sinkMethodBody)
  }

  private def generateSinkMethodSignature(apiMethodReturnType: Type, apiMethodParameterTypes: Seq[Type]): String = {
    // Add parameters to sink method that are not base types (i.e., ignore int, double, ...)
    val referenceTypeParams = apiMethodParameterTypes.filter(_.isReferenceType)

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