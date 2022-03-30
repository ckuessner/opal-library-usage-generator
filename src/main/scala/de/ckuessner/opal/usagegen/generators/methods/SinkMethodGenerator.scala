package de.ckuessner.opal.usagegen.generators.methods

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.generateMethodSignature
import de.ckuessner.opal.usagegen.{FullMethodIdentifier, SinkMethod}
import org.opalj.ba.{CODE, METHOD, PUBLIC}
import org.opalj.br.instructions.RETURN
import org.opalj.br.{Method, ObjectType, Type, VoidType}

import scala.collection.mutable

object SinkMethodGenerator {
  def generateSinkMethod(sinkClassPackage: String,
                         sinkClassName: String,
                         sinkMethodName: String,
                         apiMethod: Method
                        ): SinkMethod = {

    var returnType = apiMethod.returnType
    var calleeType: Option[ObjectType] = None // None if static, or constructor

    // If the method is a constructor method, pass the instantiated object to the sink
    if (apiMethod.isConstructor) {
      returnType = apiMethod.classFile.thisType
    }
    // If the method is an instance method, pass the callee instance to the sink
    else if (apiMethod.isNotStatic) {
      calleeType = Some(apiMethod.classFile.thisType)
    }


    val descriptor = generateSinkMethodSignature(calleeType, returnType, apiMethod.parameterTypes)
    generateSinkMethod(sinkClassPackage, sinkClassName, sinkMethodName, descriptor)
  }

  def generateExceptionSinkMethod(sinkClassPackage: String,
                                  sinkClassName: String,
                                  sinkMethodName: String,
                                  apiMethod: Method
                                 ): SinkMethod = {

    var calleeType: Option[ObjectType] = None // None if static, or constructor
    if (!apiMethod.isConstructor && apiMethod.isNotStatic) {
      calleeType = Some(apiMethod.classFile.thisType)
    }

    val descriptor = generateSinkMethodSignature(calleeType, Type(classOf[Throwable]), apiMethod.parameterTypes)
    generateSinkMethod(sinkClassPackage, sinkClassName, sinkMethodName, descriptor)
  }

  private def generateSinkMethod(sinkClassPackage: String,
                                 sinkClassName: String,
                                 sinkMethodName: String,
                                 descriptor: String
                                ): SinkMethod = {

    // Sink method body
    val sinkMethodBody = METHOD(
      PUBLIC.STATIC,
      sinkMethodName,
      descriptor,
      CODE(RETURN)
    )

    // Sink method identifiers
    val sinkMethodId = FullMethodIdentifier(sinkClassPackage, sinkClassName, sinkMethodName, descriptor)

    SinkMethod(sinkMethodId, sinkMethodBody)
  }

  private def generateSinkMethodSignature(instanceType: Option[ObjectType],
                                          apiMethodReturnType: Type,
                                          apiMethodParameterTypes: Seq[Type]
                                         ): String = {

    // Parameters are (in order): [instanceType,] [apiMethodReturnType,] referenceTypeParameters...
    val sinkMethodParameters = mutable.ArrayBuilder.make[Type]()

    if (instanceType.isDefined) sinkMethodParameters += instanceType.get

    // Add return value of called method to sink, if not void
    if (!apiMethodReturnType.isVoidType) {
      sinkMethodParameters += apiMethodReturnType
    }

    // Add parameters to sink method that are not base types (i.e., ignore int, double, ...)
    sinkMethodParameters ++= apiMethodParameterTypes.filter(_.isReferenceType)

    // returnType is void, since sinks only consume values but don't return any
    generateMethodSignature(VoidType, sinkMethodParameters.result())
  }

}
