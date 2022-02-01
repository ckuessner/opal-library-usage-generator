package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.generateCallerMethodName
import de.ckuessner.opal.usagegen.generators.MethodCallGenerator.generateStaticMethodCallerMethod
import de.ckuessner.opal.usagegen.generators.SinkGenerator.generateSinkMethod
import de.ckuessner.opal.usagegen.{CallerClass, CallerMethod, FullMethodIdentifier, SinkMethod}
import org.opalj.br.analyses.Project
import org.opalj.br.{ClassFile, Method}

import scala.language.postfixOps

object UsageGenerator {
  // TODO: It is theoretically possible that these classes already exists in the tested library.
  def generateDummyUsage(project: Project[_],
                         callerClassName: String,
                         sinkClassPackage: String,
                         sinkClassName: String,
                        ): Iterable[CallerClass] = {

    project.classesPerPackage.map { case (packageName, classFilesInPackage) =>
      // Generate caller methods with sinks and collect the caller methods in one class per package
      val callerMethods: Set[CallerMethod] = classFilesInPackage
        .filterNot(_.isAbstract) // TODO: Implementation for abstract classes
        .flatMap { calleeClassFile: ClassFile =>
          calleeClassFile.methods
            .filterNot(_.isAbstract) // TODO: Implementation for abstract methods
            .filterNot(_.isPrivate) // TODO: Implementation for private methods
            .filter(_.isStatic) // TODO: Implementation for non-static methods
            .zipWithIndex // index is for uniqueness of caller method name
            .map[CallerMethod]({ case (method: Method, uniqueNumber: Int) =>
              // Name of the method that calls the library method
              val callerMethodName = generateCallerMethodName(calleeClassFile, method, uniqueNumber)
              // Method identifier for the caller method
              val callerMethodId = FullMethodIdentifier(packageName, callerClassName, callerMethodName, "()V")

              // Sink method identifiers
              val sinkMethodId = FullMethodIdentifier(sinkClassPackage, sinkClassName, callerMethodName, "()V")
              val exceptionSinkMethodId = FullMethodIdentifier(sinkClassPackage, sinkClassName, callerMethodName + "_exception", "()V")

              // Generate caller method for `method`
              generateCallerMethod(method, callerMethodId, sinkMethodId, exceptionSinkMethodId)
            })
        }

      CallerClass(
        packageName,
        callerClassName,
        callerMethods
      )
    }
  }

  private def generateCallerMethod(method: Method,
                                   callerMethodId: FullMethodIdentifier,
                                   sinkMethodId: FullMethodIdentifier,
                                   exceptionSinkMethodId: FullMethodIdentifier): CallerMethod = {

    // Generate sink method for caller method of `method`
    val sinkMethod = generateSinkMethod(callerMethodId.methodName, method)

    // Generate exception sink method for caller method of `method`
    val exceptionSinkMethod = generateSinkMethod(exceptionSinkMethodId.methodName, method)

    val callerMethod = generateStaticMethodCallerMethod(
      callerMethodName = callerMethodId.methodName,
      calledMethod = method,
      sinkMethodId,
      exceptionSinkMethodId
    )

    CallerMethod(
      callerMethodId,
      callerMethod,
      SinkMethod(sinkMethodId, sinkMethod),
      SinkMethod(exceptionSinkMethodId, exceptionSinkMethod)
    )
  }
}

