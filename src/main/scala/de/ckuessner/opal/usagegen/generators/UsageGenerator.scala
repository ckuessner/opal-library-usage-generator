package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.generateCallerMethodName
import de.ckuessner.opal.usagegen.generators.methods.MethodCallGenerator
import de.ckuessner.opal.usagegen.{CallerClass, CallerMethod, FullMethodIdentifier, SinkMethod}
import org.opalj.br.analyses.Project
import org.opalj.br.{ClassFile, Method}
import org.opalj.collection.immutable.RefArray

import scala.language.postfixOps

class UsageGenerator(private val project: Project[_],
                     private val methodCallGenerator: MethodCallGenerator,
                     val callerClassName: String,
                     val sinkClassPackage: String,
                     val sinkClassName: String
                    ) {

  def generateDummyUsage: RefArray[CallerClass] = {
    // Generate caller methods with sinks and collect the caller methods in one class per package
    val callerClasses = project.allProjectClassFiles
      .groupBy(_.thisType.packageName)
      .map { case (packageName, classFilesInPackage) =>
        val callerMethods = classFilesInPackage
          .flatMap { calleeClassFile: ClassFile =>
            // Generate usage for methods (including constructor methods)
            calleeClassFile.methods
              .filterNot(_.isPrivate) // TODO: Implementation for private methods (including constructors)
              .filterNot(_.isStaticInitializer) // Static initializers (i.e., <clinit>) is invoked by jvm on first load of class and not directly callable
              .zipWithIndex // index is used to ensure uniqueness of caller method name
              .map[CallerMethod]({ case (method: Method, uniqueNumber: Int) =>
                // Name of the method that calls the library method
                val callerMethodName = generateCallerMethodName(calleeClassFile, method, uniqueNumber)
                // Method identifier for the caller method
                val callerMethodId = FullMethodIdentifier(packageName, callerClassName, callerMethodName, "()V")

                val sinkMethodBaseName = calleeClassFile.thisType.packageName.replace('/', '_') + "___" + callerMethodName
                val sinkMethod = SinkGenerator.generateSinkMethod(sinkClassPackage, sinkClassName, sinkMethodBaseName, method)
                val exceptionSinkMethod = SinkGenerator.generateExceptionSinkMethod(sinkClassPackage, sinkClassName, sinkMethodBaseName + "_exception", method)

                // Generate caller method for `method`
                generateCallerMethod(method, callerMethodId, sinkMethod, exceptionSinkMethod)
              })
          }

        CallerClass(
          packageName,
          callerClassName,
          RefArray._UNSAFE_from(callerMethods.toArray)
        )
      }

    RefArray._UNSAFE_from(callerClasses.toArray)
  }

  private def generateCallerMethod(method: Method,
                                   callerMethodId: FullMethodIdentifier,
                                   sink: SinkMethod,
                                   exceptionSink: SinkMethod): CallerMethod = {

    methodCallGenerator.generateCallerMethod(method, callerMethodId, sink, exceptionSink)
  }
}

