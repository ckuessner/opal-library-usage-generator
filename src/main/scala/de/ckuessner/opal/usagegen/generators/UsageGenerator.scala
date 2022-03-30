package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.UsageGeneratorCli.Config
import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.generateCallerMethodName
import de.ckuessner.opal.usagegen.generators.methods.{MethodCallGenerator, SinkMethodGenerator}
import de.ckuessner.opal.usagegen._
import de.ckuessner.opal.usagegen.generators.methods.SinkMethodGenerator.{generateExceptionSinkMethod, generateSinkMethod}
import org.opalj.br.analyses.Project
import org.opalj.br.{ClassFile, Method}
import org.opalj.collection.immutable.RefArray

import scala.language.postfixOps

class UsageGenerator(private val project: Project[_],
                     private val methodCallGenerator: MethodCallGenerator,
                     config: Config
                    ) {

  def generateCallersAndSinks: RefArray[(CallerClass, SinkClass)] = {
    // Generate caller methods with sinks and collect the caller methods in one class per package
    val callerClasses = project.allProjectClassFiles
      .groupBy(_.thisType.packageName)
      .map { case (calleePackageName, classFilesInPackage) =>
        classFilesInPackage.iterator
          .map { calleeClassFile: ClassFile =>
            val callerClassName = s"${config.callerClassBaseName}$$${calleeClassFile.thisType.simpleName}"
            val sinkClassName = s"${config.sinkClassBaseName}$$${calleeClassFile.thisType.simpleName}"

            // Generate usage for methods (including constructor methods)
            val callerMethods = calleeClassFile.methods
              .filterNot(_.isPrivate) // TODO: Implementation for private methods (including constructors)
              .filterNot(_.isStaticInitializer) // Static initializers (i.e., <clinit>) is invoked by jvm on first load of class and not directly callable
              .zipWithIndex // index is used to ensure uniqueness of caller method name
              .map[CallerMethod]({ case (method: Method, uniqueNumber: Int) =>
                // Name of the method that calls the library method
                val callerMethodName = generateCallerMethodName(method.classFile, method, uniqueNumber)
                // Method identifier for the caller method
                val callerMethodId = FullMethodIdentifier(calleePackageName, callerClassName, callerMethodName, "()V")

                // Generate caller method with attached sink methods
                generateCallerAndSinkMethods(method, callerMethodId, sinkClassName)
              })

            // Unpack sink methods into sink class
            val sinkClass = SinkClass(
              calleePackageName,
              sinkClassName,
              callerMethods.map(_.sink) ++ callerMethods.map(_.exceptionSink)
            )

            // Collect sink methods into sink class
            val callerClass = CallerClass(
              calleePackageName,
              callerClassName,
              RefArray._UNSAFE_from(callerMethods.toArray),
            )

            (callerClass, sinkClass)
          }
      }

    RefArray._UNSAFE_from(callerClasses.flatten.toArray)
  }

  private def generateCallerAndSinkMethods(method: Method,
                                   callerMethodId: FullMethodIdentifier,
                                   sinkClassName: String
                                   ): CallerMethod = {

    val calleeType = method.classFile.thisType
    val sinkMethodBaseName = callerMethodId.packageName.replace('/', '_') + "___" + callerMethodId.methodName

    val sink = generateSinkMethod(calleeType.packageName, sinkClassName, sinkMethodBaseName, method)
    val exceptionSink = generateExceptionSinkMethod(calleeType.packageName, sinkClassName, sinkMethodBaseName + "_exception", method)

    methodCallGenerator.generateCallerMethod(method, callerMethodId, sink, exceptionSink)
  }
}

