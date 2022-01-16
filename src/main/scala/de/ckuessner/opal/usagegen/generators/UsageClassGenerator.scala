package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.generators.ClassGenerator.generatePublicClass
import de.ckuessner.opal.usagegen.generators.MethodCallGenerator.generateStaticMethodCaller
import de.ckuessner.opal.usagegen.generators.SinkGenerator.generateSinkMethod
import org.opalj.ba.{CLASS, CODE, CodeElement, InstructionElement, METHOD, METHODS, PUBLIC}
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{INVOKESTATIC, RETURN}
import org.opalj.br.{ClassFile, Method}
import org.opalj.collection.immutable.RefArray

import java.util.regex.Pattern
import scala.collection.mutable
import scala.language.postfixOps

object UsageClassGenerator {
  // TODO: It is theoretically possible that these classes already exists in the tested library.
  def generateLibraryTestClasses(project: Project[_],
                                 entryPointClassPackage: Option[String] = None,
                                 entryPointClassName: String = "___TEST_RUNNER_ENTRYPOINT___",
                                 sinkClassName: String = "___SINK___",
                                 sinkClassPackage: Option[String] = None,
                                 callerClassName: String = "___DUMMY_CALLER_CLASS___"
                                ): (CLASS[_], Iterable[CLASS[_]], CLASS[_]) = {

    project.classesPerPackage.map { case (packageName, classFilesInPackage) =>
      classFilesInPackage
        .filterNot(_.isAbstract) // TODO: Implementation for abstract classes with non-abstract methods
        .map { calleeClassFile: ClassFile =>
          calleeClassFile.methods
            .filterNot(_.isAbstract) // TODO: Implementation for abstract methods
            .filterNot(_.isPrivate) // TODO: Implementation for private methods
            .filter(_.isStatic) // TODO: Implementation for non-static methods
            .zipWithIndex.map[(InstructionElement, METHOD[_], METHOD[_])]({ case (method: Method, uniqueNumber: Int) =>
            // Generate caller method for `method`
            val callerMethodName = generateCallerMethodName(calleeClassFile, method, uniqueNumber)
            val callerMethod = generateStaticMethodCaller(
              callerMethodName = callerMethodName,
              calleeClassFile = calleeClassFile,
              calledMethod = method,
              sinkClassName = sinkClassName,
              sinkMethodName = callerMethodName
            )

            // Generate sink method for caller method of `method`
            val sinkMethod = generateSinkMethod(callerMethodName, method)

            // Generate invocation of caller method
            val callerClass = calleeClassFile.thisType.packageName + '/' + callerClassName
            val callerMethodInvocation = INVOKESTATIC(
              callerClass,
              isInterface = false,
              callerMethodName,
              "()V"
            )

            (callerMethodInvocation, callerMethod, sinkMethod)
          }).unzip3
        }.unzip3 match {
        case (a, b, c) => (a.flatten, b.flatten, c.flatten) match {
          case (callerMethodInvocations, callerMethods, sinkMethods) =>
            // Bundle caller methods of package into class in same package (to access package-private classes & methods)
            val callerClass = generatePublicClass(
              Some(packageName),
              callerClassName,
              METHODS(RefArray._UNSAFE_from(callerMethods.toArray))
            )

            (callerMethodInvocations, callerClass, sinkMethods)
        }
      }
    }.unzip3[Set[InstructionElement], CLASS[_], Set[METHOD[_]]] match {
      case (a, b, c) => (a.flatten, b, c.flatten) match {
        case (callerMethodInvocations, callerClasses, sinkMethods) =>
          val sinkClass = generatePublicClass(
            sinkClassPackage,
            sinkClassName,
            METHODS(RefArray._UNSAFE_from(sinkMethods.toArray))
          )

          val entryPointInstructions = mutable.ArrayBuilder.make[CodeElement[InstructionElement]]
          entryPointInstructions ++= callerMethodInvocations
          entryPointInstructions += RETURN

          val entryPointClass = generatePublicClass(
            entryPointClassPackage,
            entryPointClassName,
            METHODS(METHOD(
              PUBLIC.STATIC,
              "callCallers",
              "()V",
              CODE(entryPointInstructions.result())
            ))
          )

          (entryPointClass, callerClasses, sinkClass)
      }
    }
  }

  private val unqualifiedNamePattern = Pattern.compile("[.;\\[/<>]")

  private def generateCallerMethodName(classFile: ClassFile, method: Method, uniqueMethodId: Int): String = {
    // Note: Does not work for constructor (because of <,> in <init> and <clinit>
    // Restrictions on names described in https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.2.2
    val sb = new StringBuilder()
    sb.append(classFile.thisType.fqn.replace('/', '_'))
    sb.append("__")
    sb.append(unqualifiedNamePattern.matcher(method.name).replaceAll(""))
    sb.append("__")
    sb.append(uniqueMethodId)
    sb.toString()
  }
}

