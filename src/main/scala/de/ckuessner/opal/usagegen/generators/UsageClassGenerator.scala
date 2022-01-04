package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.generateMethodSignature
import de.ckuessner.opal.usagegen.generators.PublicClassUsageGenerator.generatePublicStaticMethodCall
import org.opalj.ba.{CLASS, CODE, CodeElement, METHOD, METHODS, PUBLIC}
import org.opalj.br
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{GETSTATIC, INVOKEVIRTUAL, LoadString, RETURN}
import org.opalj.br.{ClassFile, Method}
import org.opalj.collection.immutable.RefArray

import java.util.regex.Pattern
import scala.language.postfixOps

object UsageClassGenerator {
  def buildCallerWithSink(project: Project[_], callerClassName: String, sinkClassName: String): (CLASS[_], CLASS[_]) = {
    val apiMethods = extractMethods(project)
    val sinkClass = SinkGenerator.generateSinkClass(sinkClassName, apiMethods)
    val callerClass = generateCallerClass(callerClassName, sinkClassName, apiMethods)
    (callerClass, sinkClass)
  }

  private def generateCallerClass(callerClassName: String, sinkClassName: String, methods: Seq[(String, ClassFile, Method)]): CLASS[_] = {
    val callerMethods = methods.map { case (callerMethodName, classFile, method) =>
      val instructions = if (method.isPublic && method.isStatic) {
        generatePublicStaticMethodCall(classFile, method, sinkClassName, callerMethodName)
      } else {
        List(
          GETSTATIC("java/lang/System", "out", "Ljava/io/PrintStream;"),
          LoadString("Not implemented yet: " + classFile.fqn + "#" + method.name + generateMethodSignature(method)),
          INVOKEVIRTUAL("java/io/PrintStream", "println", "(Ljava/lang/String;)V"),
          RETURN
        )
      }

      METHOD(
        PUBLIC STATIC,
        callerMethodName,
        "()V", // Doesn't take parameters and doesn't return a value, since the return value is passed into Sink.
        CODE(instructions.toIndexedSeq.map(CodeElement.instructionToInstructionElement))
      )
    }

    CLASS(
      accessModifiers = PUBLIC SUPER,
      thisType = callerClassName,
      methods = METHODS(RefArray._UNSAFE_from(callerMethods.toArray))
    )
  }

  /**
   * Extracts all methods that are not abstract from the project.
   * The generated names should be unique for the project.
   *
   * @param project
   * @return Iterable with (callerMethodName, classFile, method)
   */
  private def extractMethods(project: Project[_]): Seq[(String, ClassFile, Method)] = {
    project.allClassFiles.flatMap { classFile: ClassFile =>
      classFile.methods
        .filter(_.isNotAbstract)
        .zipWithIndex.map {
        _ match {
          case (method: Method, index: Int) => (generateCallerMethodName(classFile, method, index), classFile, method)
        }
      }
    }.toSeq
  }


  private val unqualifiedNamePattern = Pattern.compile("[.;\\[/<>]")

  private def generateCallerMethodName(classFile: br.ClassFile, method: Method, uniqueMethodId: Int): String = {
    // Note: Does not work for constructor (because of <,> in <init> and <clinit>
    // Restrictions on names described in https://docs.oracle.com/javase/specs/jvms/se17/html/jvms-4.html#jvms-4.2.2
    val sb = new StringBuilder()
    sb.append(classFile.thisType.fqn.replace('/', '_'))
    sb.append("__")
    sb.append(unqualifiedNamePattern.matcher(method.name).replaceAll(""))
    sb.append('_')
    sb.append(uniqueMethodId)
    sb.toString()
  }
}

