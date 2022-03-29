package de.ckuessner.opal.usagegen.generators.classes

import de.ckuessner.opal.usagegen.{CallerClass, FullMethodIdentifier}
import org.opalj.ba.{CLASS, CODE, CodeElement, METHOD, METHODS, PUBLIC}
import org.opalj.br.instructions.{INVOKESTATIC, RETURN}

import scala.collection.mutable

object EntryPointClassGenerator {
  def generateEntrypointClass(entryPointMethodId: FullMethodIdentifier,
                              callerClasses: Iterable[CallerClass],
                             ): CLASS[_] = {

    CLASS(
      accessModifiers = PUBLIC.SUPER,
      thisType = entryPointMethodId.jvmClassName,
      methods = METHODS(
        generateEntryPointMethod(entryPointMethodId, callerClasses),
        generateMainMethod(entryPointMethodId)
      )
    )
  }

  private def generateEntryPointMethod[T](entryPointMethodId: FullMethodIdentifier, callerClasses: Iterable[CallerClass]): METHOD[T] = {
    val entryPointInstructions = mutable.ArrayBuilder.make[CodeElement[T]]

    callerClasses.foreach { callerClass =>
      callerClass.callerMethods.foreach { callerMethod =>
        entryPointInstructions += INVOKESTATIC(
          callerMethod.methodId.jvmClassName,
          isInterface = false,
          callerMethod.methodId.methodName,
          "()V"
        )
      }
    }

    entryPointInstructions += RETURN


    METHOD(
      PUBLIC.STATIC,
      entryPointMethodId.methodName,
      "()V",
      CODE(entryPointInstructions.result()).asInstanceOf[org.opalj.br.CodeAttributeBuilder[T]]
    )
  }

  private def generateMainMethod[T](calledMethod: FullMethodIdentifier): METHOD[T] = {
    val entryPointInstructions = Array[CodeElement[T]](
      INVOKESTATIC(calledMethod.jvmClassName, isInterface = false, calledMethod.methodName, "()V"),
      RETURN
    )

    METHOD(
      PUBLIC.STATIC,
      "main",
      "([Ljava/lang/String;)V",
      CODE(entryPointInstructions).asInstanceOf[org.opalj.br.CodeAttributeBuilder[T]]
    )
  }
}
