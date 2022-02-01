package de.ckuessner.opal.usagegen.generators

import de.ckuessner.opal.usagegen.CallerClass
import de.ckuessner.opal.usagegen.generators.ClassGenerator.generatePublicClass
import org.opalj.ba.{CLASS, CODE, CodeElement, InstructionElement, METHOD, METHODS, PUBLIC}
import org.opalj.br.instructions.{INVOKESTATIC, RETURN}

import scala.collection.mutable

object EntryPointClassGenerator {
  def generateEntrypointClass(entryPointPackage: String,
                              entryPointClass: String,
                              entryPointMethodName: String,
                              callerClasses: Iterable[CallerClass],
                             ): CLASS[_] = {

    val entryPointInstructions = mutable.ArrayBuilder.make[CodeElement[InstructionElement]]

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

    generatePublicClass(
      entryPointPackage,
      entryPointClass,
      METHODS(METHOD(
        PUBLIC.STATIC,
        entryPointMethodName,
        "()V",
        CODE(entryPointInstructions.result())
      ))
    )
  }

}