package de.ckuessner.opal.usagegen.generators.classes

import de.ckuessner.opal.usagegen._
import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers
import org.opalj.ba.{CODE, CodeElement, METHOD, PUBLIC}
import org.opalj.br.instructions.{ALOAD_0, ASTORE_0, GETSTATIC, INVOKESTATIC, INVOKEVIRTUAL, LoadString_W, RETURN}
import org.opalj.collection.immutable.RefArray

import scala.collection.mutable

object EntryPointClassGenerator {

  val entryPointMethodName: String = "performCalls"

  def generateEntrypointClass(entryPointMethodId: FullMethodIdentifier,
                              callerClasses: Iterable[CallerClass],
                             ): EntryPointClass = {

    EntryPointClass(
      entryPointMethodId.packageName,
      entryPointMethodId.simpleClassName,
      mainMethod = generateDelegatingMainMethod(delegateFor = entryPointMethodId),
      entryPointMethod = generateEntryPointMethod(entryPointMethodId, callerClasses)
    )
  }

  def generateCallerClassEntryPointMethod(packageName: String, className: String, callerMethods: RefArray[CallerMethod]): EntryPointMethod = {
    val callCallersInstructions = mutable.ArrayBuilder.make[CodeElement[Nothing]]()

    callerMethods.foreach { callerMethod =>
      callCallersInstructions += INVOKESTATIC(
        callerMethod.methodId.jvmClassName,
        isInterface = false,
        callerMethod.methodId.methodName,
        "()V"
      )
    }

    callCallersInstructions += RETURN

    val body = ByteCodeGenerationHelpers.tryCatchBlock(
      callCallersInstructions.result(),
      RefArray[CodeElement[Nothing]](
        ASTORE_0, // Store exception
        GETSTATIC("java/lang/System", "err", "Ljava/io/PrintStream;"),
        LoadString_W(s"Caught Exception in $packageName$className}.$entryPointMethodName"),
        INVOKEVIRTUAL("java/io/PrintStream", "println", "(Ljava/lang/String;)V"),
        ALOAD_0, // Load exception
        INVOKEVIRTUAL("java/lang/Throwable", "printStackTrace", "()V"),
        RETURN
      ), Symbol("entryPointMethodException")
    )

    EntryPointMethod(
      FullMethodIdentifier(packageName, className, entryPointMethodName, "()V"),
      METHOD(
        PUBLIC.STATIC,
        entryPointMethodName,
        "()V",
        CODE(body)
      )
    )
  }

  private def generateEntryPointMethod[T](entryPointMethodId: FullMethodIdentifier, callerClasses: Iterable[CallerClass]): EntryPointMethod = {
    val entryPointInstructions = mutable.ArrayBuilder.make[CodeElement[T]]

    callerClasses.foreach { callerClass =>
      entryPointInstructions += INVOKESTATIC(callerClass.jvmClassName, isInterface = false, entryPointMethodName, "()V")
    }
    entryPointInstructions += RETURN

    EntryPointMethod(
      entryPointMethodId,
      METHOD(
        PUBLIC.STATIC,
        entryPointMethodId.methodName,
        "()V",
        CODE(entryPointInstructions.result()).asInstanceOf[org.opalj.br.CodeAttributeBuilder[T]]
      )
    )
  }

  private def generateDelegatingMainMethod[T](delegateFor: FullMethodIdentifier): AuxiliaryMethod = {
    AuxiliaryMethod(
      delegateFor.copy(methodName = "main", descriptor = "([Ljava/lang/String;)V"),
      METHOD(
        PUBLIC.STATIC,
        "main",
        "([Ljava/lang/String;)V",
        CODE(
          INVOKESTATIC(delegateFor.fqnClassName, isInterface = false, delegateFor.methodName, "()V"),
          RETURN
        )
      )

    )
  }
}
