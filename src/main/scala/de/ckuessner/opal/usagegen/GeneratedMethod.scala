package de.ckuessner.opal.usagegen

import de.ckuessner.opal.usagegen.analyses.InstanceSource
import de.ckuessner.opal.usagegen.generators.ByteCodeGenerationHelpers.{fqnIllegalCharsPattern, packageAndClassToJvmClassName, unqualifiedNameIllegalCharsPattern}
import org.opalj.ba.METHOD
import org.opalj.br.ObjectType

sealed trait GeneratedMethod {
  def methodId: FullMethodIdentifier

  def methodBody: METHOD[_]
}

case class CallerMethod(methodId: FullMethodIdentifier, methodBody: METHOD[_], sink: SinkMethod, exceptionSink: SinkMethod) extends GeneratedMethod

case class SinkMethod(methodId: FullMethodIdentifier, methodBody: METHOD[_]) extends GeneratedMethod

case class InstanceProviderMethod(methodId: FullMethodIdentifier, methodBody: METHOD[_], instanceSource: InstanceSource) extends GeneratedMethod {
  def instanceType: ObjectType = instanceSource.instanceType
}

case class FullMethodIdentifier private(packageName: String, simpleClassName: String, methodName: String, signature: String) {
  def jvmClassName: String = packageAndClassToJvmClassName(packageName, simpleClassName)

  def fqnClassName: String =
    if (packageName.isEmpty) simpleClassName
    else packageName + "." + simpleClassName
}

object FullMethodIdentifier {
  // NOTE: This is merely a sanity check, some illegal parameters are accepted (e.g. signature is unchecked)
  def apply(packageName: String, className: String, methodName: String, signature: String): FullMethodIdentifier = {
    if (fqnIllegalCharsPattern.matcher(packageName).matches()) {
      throw new IllegalArgumentException(packageName + " is an invalid package name. (Hint: don't use the jvm representation, but the standard java package notation)")
    }

    if (unqualifiedNameIllegalCharsPattern.matcher(className).matches()) {
      throw new IllegalArgumentException(className + " is an invalid class name")
    }

    if (unqualifiedNameIllegalCharsPattern.matcher(methodName).matches()
      || "<init>".equals(methodName)
      || "<clinit>".equals(methodName)
    ) {
      throw new IllegalArgumentException(methodName + " is an invalid method name")
    }

    // TODO: The signature could also be checked

    new FullMethodIdentifier(packageName, className, methodName, signature)
  }

}
