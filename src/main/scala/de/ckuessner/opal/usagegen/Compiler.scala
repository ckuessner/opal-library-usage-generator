package de.ckuessner.opal.usagegen

import org.opalj.ba.CLASS

object Compiler {
  def compile[T](code: T)(implicit compilable: Compilable[T]): ClassByteCode = {
    compilable(code)
  }
}

trait Compilable[T] {
  def apply(code: T): ClassByteCode
}

object Compilable {
  implicit def generatedClassCompiler[T <: GeneratedClass]: Compilable[T] = (generatedClass: GeneratedClass) => {
    val classFile = generatedClass.asClass.toDA._1
    val byteCode = org.opalj.bc.Assembler(classFile)
    ClassByteCode(generatedClass.jvmClassName, byteCode)
  }

  implicit def opalClassCompiler[T]: Compilable[CLASS[T]] = (clazz: CLASS[T]) => {
    val classFile = clazz.toDA._1
    val byteCode = org.opalj.bc.Assembler(classFile)
    ClassByteCode(classFile.thisType.asJVMType, byteCode)
  }
}

case class ClassByteCode(jvmClassName: String, byteCode: Array[Byte]) {
  def javaClassName: String = jvmClassName.replace("/", ".")
}