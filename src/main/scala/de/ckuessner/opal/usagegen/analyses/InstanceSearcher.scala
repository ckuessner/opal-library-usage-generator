package de.ckuessner.opal.usagegen.analyses

import org.opalj.br.analyses.Project
import org.opalj.br.{ClassFile, ObjectType}

import java.io.File
import java.net.URL

class InstanceSearcher(private val project: Project[_]) {
  // TODO: Include subtypes instead of only exact type matches when searching for instance sources

  def searchStaticNonPrivateFieldsOfType(theType: ObjectType): Iterable[StaticFieldInstanceSource] = {
    project.allFields.filter(field =>
      field.isStatic && !field.isPrivate && field.fieldType == theType
    ).map(StaticFieldInstanceSource(theType, _))
  }

  def searchNonPrivateConstructorsForType(theType: ObjectType): Iterable[ConstructorInstanceSource] = {
    project.classFile(theType).toIterable.flatMap(_.methods.filter(
      method => method.isConstructor && !method.isPrivate
    )).map(ConstructorInstanceSource(theType, _))
  }

  def searchStaticMethodsReturningType(theType: ObjectType): Iterable[StaticMethodInstanceSource] = {
    project.allMethods.filter(method =>
      method.returnType == theType && !method.isConstructor && method.isStatic
    ).map(StaticMethodInstanceSource(theType, _))
  }

  /**
   * Searches all object types that are used either as a parameter of a method, or classes that have non-private instance methods in the project.
   */
  lazy val requiredInstanceTypesInLibrary: Set[ObjectType] = {
    // Non-private instance methods -> require instance of classFile type
    val classesWithNonPrivateInstanceMethods = project.allProjectClassFiles.toSet.filter(classFile =>
      classFile.instanceMethods.exists(!_.isPrivate)
    ).map(_.thisType)

    // Methods taking parameters
    val usedAsParameter = project.allProjectClassFiles.toSet.flatMap((classFile: ClassFile) =>
      classFile.methods.flatMap(method =>
        method.descriptor.parameterTypes.filter(param => param.isObjectType) // TODO: maybe ignore ObjectType.String
      ).map(_.asObjectType)
    )

    usedAsParameter union classesWithNonPrivateInstanceMethods
  }

  lazy val typeToInstanceSourcesMap: Map[ObjectType, Seq[InstanceSource]] = {
    requiredInstanceTypesInLibrary.map(theType => {
      val instanceSources = List.newBuilder[InstanceSource]
      instanceSources ++= searchNonPrivateConstructorsForType(theType)
      instanceSources ++= searchStaticNonPrivateFieldsOfType(theType)
      instanceSources ++= searchStaticMethodsReturningType(theType)
      theType -> instanceSources.result()
    }).toMap
  }
}

object InstanceSearcher {
  def apply(project: Project[URL]): InstanceSearcher = {
    new InstanceSearcher(project)
  }

  def apply(libraryJar: File, runTimeJars: Seq[File]): InstanceSearcher = {
    InstanceSearcher(
      Project(
        Array(libraryJar),
        runTimeJars.toArray
      )
    )
  }
}
