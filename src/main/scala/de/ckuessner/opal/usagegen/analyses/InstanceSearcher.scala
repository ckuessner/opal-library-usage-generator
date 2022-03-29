package de.ckuessner.opal.usagegen.analyses

import org.opalj.br.analyses.Project
import org.opalj.br.{ClassFile, ObjectType}
import org.opalj.collection.immutable.RefArray

import java.io.File
import java.net.URL

class InstanceSearcher(private val project: Project[_]) {
  def searchStaticNonPrivateFieldsOfType(theType: ObjectType): Iterable[StaticFieldInstanceSource] = {
    project.allFields.filter(field =>
      field.isStatic && !field.isPrivate && field.fieldType == theType
    ).map(StaticFieldInstanceSource(theType, _))
  }

  def searchNonPrivateConstructorsForType(theType: ObjectType): Iterable[ConstructorInstanceSource] = {
    project.classFile(theType).toIterable
      .filterNot(_.isAbstract)
      .flatMap(_.methods.filter(
        method => method.isConstructor && !method.isPrivate
      )).map(ConstructorInstanceSource(theType, _))
  }

  def searchStaticMethodsReturningType(theType: ObjectType): Iterable[StaticMethodInstanceSource] = {
    project.allMethods.filter(method =>
      !method.isPrivate && method.returnType == theType && !method.isConstructor && method.isStatic
    ).map(StaticMethodInstanceSource(theType, _))
  }

  // TODO: non-static fields and methods can also provide instances, they require an instance of the containing class

  /**
   * The object types that are used either as a parameter of a method, or classes that have non-private instance methods
   * in the project.
   */
  lazy val requiredObjectTypesInLibrary: Set[ObjectType] = {
    // Non-private instance methods -> require instance of classFile type
    val classesWithNonPrivateInstanceMethods = project.allProjectClassFiles.toSet.filter(classFile =>
      classFile.instanceMethods.exists(!_.isPrivate)
    ).map(_.thisType)

    // Methods taking parameters
    val usedAsParameter = project.allProjectClassFiles.toSet.flatMap((classFile: ClassFile) =>
      classFile.methods.flatMap(method =>
        // TODO: maybe filter out ObjectType.{Object, String} (and/or others)
        method.descriptor.parameterTypes.filter(param => param.isObjectType)
      ).map(_.asObjectType)
    )

    usedAsParameter union classesWithNonPrivateInstanceMethods
  }

  /**
   * The object types that are used either as a parameter of a method, or classes that have non-private instance methods
   * in the project. Also contains all the supertypes of these types.
   */
  lazy val requiredObjectTypesInLibraryWithSupertypes: Set[ObjectType] = {
    requiredObjectTypesInLibrary.flatMap { objectType =>
      project.classHierarchy.allSupertypes(objectType, reflexive = true)
    }
  }

  lazy val requiredObjectTypesInLibraryWithSubtypes: Set[ObjectType] = {
    requiredObjectTypesInLibrary.flatMap { objectType =>
      project.classHierarchy.allSubtypes(objectType, reflexive = true)
    }
  }

  lazy val typeToInstanceSourcesMap: Map[ObjectType, Array[InstanceSource]] = {
    // Collect all the direct instance sources
    val instanceSources = directInstanceSourcesForTypes(requiredObjectTypesInLibraryWithSubtypes)

    // Merge instance sources to include transitive sources
    requiredObjectTypesInLibrary.map { objectType =>
      objectType -> transitiveSourcesForType(objectType, instanceSources)
    }.toMap
  }

  def directInstanceSourcesForTypes(types: Set[ObjectType]): Map[ObjectType, Array[InstanceSource]] = types.map(
    theType => theType -> directSourcesForType(theType)
  ).toMap

  def directSourcesForType(objectType: ObjectType): Array[InstanceSource] = {
    val instanceSourcesForType = Array.newBuilder[InstanceSource]
    instanceSourcesForType ++= searchNonPrivateConstructorsForType(objectType)
    instanceSourcesForType ++= searchStaticNonPrivateFieldsOfType(objectType)
    instanceSourcesForType ++= searchStaticMethodsReturningType(objectType)
    instanceSourcesForType.result()
  }

  def transitiveSourcesForType(objectType: ObjectType, directSources: Map[ObjectType, Array[InstanceSource]]): Array[InstanceSource] = {
    project.classHierarchy.allSubtypes(objectType, reflexive = true).flatMap { subtype: ObjectType =>
      directSources.getOrElse(subtype, Array[InstanceSource]())
    }.toArray
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
