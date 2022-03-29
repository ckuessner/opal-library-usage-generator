package de.ckuessner.opal.usagegen.analyses

import org.opalj.bi.ACC_PRIVATE
import org.opalj.br.ObjectType
import org.opalj.br.analyses.Project
import org.opalj.collection.immutable.RefArray
import org.opalj.log.DevNullLogger
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.{be, defined}
import org.scalatest.matchers.should.Matchers.{contain, convertToAnyShouldWrapper, empty, not}

import java.net.URL
import java.nio.file.Paths
import scala.language.implicitConversions

class InstanceSearcherTest extends AnyFlatSpec with BeforeAndAfterAll {
  var project: Project[URL] = null
  var instanceSearcher: InstanceSearcher = null

  implicit def stringToObjectType(obj: String): ObjectType = ObjectType(obj)

  override protected def beforeAll(): Unit = {
    val jarFile = Paths.get(getClass.getResource("/instancesearchertest.jar").toURI)
    project = Project(jarFile.toFile, DevNullLogger)
    instanceSearcher = InstanceSearcher(project, RefArray.empty)
  }

  val AbstractClassTest: ObjectType = ObjectType("instancesearchertest/AbstractClassTest")
  val InheritanceTest: ObjectType = ObjectType("instancesearchertest/InheritanceTest")
  val InheritanceTestSubclass: ObjectType = ObjectType("instancesearchertest/InheritanceTest$Subclass")
  val InterfaceTest: ObjectType = ObjectType("instancesearchertest/InterfaceTest")
  val ClassImplementingInterface: ObjectType = ObjectType("instancesearchertest/InterfaceTest$ClassImplementingInterface")
  val SubInterface: ObjectType = ObjectType("instancesearchertest/InterfaceTest$SubInterface")
  val ClassImplementingSubInterface: ObjectType = ObjectType("instancesearchertest/InterfaceTest$ClassImplementingSubInterface")

  val parametersInClassThatUsesAllOtherClasses: Set[ObjectType] = Set(
    "instancesearchertest/AbstractClassTest",
    "instancesearchertest/ConstructorTest",
    "instancesearchertest/InheritanceTest",
    "instancesearchertest/InnerClassTest",
    "instancesearchertest/StaticFieldTest",
    "instancesearchertest/StaticMethodTest",
    "instancesearchertest/InterfaceTest$SubInterface",
  )

  "requiredObjectTypesInLibrary" should "match ClassThatUsesAllOtherClasses/takesParameters" in {
    instanceSearcher.requiredObjectTypesInLibrary shouldEqual parametersInClassThatUsesAllOtherClasses
  }

  it should "work for interfaces" in {
    instanceSearcher.requiredObjectTypesInLibrary should contain(ObjectType("instancesearchertest/InterfaceTest$SubInterface"))
  }

  "requiredObjectTypesInLibraryWithSupertypes" should "be correct for test jar" in {
    instanceSearcher.requiredObjectTypesInLibraryWithSupertypes shouldEqual parametersInClassThatUsesAllOtherClasses ++ Seq[ObjectType](
      ObjectType.Object,
      "instancesearchertest/InheritanceTest",
      "instancesearchertest/InterfaceTest",
    )
  }

  it should "work for interfaces" in {
    instanceSearcher.requiredObjectTypesInLibraryWithSupertypes should contain allOf(
      ObjectType("instancesearchertest/InterfaceTest"),
      ObjectType("instancesearchertest/InterfaceTest$SubInterface")
    )
  }

  "requiredObjectTypesInLibraryWithSubtypes" should "work for subtypes" in {
    instanceSearcher.requiredObjectTypesInLibraryWithSubtypes should not contain (
      ObjectType.Object
      )

    instanceSearcher.requiredObjectTypesInLibraryWithSubtypes should contain allOf(
      ObjectType("instancesearchertest/InheritanceTest$Subclass"),
      ObjectType("instancesearchertest/InheritanceTest"),
    )

    instanceSearcher.requiredObjectTypesInLibraryWithSubtypes shouldEqual parametersInClassThatUsesAllOtherClasses ++ Seq[ObjectType](
      ObjectType("instancesearchertest/InheritanceTest$Subclass"),
      ObjectType("instancesearchertest/InterfaceTest$ClassImplementingSubInterface")
    )
  }

  "searchNonPrivateConstructorsForType" should "work for non-abstract classes" in {
    val sources = instanceSearcher.searchNonPrivateConstructorsForType("instancesearchertest/ConstructorTest")

    sources.find {
      case ConstructorInstanceSource(ObjectType("instancesearchertest/ConstructorTest"), constructorMethod) => constructorMethod.isPublic
    } should not be empty

    sources.find {
      case ConstructorInstanceSource(ObjectType("instancesearchertest/ConstructorTest"), constructorMethod) => constructorMethod.isProtected
    } should not be empty

    sources.find {
      case ConstructorInstanceSource(ObjectType("instancesearchertest/ConstructorTest"), constructorMethod) => constructorMethod.isPackagePrivate
    } should not be empty

    sources.find {
      case ConstructorInstanceSource(ObjectType("instancesearchertest/ConstructorTest"), constructorMethod) => constructorMethod.isPrivate
    } shouldBe empty

    sources.find {
      case ConstructorInstanceSource(ObjectType("instancesearchertest/ConstructorTest"), constructorMethod) =>
        constructorMethod.isPublic && constructorMethod.parameterTypes.size == 1
    } should not be empty

    sources.find {
      case ConstructorInstanceSource(ObjectType("instancesearchertest/ConstructorTest"), constructorMethod) =>
        constructorMethod.isPublic && constructorMethod.parameterTypes.size == 0
    } should not be empty

    sources.size shouldEqual 4
  }

  it should "not find any constructors for abstract classes" in {
    instanceSearcher.typeToInstanceSourcesMap.foreach { case (_, sources) =>
      sources.filter {
        case ConstructorInstanceSource(_, method) => method.classFile.isAbstract
        case _ => false
      } shouldBe empty
    }
  }

  "searchStaticMethodsReturningType" should "find static method sources in abstract class" in {
    val objType: ObjectType = "instancesearchertest/AbstractClassTest"
    val sources = instanceSearcher.searchStaticMethodsReturningType(objType)

    sources.find {
      case StaticMethodInstanceSource(`objType`, staticMethod) =>
        staticMethod.parameterTypes.isEmpty && staticMethod.isStatic && !staticMethod.isPrivate
    } should not be empty
  }

  "searchStaticNonPrivateFieldsOfType" should "find static field sources in abstract class" in {
    val objType: ObjectType = "instancesearchertest/AbstractClassTest"
    val sources = instanceSearcher.searchStaticNonPrivateFieldsOfType(objType)

    sources.find {
      case StaticFieldInstanceSource(`objType`, field) => field.name == "staticField" && field.isPublic && field.isStatic
    } should not be empty

    sources.find {
      case StaticFieldInstanceSource(`objType`, field) => field.isPrivate || field.isNotStatic
    } shouldBe empty
  }

  "typeToInstanceSourcesMap" should "not contain abstract class constructors" in {
    val sources = instanceSearcher.typeToInstanceSourcesMap.get(AbstractClassTest)
    sources shouldNot be(empty)
    sources.get.foreach {
      case cis@ConstructorInstanceSource(_, _) => fail(s"Contained instance source to constructor of abstract class: $cis")
      case _ =>
    }
  }

  it should "contain static field sources for abstract classes" in {
    val sources = instanceSearcher.typeToInstanceSourcesMap.get(AbstractClassTest)
    sources shouldNot be(empty)
    sources.get shouldNot be(empty)

    sources.get.count {
      case StaticFieldInstanceSource(`AbstractClassTest`, field) =>
        field.classFile.thisType == AbstractClassTest && field.isStatic && field.name == "staticField"
      case _ => false
    } shouldEqual 1
  }

  it should "not contain non-static sources" in {
    instanceSearcher.typeToInstanceSourcesMap.foreach { case (_, sources) =>
      sources.filter {
        case StaticFieldInstanceSource(_, field) => field.isNotStatic
        case StaticMethodInstanceSource(_, staticMethod) => staticMethod.isNotStatic
        case _ => false
      } shouldBe empty
    }
  }

  it should "not contain private sources" in {
    instanceSearcher.typeToInstanceSourcesMap.foreach { case (_, sources) =>
      sources.filter {
        case StaticFieldInstanceSource(_, field) => field.isPrivate
        case StaticMethodInstanceSource(_, staticMethod) => staticMethod.isPrivate
        case ConstructorInstanceSource(_, constructorMethod) => constructorMethod.isPrivate
        case StubSubclassInstanceSource(instanceType, stubClass, stubClassConstructor) => false // TODO: Check whether superconstructor is private
      } shouldBe empty
    }
  }

  it should "not contain sources from private classes" in {
    instanceSearcher.typeToInstanceSourcesMap.foreach { case (_, sources) =>
      sources.map {
        case StaticFieldInstanceSource(_, field) => field.classFile
        case StaticMethodInstanceSource(_, staticMethod) => staticMethod.classFile
        case ConstructorInstanceSource(_, constructorMethod) => constructorMethod.classFile
        case StubSubclassInstanceSource(_, stubClass, _) => project.classFile(stubClass.abstractSuperClass.thisType).get
      }.count {
        cf => (cf.accessFlags & ACC_PRIVATE.mask) != 0
      } shouldEqual 0
    }
  }

  it should "contain static method sources for abstract classes" in {
    val sources = instanceSearcher.typeToInstanceSourcesMap.get(AbstractClassTest)
    sources shouldBe defined
    sources.get.count {
      case StaticMethodInstanceSource(`AbstractClassTest`, method) =>
        method.classFile.thisType == AbstractClassTest && method.isStatic && method.name == "staticMethod"
      case _ => false
    } shouldEqual 1
  }

  it should "contain sources for subtypes of classes" in {
    val instances = instanceSearcher.typeToInstanceSourcesMap.get(InheritanceTest)
    instances shouldBe defined

    instances.get count {
      case ConstructorInstanceSource(`InheritanceTestSubclass`, _) => true
      case _ => false
    } shouldEqual 1

    instances.get count {
      case StaticFieldInstanceSource(`InheritanceTest`, _) => true
      case _ => false
    } shouldEqual 1
  }

  it should "contain subtypes for interfaces" in {
    val directSources = instanceSearcher.directInstanceSourcesForTypes(Set(InterfaceTest, SubInterface, ClassImplementingInterface, ClassImplementingSubInterface))
    val instances = instanceSearcher.transitiveSourcesForType(InterfaceTest, directSources)

    instances.count {
      case ConstructorInstanceSource(`ClassImplementingInterface`, _) => true
      case _ => false
    } shouldEqual 1

    instances count {
      case ConstructorInstanceSource(`ClassImplementingSubInterface`, _) => true
      case _ => false
    } shouldEqual 1

    instances count {
      case StaticFieldInstanceSource(`ClassImplementingSubInterface`, _) => true
      case _ => false
    } shouldEqual 1

    instances count {
      case StaticFieldInstanceSource(`InterfaceTest`, _) => true
      case _ => false
    } shouldEqual 1

    instances.length shouldEqual 4
  }
}
