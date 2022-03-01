package de.ckuessner.opal.usagegen

import de.ckuessner.opal.usagegen.Compilable.{generatedClassCompiler, opalClassCompiler}
import de.ckuessner.opal.usagegen.analyses.InstanceSearcher
import de.ckuessner.opal.usagegen.generators._
import org.opalj.br.analyses.Project
import org.opalj.log.{ConsoleOPALLogger, GlobalLogContext, OPALLogger}
import scopt.OParser

import java.io.File
import java.lang.reflect.InvocationTargetException
import java.util.ResourceBundle
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader

object UsageGeneratorCli extends App {

  case class Config(projectJarFile: File,
                    outputJarFile: File,
                    force: Boolean = false,
                    runBytecode: Boolean = false,
                    runtimeJars: Seq[File] = Seq.empty,
                    verbose: Boolean = false,
                    callerClassName: String = "___METHOD_CALLER___",
                    instanceProviderClassName: String = "___INSTANCE_PROVIDER___",
                    sinkClassPackage: String = "",
                    sinkClassName: String = "___SINK___",
                    entryPointClassPackage: String = "",
                    entryPointClassName: String = "___TEST_RUNNER_ENTRYPOINT___",
                    entryPointMethodName: String = "run"
                   )

  val builder = OParser.builder[Config]
  val argParser = {
    import builder._
    OParser.sequence(
      arg[File]("libraryJarFile")
        .text("Path to the JAR file containing the tested library")
        .required()
        .validate(file =>
          if (file.exists()) success
          else failure("File " + file.toString + " does not exist")
        )
        .action((file, c) => c.copy(projectJarFile = file)),

      arg[File]("outputJarFile")
        .text("Path to the output JAR file that contains the generated usage code")
        .required()
        .validate(file =>
          if (file.getName.matches(".*\\.(jar|zip)"))
            success
          else
            failure("outputJarFile must end with .jar or .zip")
        ).action((file, c) => c.copy(outputJarFile = file)),

      opt[Unit]('f', "force")
        .text("Overwrite outputJarFile if it already exists")
        .optional()
        .action((_, c) => c.copy(force = true)),

      opt[Unit]('r', "run")
        .text("Run generated bytecode after generation")
        .optional()
        .action((_, c) => c.copy(runBytecode = true)),

      opt[Seq[File]]("runtime-jars")
        .text("list of jars containing the runtime dependencies of the tested library")
        .valueName("<jar1>,<jar2>...")
        .validate(jarFiles => {
          val nonExistentJarFile = jarFiles.find(jarFile => !jarFile.exists())
          nonExistentJarFile match {
            case Some(jarFile) => failure("File " + jarFile.toString + " does not exist")
            case None => success
          }
        })
        .action((files, config) => config.copy(runtimeJars = files)),

      opt[Unit]('v', "verbose")
        .text("Increase logging verbosity")
        .optional()
        .action((_, c) => c.copy(verbose = true)),

      help('h', "help")
        .text("Print this usage text"),

      checkConfig(c =>
        if (!c.force && c.outputJarFile != null && c.outputJarFile.exists())
          failure("File " + c.outputJarFile + " already exists, use -f to override outputJarFile")
        else
          success
      )
    )
  }

  // OParser.parse returns Option[Config]
  OParser.parse(argParser, args, Config(null, null)) match {
    case Some(config) => run(config)
    case _ => // Invalid parameters, errors logged by scopt
  }

  def run(config: Config): Unit = {
    // Silence Info level logs from opal
    val opalLogLevel =
      if (config.verbose) org.opalj.log.Info
      else org.opalj.log.Warn

    OPALLogger.updateLogger(GlobalLogContext, new ConsoleOPALLogger(true, opalLogLevel))
    val project = Project(
      projectFiles = Array(config.projectJarFile),
      libraryFiles = config.runtimeJars.toArray
      //, new ConsoleOPALLogger(true, opalLogLevel)
    )

    // Extract sources of instances for types consumed by library methods (either as parameter, or as instance for instance methods)
    val instanceSourcesMap = InstanceSearcher(project).typeToInstanceSourcesMap
    // Generate classes with methods providing instances for used types
    val instanceProviderGenerator = InstanceProviderGenerator(DefaultValueLoadingGenerator.defaultValuesForFieldTypes, config.instanceProviderClassName)
    val instanceProviderClasses = instanceProviderGenerator.generateInstanceProviderClasses(instanceSourcesMap.flatMap(_._2))

    // Generate caller classes that actually call the library functions
    val usageGenerator = new UsageGenerator(project, new MethodCallGenerator(instanceProviderClasses.typeToProviderMethodMap), config.callerClassName, config.sinkClassPackage, config.sinkClassName)
    val callerClasses = usageGenerator.generateDummyUsage
    // Generate sink class containing all sink methods used by caller methods
    val sinkClass = SinkGenerator.generateSinkClass(config.sinkClassPackage, config.sinkClassName, callerClasses)
    // Generate entry point class that calls all caller methods (avoiding use of reflection)
    val entryPointClass = EntryPointClassGenerator.generateEntrypointClass(
      FullMethodIdentifier(config.entryPointClassPackage, config.entryPointClassName, config.entryPointMethodName, "()V"),
      callerClasses
    )

    // Class that is called when generated jar is run using MANIFEST main. Calls all caller classes.
    val compiledEntryPointClass = Compiler.compile(entryPointClass)
    // Class that contains all sink methods.
    val compiledSinkClass = Compiler.compile(sinkClass)
    // Classes with methods that each call one method of the tested library.
    val compiledCallerClasses = callerClasses.map(Compiler.compile(_)).toList
    // Classes that contain methods that return an instance of a specific type.
    val compiledInstanceProviderClasses = instanceProviderClasses.providerClasses.map(Compiler.compile(_))

    val classes = compiledEntryPointClass :: compiledSinkClass :: (compiledCallerClasses ++ compiledInstanceProviderClasses)

    JarFileGenerator.writeClassFilesToJarFile(
      config.outputJarFile,
      classes,
      config.force,
      Some(compiledEntryPointClass.javaClassName)
    )

    if (config.runBytecode) {
      ResourceBundle.clearCache(ClassLoader.getSystemClassLoader)

      val classPath = Seq(
        config.projectJarFile, // Classloader needs both library jar file
        config.outputJarFile // as well as generated jar file
      ) ++: config.runtimeJars // Add the runtime jars (i.e., the runtime dependencies specified by --runtime-jars)

      val testedProjectClassLoader: ClassLoader = new URLClassLoader(classPath.map(_.toURI.toURL), null)
      ResourceBundle.clearCache(testedProjectClassLoader)

      val entryPointFqnClassName =
        if (config.entryPointClassPackage.isEmpty) config.entryPointClassName
        else config.entryPointClassPackage + '.' + config.entryPointClassName

      val entryPointClass = testedProjectClassLoader.loadClass(entryPointFqnClassName)
      val entryPointMethod = entryPointClass.getMethod(config.entryPointMethodName)
      try {
        entryPointMethod.invoke(null)
      } catch {
        case e: InvocationTargetException =>
          System.err.println(entryPointMethod.getName + " threw " + e.getTargetException.toString)
      }
    }
  }
}
