package daikon.dcomp;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.DynComp;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.options.Option;
import daikon.plumelib.options.Options;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URI;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import org.apache.bcel.*;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.generic.*;

/**
 * BuildJDK uses {@link DCInstrument} to add comparability instrumentation to a set of Java class
 * files and then stores the modified files into a directory identified by a (required) command line
 * argument.
 *
 * <p>DCInstrument duplicates each method of a class file. The new methods are distinguished by the
 * addition of a final parameter of type DCompMarker and are instrumented to track comparability.
 * Based on its invocation arguments, DynComp will decide whether to call the instrumented or
 * uninstrumented version of a method.
 */
public class BuildJDK {

  /**
   * The "java.home" system property. Note that there is also a JAVA_HOME variable that contains
   * {@code System.getenv("JAVA_HOME")}.
   */
  public static final String java_home = System.getProperty("java.home");

  /**
   * Instrument the class files given on the command line instead of the ones in the Java runtime
   * library.
   */
  @Option("Instrument only the class files given on the command line")
  public static boolean classfiles = false;

  /**
   * Used when debugging to terminate processing if an error occurs. This flag is tested in
   * DCInstrument.
   */
  @Option("Halt if an instrumentation error occurs")
  public static boolean quit_if_error = false;

  /** Print information about the classes being instrumented. */
  private static boolean verbose = false;

  /** Number of class files processed; used for progress display. */
  private int _numFilesProcessed = 0;

  /** Name of entry in output jar containing the statics map. */
  private static String static_field_id_fname = "dcomp_jdk_static_field_id";

  /** Names of all the classes processed. */
  private static List<String> all_classes = new ArrayList<>();

  /** Names of all methods that DCInstrument could not process. Should be empty. */
  private static List<String> skipped_methods = new ArrayList<>();

  /**
   * A list of methods known to cause DCInstrument to fail. This is used to remove known problems
   * from the list of failures displayed at the end of BuildJDK's execution.
   */
  public static List<String> known_uninstrumentable_methods =
      Arrays.asList(
          // None at present
          );

  /**
   * We want to share code to read and instrument the Java class file members of a jar file (JDK 8)
   * or a module file (JDK 9+). However, jar files and module files are located in two completely
   * different file systems. So we open an InputStream for each class file we wish to instrument and
   * save it the the class_stream_map. From that point the code to instrument a class file can be
   * shared.
   */
  private Map<String, InputStream> class_stream_map = new HashMap<>();

  /**
   * Instruments each class file in the Java runtime and puts the result in the first non-option
   * command-line argument.
   *
   * <p>By default, BuildJDK will locate the appropriate Java runtime library and instrument each of
   * its member class files. If the optional --classfiles argument is present, then the class files
   * to be instrumented are given on the command line and the Java runtime library is not used. This
   * usage is primarily for debugging purposes.
   *
   * @param args arguments being passed to BuildJDK
   * @throws IOException if unable to read or write dcomp_jdk_static_field_id or if unable to write
   *     jdk_classes.txt
   */
  public static void main(String[] args) throws IOException {

    System.out.println("BuildJDK starting at " + new Date());

    Options options =
        new Options(
            "daikon.BuildJDK [options] dest_dir [classfiles...]", BuildJDK.class, DynComp.class);
    String[] cl_args = options.parse(true, args);
    check_args(options, cl_args);
    verbose = DynComp.verbose;

    BuildJDK build = new BuildJDK();
    File dest_dir = new File(cl_args[0]);

    if (classfiles) {

      // Arguments are <destdir> <classfiles>...
      File[] class_files = new File[cl_args.length - 1];
      for (int i = 1; i < cl_args.length; i++) {
        class_files[i - 1] = new File(cl_args[i]);
      }

      // The classfiles argument is usually used for testing.
      // But if we're using it to fix a broken classfile, then we need
      // to restore the static map from when our runtime jar was originally
      // built.  We assume it is in the destination directory.
      DCInstrument.restore_static_field_id(new File(dest_dir, static_field_id_fname));
      System.out.printf(
          "Restored %d entries in static map.%n", DCInstrument.static_field_id.size());

      // Read in each specified classfile
      for (File class_file : class_files) {
        String classFileName = class_file.toString();
        if (classFileName.endsWith("java/lang/Object.class")) {
          System.out.printf("Skipping %s%n", classFileName);
          continue;
        }

        // Convert the class file to internal BCEL form.
        JavaClass jc;
        try {
          ClassParser parser = new ClassParser(classFileName);
          jc = parser.parse();
        } catch (Throwable e) {
          throw new Error("Failed to parse classfile " + classFileName, e);
        }

        // Instrument the class file.
        try {
          build.instrumentClassFile(jc, dest_dir, classFileName);
        } catch (Throwable e) {
          throw new Error("Couldn't instrument " + classFileName, e);
        }
      }

    } else {

      // Collect the Java runtime classes we want to instrument in class_stream_map.
      if (BcelUtil.javaVersion > 8) {
        build.gather_runtime_from_modules();
      } else {
        build.gather_runtime_from_jar();
      }

      // Instrument the Java runtime classes we have stored in class_stream_map.
      build.instrument_classes(dest_dir);

      // Write out the static map
      System.out.printf("Found %d statics.%n", DCInstrument.static_field_id.size());
      DCInstrument.save_static_field_id(new File(dest_dir, static_field_id_fname));

      // Write out the list of all classes in the jar file
      File jdk_classes_dir = new File(dest_dir, "java/lang");
      File jdk_classes_file = new File(jdk_classes_dir, "jdk_classes.txt");
      System.out.printf("Writing all classes to %s%n", jdk_classes_file);
      try (PrintWriter pw = new PrintWriter(jdk_classes_file, UTF_8.name())) {
        pw.println("no_primitives: " + DynComp.no_primitives);
        for (String classname : all_classes) {
          pw.println(classname);
        }
      }

      // Print out any methods that could not be instrumented
      print_skipped_methods();
    }
    System.out.println("BuildJDK done at " + new Date());
  }

  /**
   * Check the non-option command-line arguments for legality. Verify java.home and JAVA_HOME match.
   * Exits the JVM if there is an error.
   *
   * @param options describes the legal command-line options to BuildJDK
   * @param target_args command-line arguments, after all options have been removed
   */
  public static void check_args(Options options, String[] target_args) {

    if (classfiles) {
      if (target_args.length < 1) {
        System.out.println("must specify destination dir");
        options.printUsage();
        System.exit(1);
      }
      if (target_args.length < 2) {
        System.out.println("must specify classfiles to instrument");
        options.printUsage();
        System.exit(1);
      }
    } else {
      if (target_args.length < 1) {
        System.out.println("must specify destination dir");
        options.printUsage();
        System.exit(1);
      }
      if (target_args.length > 1) {
        System.out.println("too many arguments: found " + target_args.length + ", expected 1");
        options.printUsage();
        System.exit(1);
      }
    }

    String JAVA_HOME = System.getenv("JAVA_HOME");
    if (JAVA_HOME == null) {
      if (verbose) {
        System.out.println("JAVA_HOME not defined; using java.home: " + java_home);
      }
      JAVA_HOME = java_home;
    }

    File jrt = new File(JAVA_HOME);
    if (!jrt.exists()) {
      System.out.printf("Java home directory %s does not exist.%n", jrt);
      System.exit(1);
    }

    try {
      jrt = jrt.getCanonicalFile();
    } catch (Exception e) {
      System.out.printf("Error geting canonical file for %s: %s", jrt, e.getMessage());
      System.exit(1);
    }

    JAVA_HOME = jrt.getAbsolutePath();
    if (!java_home.startsWith(JAVA_HOME)) {
      System.out.printf(
          "JAVA_HOME (%s) does not agree with java.home (%s).%n", JAVA_HOME, java_home);
      System.out.printf("Please correct your Java environment.%n");
      System.exit(1);
    }
  }

  /**
   * For Java 8 the Java runtime is located in rt.jar. This method creates an InputStream for each
   * class in rt.jar and saves this information in {@link #class_stream_map}.
   */
  void gather_runtime_from_jar() {

    String jar_name = java_home + "/lib/rt.jar";
    System.out.printf("using jar file %s%n", jar_name);
    try {
      JarFile jfile = new JarFile(jar_name);
      // Get each class to be instrumented and store it away
      Enumeration<JarEntry> entries = jfile.entries();
      while (entries.hasMoreElements()) {
        JarEntry entry = entries.nextElement();
        // System.out.printf("processing entry %s%n", entry);
        final String entryName = entry.getName();
        if ((entryName.endsWith("/")) || (entryName.endsWith("~"))) {
          continue;
        }

        // Get the InputStream for this file
        InputStream is = jfile.getInputStream(entry);
        class_stream_map.put(entryName, is);
      }
    } catch (Exception e) {
      throw new Error("Problem while reading " + jar_name, e);
    }
  }

  /**
   * For Java 9+ the Java runtime is located in a series of modules. At this time, we are only
   * pre-instrumenting the java.base module. This method initializes the DirectoryStream used to
   * explore java.base. It calls gather_runtime_from_modules_directory to process the directory
   * structure.
   */
  void gather_runtime_from_modules() {

    FileSystem fs = FileSystems.getFileSystem(URI.create("jrt:/"));
    Path modules = fs.getPath("/modules");
    System.out.printf("using modules file %s%n", java_home + "/lib/modules");
    try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(modules, "java.base*")) {
      for (Path module : directoryStream) {
        gather_runtime_from_modules_directory(module, module.toString().length());
      }
    } catch (IOException e) {
      throw new Error(e);
    }
  }

  /**
   * This is a helper method for gather_runtime_from_modules. It recurses down the module directory
   * tree, selects the classes we want to instrument, creates an InputStream for each of these
   * classes and saves this information in the class_stream_map.
   *
   * @param path path to module file, which might be subdirectory
   * @param modulePrefixLength length of "/module/..." path prefix before start of actual member
   *     path
   */
  void gather_runtime_from_modules_directory(Path path, int modulePrefixLength) {

    if (Files.isDirectory(path)) {
      try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(path)) {
        for (Path subpath : directoryStream) {
          gather_runtime_from_modules_directory(subpath, modulePrefixLength);
        }
      } catch (IOException e) {
        throw new Error(e);
      }
    } else {
      try {
        String pathString = path.toString();
        final String entryName = pathString.substring(modulePrefixLength + 1);
        // Get the InputStream for this file
        InputStream is = Files.newInputStream(path);
        class_stream_map.put(entryName, is);
      } catch (Exception e) {
        throw new Error(e);
      }
    }
  }

  void instrument_classes(File dest_dir) {

    try {
      // Create the destination directory
      dest_dir.mkdirs();

      // Process each file.
      for (String className : class_stream_map.keySet()) {
        if (verbose) System.out.println("instrument_classes: " + className);

        if (className.equals("module-info.class")) {
          System.out.printf("Skipping file %s%n", className);
          continue;
        }

        if (className.endsWith(".class")) {
          all_classes.add(className.replace(".class", ""));
        }
        if (!className.endsWith(".class") || className.equals("java/lang/Object.class")) {
          // Copy non-.class files (and Object.class) unchanged (JDK 8).
          // For JDK 9+ we do not copy as these items will be loaded from the original module file.
          File destfile = new File(className);
          if (destfile.getParent() == null || BcelUtil.javaVersion > 8) {
            if (verbose) System.out.printf("Skipping file %s%n", destfile);
            continue;
          }
          File dir = new File(dest_dir, destfile.getParent());
          dir.mkdirs();
          File destpath = new File(dir, destfile.getName());
          if (verbose) System.out.println("Copying Object or non-classfile: " + destpath);
          try (InputStream in = class_stream_map.get(className)) {
            Files.copy(in, destpath.toPath());
          }
          continue;
        }

        // Get the binary for this class
        InputStream is = class_stream_map.get(className);
        JavaClass jc;
        try {
          ClassParser parser = new ClassParser(is, className);
          jc = parser.parse();
        } catch (Throwable e) {
          throw new Error("Failed to parse classfile " + className, e);
        }

        // Instrument the class file.
        try {
          instrumentClassFile(jc, dest_dir, className);
        } catch (Throwable e) {
          throw new Error("Couldn't instrument " + className, e);
        }
      }

      // Convert from JDK version number to ClassFile major_version.
      // A bit of a hack, but seems OK.
      int java_class_version = BcelUtil.javaVersion + 44;
      String dest_dir_name = dest_dir.getName();

      // We've finished instrumenting all the class files. Now we create and
      // the DcompMarker class which is used to identify instrumented calls.
      ClassGen dcomp_marker =
          new ClassGen(
              "java.lang.DCompMarker",
              "java.lang.Object",
              "daikon.dcomp.BuildJDK tool",
              Const.ACC_INTERFACE | Const.ACC_PUBLIC | Const.ACC_ABSTRACT,
              new String[0]);
      dcomp_marker.setMinor(0);
      dcomp_marker.setMajor(java_class_version);
      dcomp_marker
          .getJavaClass()
          .dump(
              new File(
                  dest_dir_name,
                  "java" + File.separator + "lang" + File.separator + "DCompMarker.class"));

      // The remainer of the generated classes are needed for JDK 9+ only.
      if (BcelUtil.javaVersion > 8) {
        // Create the DcompInstrumented interface
        ClassGen dcomp_instrumented =
            new ClassGen(
                "java.lang.DCompInstrumented",
                "java.lang.Object",
                "daikon.dcomp.BuildJDK tool",
                Const.ACC_INTERFACE | Const.ACC_PUBLIC | Const.ACC_ABSTRACT,
                new String[0]);
        dcomp_instrumented.setMinor(0);
        dcomp_instrumented.setMajor(java_class_version);
        @SuppressWarnings(
            "nullness:argument.type.incompatible") // null instruction list is ok for abstract
        MethodGen mg =
            new MethodGen(
                Const.ACC_PUBLIC | Const.ACC_ABSTRACT,
                Type.BOOLEAN,
                new Type[] {Type.OBJECT},
                new String[] {"o"},
                "equals_dcomp_instrumented",
                dcomp_instrumented.getClassName(),
                null,
                dcomp_instrumented.getConstantPool());
        dcomp_instrumented.addMethod(mg.getMethod());
        dcomp_instrumented
            .getJavaClass()
            .dump(
                new File(
                    dest_dir_name,
                    "java" + File.separator + "lang" + File.separator + "DCompInstrumented.class"));

        // Create the DCompClone interface
        ClassGen dcomp_clone =
            new ClassGen(
                "java.lang.DCompClone",
                "java.lang.Object",
                "daikon.dcomp.BuildJDK tool",
                Const.ACC_INTERFACE | Const.ACC_PUBLIC | Const.ACC_ABSTRACT,
                new String[0]);
        dcomp_clone.setMinor(0);
        dcomp_clone.setMajor(java_class_version);
        dcomp_clone
            .getJavaClass()
            .dump(
                new File(
                    dest_dir_name,
                    "java" + File.separator + "lang" + File.separator + "DCompClone.class"));

        // Create the DCompToString interface
        ClassGen dcomp_tostring =
            new ClassGen(
                "java.lang.DCompToString",
                "java.lang.Object",
                "daikon.dcomp.BuildJDK tool",
                Const.ACC_INTERFACE | Const.ACC_PUBLIC | Const.ACC_ABSTRACT,
                new String[0]);
        dcomp_tostring.setMinor(0);
        dcomp_tostring.setMajor(java_class_version);
        dcomp_tostring
            .getJavaClass()
            .dump(
                new File(
                    dest_dir_name,
                    "java" + File.separator + "lang" + File.separator + "DCompToString.class"));
      }

    } catch (Exception e) {
      throw new Error(e);
    }
  }

  /**
   * Instruments the JavaClass {@code jc} (whose name is {@code classname}). Writes the resulting
   * class to its corresponding location in the directory outputDir.
   *
   * @param jc JavaClass to be instrumented
   * @param outputDir output directory for instrumented class
   * @param classname name of class to be instrumented
   * @throws IOException if unable to write out instrumented class
   */
  private void instrumentClassFile(JavaClass jc, File outputDir, String classname)
      throws java.io.IOException {
    if (verbose) System.out.printf("processing target %s%n", classname);
    DCInstrument dci = new DCInstrument(jc, true, null);
    JavaClass inst_jc;
    if (DynComp.no_primitives) {
      inst_jc = dci.instrument_jdk_refs_only();
    } else {
      inst_jc = dci.instrument_jdk();
    }
    skipped_methods.addAll(dci.get_skipped_methods());
    // File classfile = new File(classname.replace('.', '/') + ".class");
    File classfile = new File(classname);
    File dir;
    if (classfile.getParent() == null) {
      dir = outputDir;
    } else {
      dir = new File(outputDir, classfile.getParent());
    }
    dir.mkdirs();
    File classpath = new File(dir, classfile.getName());
    if (verbose) System.out.printf("writing to file %s%n", classpath);
    inst_jc.dump(classpath);
    _numFilesProcessed++;
    if (((_numFilesProcessed % 100) == 0) && (System.console() != null)) {
      System.out.printf(
          "Processed %d/%d classes at %tc%n",
          _numFilesProcessed, class_stream_map.size(), new Date());
    }
  }

  /**
   * Print information about methods that were not instrumented. This happens when a method fails
   * BCEL's verifier (which is more strict than Java's).
   */
  private static void print_skipped_methods() {

    if (skipped_methods.isEmpty()) {
      // System.out.printf("No methods were skipped.%n");
      return;
    }

    System.out.println(
        "Warning: The following JDK methods could not be instrumented. DynComp will");
    System.out.println("still work as long as these methods are not called by your application.");
    System.out.println("If your application calls one, it will throw a NoSuchMethodException.");

    List<String> unknown = new ArrayList<>(skipped_methods);
    unknown.removeAll(known_uninstrumentable_methods);
    List<String> known = new ArrayList<>(skipped_methods);
    known.retainAll(known_uninstrumentable_methods);

    if (!unknown.isEmpty()) {
      System.out.println("Please report the following problems to the Daikon maintainers.");
      System.out.println(
          "Please give sufficient details; see \"Reporting problems\" in the Daikon manual.");
      for (String method : unknown) {
        System.out.printf("  %s%n", method);
      }
    }
    if (!known.isEmpty()) {
      System.out.printf("The following are known problems; you do not need to report them.");
      for (String method : known) {
        System.out.printf("  %s%n", method);
      }
    }
  }
}
