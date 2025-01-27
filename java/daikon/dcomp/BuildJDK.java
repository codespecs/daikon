package daikon.dcomp;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.DynComp;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.options.Options;
import daikon.plumelib.reflection.Signatures;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.UncheckedIOException;
import java.net.URI;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.generic.ClassGen;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.signature.qual.BinaryName;

/**
 * BuildJDK uses {@link DCInstrument} to add comparability instrumentation to Java class files, then
 * stores the modified files into a directory identified by a (required) command line argument.
 *
 * <p>DCInstrument duplicates each method of a class file. The new methods are distinguished by the
 * addition of a final parameter of type DCompMarker and are instrumented to track comparability.
 * Based on its invocation arguments, DynComp will decide whether to call the instrumented or
 * uninstrumented version of a method.
 */
@SuppressWarnings({
  "mustcall:type.argument",
  "mustcall:type.arguments.not.inferred"
}) // assignments into owning collection
public class BuildJDK {

  /** Creates a new BuildJDK. */
  private BuildJDK() {}

  /**
   * The "java.home" system property. Note that there is also a JAVA_HOME variable that contains
   * {@code System.getenv("JAVA_HOME")}.
   */
  public static final String java_home = System.getProperty("java.home");

  /** Whether to print information about the classes being instrumented. */
  private static boolean verbose = false;

  /** Number of class files processed; used for progress display. */
  private int _numFilesProcessed = 0;

  /** Name of file in output jar containing the static-fields map. */
  private static String static_field_id_filename = "dcomp_jdk_static_field_id";

  /**
   * Collects names of all methods that DCInstrument could not process. Should be empty. Format is
   * &lt;fully-qualified class name&gt;.&lt;method name&gt;
   */
  private static List<String> skipped_methods = new ArrayList<>();

  /**
   * A list of methods known to cause DCInstrument to fail. This is used to remove known problems
   * from the list of failures displayed at the end of BuildJDK's execution. Format is
   * &lt;fully-qualified class name&gt;.&lt;method name&gt;
   */
  public static List<String> known_uninstrumentable_methods =
      Arrays.asList(
          // None at present
          );

  /**
   * Instruments each class file in the Java runtime and puts the result in the first non-option
   * command-line argument.
   *
   * <p>By default, BuildJDK will locate the appropriate Java runtime library and instrument each of
   * its member class files. However, if there are additional arguments on the command line after
   * the destination directory, then we assume these are class files to be instrumented and the Java
   * runtime library is not used. This usage is primarily for testing purposes.
   *
   * @param args arguments being passed to BuildJDK
   * @throws IOException if unable to read or write file {@code dcomp_jdk_static_field_id} or if
   *     unable to write {@code jdk_classes.txt}
   */
  @SuppressWarnings("builder:required.method.not.called") // assignment into collection of @Owning
  public static void main(String[] args) throws IOException {

    System.out.println("BuildJDK starting at " + LocalDateTime.now(ZoneId.systemDefault()));

    BuildJDK build = new BuildJDK();

    Options options =
        new Options(
            "daikon.BuildJDK [options] dest_dir [classfiles...]",
            DynComp.class,
            DCInstrument.class);
    String[] cl_args = options.parse(true, args);
    if (cl_args.length < 1) {
      System.err.println("must specify destination dir");
      options.printUsage();
      System.exit(1);
    }
    verbose = DynComp.verbose;

    File dest_dir = new File(cl_args[0]);

    // Key is a class file name, value is a stream that opens that file name.
    //
    // <p>We want to share code to read and instrument the Java class file members of a jar file
    // (JDK 8) or a module file (JDK 9+). However, jar files and module files are located in two
    // completely different file systems. So we open an InputStream for each class file we wish to
    // instrument and save it in the class_stream_map with the file name as the key. From that point
    // the code to instrument a class file can be shared.
    Map<String, InputStream> class_stream_map;

    if (cl_args.length > 1) {

      // Arguments are <destdir> [<classfiles>...]
      @SuppressWarnings("nullness:assignment") // https://tinyurl.com/cfissue/3224
      @NonNull String[] class_files = Arrays.copyOfRange(cl_args, 1, cl_args.length);

      // Instrumenting a specific list of class files is usually used for testing.
      // But if we're using it to fix a broken classfile, then we need
      // to restore the static-fields map from when our runtime jar was originally
      // built.  We assume it is in the destination directory.
      DCInstrument.restore_static_field_id(new File(dest_dir, static_field_id_filename));
      System.out.printf(
          "Restored %d entries in static map.%n", DCInstrument.static_field_id.size());

      class_stream_map = new HashMap<>();
      for (String classFileName : class_files) {
        try {
          class_stream_map.put(classFileName, new FileInputStream(classFileName));
        } catch (FileNotFoundException e) {
          throw new Error("File not found: " + classFileName, e);
        }
      }

      // Instrument the classes identified in class_stream_map.
      build.instrument_classes(dest_dir, class_stream_map);

    } else {

      check_java_home();

      if (BcelUtil.javaVersion > 8) {
        class_stream_map = build.gather_runtime_from_modules();
      } else {
        class_stream_map = build.gather_runtime_from_jar();
      }

      // Instrument the Java runtime classes identified in class_stream_map.
      build.instrument_classes(dest_dir, class_stream_map);

      // We've finished instrumenting all the class files. Now we create some
      // abstract interface classes for use by the DynComp runtime.
      build.addInterfaceClasses(dest_dir.getName());

      // Write out the file containing the static-fields map.
      System.out.printf("Found %d static fields.%n", DCInstrument.static_field_id.size());
      DCInstrument.save_static_field_id(new File(dest_dir, static_field_id_filename));

      // Write out the list of all classes in the jar file
      File jdk_classes_file = new File(dest_dir, "java/lang/jdk_classes.txt");
      System.out.printf("Writing a list of class names to %s%n", jdk_classes_file);
      // Class names are written in internal form.
      try (PrintWriter pw = new PrintWriter(jdk_classes_file, UTF_8.name())) {
        for (String classFileName : class_stream_map.keySet()) {
          pw.println(classFileName.replace(".class", ""));
        }
      }
    }

    // Print out any methods that could not be instrumented
    print_skipped_methods();

    System.out.println("BuildJDK done at " + LocalDateTime.now(ZoneId.systemDefault()));
  }

  /** Verify that java.home and JAVA_HOME match. Exits the JVM if there is an error. */
  public static void check_java_home() {

    // We are going to instrument the default Java runtime library.
    // We need to verify where we should look for it.

    String JAVA_HOME = System.getenv("JAVA_HOME");
    if (JAVA_HOME == null) {
      if (verbose) {
        System.out.println("JAVA_HOME not defined; using java.home: " + java_home);
      }
      JAVA_HOME = java_home;
    }

    File jrt = new File(JAVA_HOME);
    if (!jrt.exists()) {
      System.err.printf("Java home directory %s does not exist.%n", jrt);
      System.exit(1);
    }

    try {
      jrt = jrt.getCanonicalFile();
    } catch (Exception e) {
      System.err.printf("Error geting canonical file for %s: %s", jrt, e.getMessage());
      System.exit(1);
    }

    JAVA_HOME = jrt.getAbsolutePath();
    if (!java_home.startsWith(JAVA_HOME)) {
      System.err.printf(
          "JAVA_HOME (%s) does not agree with java.home (%s).%n", JAVA_HOME, java_home);
      System.err.printf("Please correct your Java environment.%n");
      System.exit(1);
    }
  }

  /**
   * For Java 8 the Java runtime is located in rt.jar. This method creates an InputStream for each
   * class in rt.jar and returns this information in a map from class file name to InputStream.
   *
   * @return a map from class file name to the associated InputStream
   */
  @SuppressWarnings({
    "JdkObsolete", // JarEntry.entries() returns Enumeration
    "builder:required.method.not.called" // assignment into collection of @Owning
  })
  Map<String, InputStream> gather_runtime_from_jar() {

    Map<String, InputStream> class_stream_map = new HashMap<>();
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
        if (entryName.endsWith("/") || entryName.endsWith("~")) {
          continue;
        }

        // Get the InputStream for this file
        InputStream is = jfile.getInputStream(entry);
        class_stream_map.put(entryName, is);
      }
    } catch (Exception e) {
      throw new Error("Problem while reading " + jar_name, e);
    }
    return class_stream_map;
  }

  /**
   * For Java 9+ the Java runtime is located in a series of modules. At this time, we are only
   * pre-instrumenting the java.base module. This method initializes the DirectoryStream used to
   * explore java.base. It calls gather_runtime_from_modules_directory to process the directory
   * structure.
   *
   * @return a map from class file name to the associated InputStream
   */
  Map<String, InputStream> gather_runtime_from_modules() {

    Map<String, InputStream> class_stream_map = new HashMap<>();
    FileSystem fs = FileSystems.getFileSystem(URI.create("jrt:/"));
    Path modules = fs.getPath("/modules");
    // The path java_home+/lib/modules is the file in the host file system that
    // corresponds to the modules file in the jrt: file system.
    System.out.printf("using modules directory %s%n", java_home + "/lib/modules");
    try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(modules, "java.base*")) {
      for (Path moduleDir : directoryStream) {
        gather_runtime_from_modules_directory(
            moduleDir, moduleDir.toString().length(), class_stream_map);
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
    return class_stream_map;
  }

  /**
   * This is a helper method for {@link #gather_runtime_from_modules}. It recurses down the module
   * directory tree, selects the classes we want to instrument, creates an InputStream for each of
   * these classes, and adds this information to the {@code class_stream_map} argument.
   *
   * @param path module file, which might be subdirectory
   * @param modulePrefixLength length of "/module/..." path prefix before start of actual member
   *     path
   * @param class_stream_map a map from class file name to InputStream that collects the results
   */
  @SuppressWarnings("builder:required.method.not.called") // assignment into collection of @Owning
  void gather_runtime_from_modules_directory(
      Path path, int modulePrefixLength, Map<String, InputStream> class_stream_map) {

    if (Files.isDirectory(path)) {
      try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(path)) {
        for (Path subpath : directoryStream) {
          gather_runtime_from_modules_directory(subpath, modulePrefixLength, class_stream_map);
        }
      } catch (IOException e) {
        throw new UncheckedIOException(e);
      }
    } else {
      String entryName = path.toString().substring(modulePrefixLength + 1);
      try {
        // Get the InputStream for this file
        InputStream is = Files.newInputStream(path);
        class_stream_map.put(entryName, is);
      } catch (Exception e) {
        throw new Error(e);
      }
    }
  }

  /**
   * Instrument each of the classes indentified by the class_stream_map argument.
   *
   * @param dest_dir where to store the instrumented classes
   * @param class_stream_map maps from class file name to an input stream on that file
   */
  void instrument_classes(File dest_dir, Map<String, InputStream> class_stream_map) {

    try {
      // Create the destination directory
      dest_dir.mkdirs();

      // Process each file.
      for (String classFileName : class_stream_map.keySet()) {
        if (verbose) {
          System.out.println("instrument_classes: " + classFileName);
        }

        if (classFileName.equals("module-info.class")) {
          System.out.printf("Skipping file %s%n", classFileName);
          continue;
        }

        // Handle non-.class files and Object.class.  In JDK 8, copy them unchanged.
        // For JDK 9+ we do not copy as these items will be loaded from the original module file.
        if (!classFileName.endsWith(".class") || classFileName.equals("java/lang/Object.class")) {
          if (BcelUtil.javaVersion > 8) {
            if (verbose) {
              System.out.printf("Skipping file %s%n", classFileName);
            }
            continue;
          }
          // This File constructor ignores dest_dir if classFileName is absolute.
          File classFile = new File(dest_dir, classFileName);
          if (classFile.getParentFile() == null) {
            throw new Error("This can't happen: " + classFile);
          }
          classFile.getParentFile().mkdirs();
          if (verbose) {
            System.out.println("Copying Object.class or non-classfile: " + classFile);
          }
          try (InputStream in = class_stream_map.get(classFileName)) {
            Files.copy(in, classFile.toPath());
          }
          continue;
        }

        // Get the binary for this class
        JavaClass jc;
        try (InputStream is = class_stream_map.get(classFileName)) {
          ClassParser parser = new ClassParser(is, classFileName);
          jc = parser.parse();
        } catch (Throwable e) {
          throw new Error("Failed to parse classfile " + classFileName, e);
        }

        // Instrument the class file.
        try {
          instrumentClassFile(jc, dest_dir, classFileName, class_stream_map.size());
        } catch (Throwable e) {
          throw new Error("Couldn't instrument " + classFileName, e);
        }
      }
    } catch (Exception e) {
      throw new Error(e);
    }
  }

  /**
   * Add abstract interface classes needed by the DynComp runtime.
   *
   * @param destDir where to store the interface classes
   */
  private void addInterfaceClasses(String destDir) {
    // Create the DcompMarker class which is used to identify instrumented calls.
    createDCompClass(destDir, "DCompMarker", false);

    // The remainer of the generated classes are needed for JDK 9+ only.
    if (BcelUtil.javaVersion > 8) {
      createDCompClass(destDir, "DCompInstrumented", true);
      createDCompClass(destDir, "DCompClone", false);
      createDCompClass(destDir, "DCompToString", false);
    }
  }

  /**
   * Create an abstract interface class for use by the DynComp runtime.
   *
   * @param destDir where to store the new class
   * @param className name of class
   * @param dcompInstrumented if true, add equals_dcomp_instrumented method to class
   */
  private void createDCompClass(
      String destDir, @BinaryName String className, boolean dcompInstrumented) {
    try {
      ClassGen dcomp_class =
          new ClassGen(
              Signatures.addPackage("java.lang", className),
              "java.lang.Object",
              "daikon.dcomp.BuildJDK tool",
              Const.ACC_INTERFACE | Const.ACC_PUBLIC | Const.ACC_ABSTRACT,
              new @BinaryName String[0]);
      dcomp_class.setMinor(0);
      // Convert from JDK version number to ClassFile major_version.
      // A bit of a hack, but seems OK.
      dcomp_class.setMajor(BcelUtil.javaVersion + 44);

      if (dcompInstrumented) {
        @SuppressWarnings("nullness:argument") // null instruction list is ok for abstract
        MethodGen mg =
            new MethodGen(
                Const.ACC_PUBLIC | Const.ACC_ABSTRACT,
                Type.BOOLEAN,
                new Type[] {Type.OBJECT},
                new String[] {"o"},
                "equals_dcomp_instrumented",
                dcomp_class.getClassName(),
                null,
                dcomp_class.getConstantPool());
        dcomp_class.addMethod(mg.getMethod());
      }

      dcomp_class
          .getJavaClass()
          .dump(
              // Path.of exists in Java 11 and later.
              new File(new File(new File(destDir, "java"), "lang"), className + ".class"));
    } catch (Exception e) {
      throw new Error(e);
    }
  }

  /** Formats just the time part of a DateTime. */
  private DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss");

  /**
   * Instruments the JavaClass {@code jc} (whose name is {@code classFileName}). Writes the
   * resulting class to its corresponding location in the directory outputDir.
   *
   * @param jc JavaClass to be instrumented
   * @param outputDir output directory for instrumented class
   * @param classFileName name of class to be instrumented
   * @param classTotal total number of classes to be processed; used for progress display
   * @throws IOException if unable to write out instrumented class
   */
  @SuppressWarnings("SystemConsoleNull") // https://errorprone.info/bugpattern/SystemConsoleNull
  private void instrumentClassFile(
      JavaClass jc, File outputDir, String classFileName, int classTotal)
      throws java.io.IOException {
    if (verbose) {
      System.out.printf("processing target %s%n", classFileName);
    }
    DCInstrument dci = new DCInstrument(jc, true, null);
    JavaClass inst_jc;
    inst_jc = dci.instrument_jdk();
    skipped_methods.addAll(dci.get_skipped_methods());
    File classfile = new File(classFileName);
    File dir;
    if (classfile.getParent() == null) {
      dir = outputDir;
    } else {
      dir = new File(outputDir, classfile.getParent());
    }
    dir.mkdirs();
    File classpath = new File(dir, classfile.getName());
    if (verbose) {
      System.out.printf("writing to file %s%n", classpath);
    }
    inst_jc.dump(classpath);
    _numFilesProcessed++;
    if (((_numFilesProcessed % 100) == 0) && (System.console() != null)) {
      System.out.printf(
          "Processed %d/%d classes at %s%n",
          _numFilesProcessed,
          classTotal,
          LocalDateTime.now(ZoneId.systemDefault()).format(timeFormatter));
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

    System.err.println(
        "Warning: The following JDK methods could not be instrumented. DynComp will");
    System.err.println("still work as long as these methods are not called by your application.");
    System.err.println("If your application calls one, it will throw a NoSuchMethodException.");

    List<String> unknown = new ArrayList<>(skipped_methods);
    unknown.removeAll(known_uninstrumentable_methods);
    List<String> known = new ArrayList<>(skipped_methods);
    known.retainAll(known_uninstrumentable_methods);

    if (!unknown.isEmpty()) {
      System.err.println("Please report the following problems to the Daikon maintainers.");
      System.err.println(
          "Please give sufficient details; see \"Reporting problems\" in the Daikon manual.");
      for (String method : unknown) {
        System.err.printf("  %s%n", method);
      }
    }
    if (!known.isEmpty()) {
      System.err.printf("The following are known problems; you do not need to report them.");
      for (String method : known) {
        System.err.printf("  %s%n", method);
      }
    }
  }
}
