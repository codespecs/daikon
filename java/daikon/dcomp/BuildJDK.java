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
 * Converts each file in the JDK. Each method is doubled. The new methods are distinguished by a
 * final parameter of type DCompMarker and are instrumented to track comparability. User code will
 * call the new methods, but instrumentation code is unchanged and calls the original methods.
 */
public class BuildJDK {

  /** The "java.home" system property. */
  public static final String java_home = System.getProperty("java.home");

  @Option("Instrument the classfiles given on the command line")
  public static boolean classfiles = false;

  // Used for debugging; tested in DCInstrument.
  @Option("Halt if an instrumentation error occurs")
  public static boolean quit_if_error = false;

  private static boolean verbose = false;

  private int _numFilesProcessed = 0;

  private static String static_map_fname = "dcomp_jdk_static_map";

  private static List<String> all_classes = new ArrayList<>();

  private static List<String> skipped_methods = new ArrayList<>();

  public static List<String> known_uninstrumentable_methods =
      Arrays.asList(
          // None at present
          );

  // We want to share code to read members of a jar file (JDK 8) or members of a
  // module file (JDK 9+). We accomplish this by saving an InputStream for
  // each member class we find.
  private Map<String, InputStream> class_stream_map = new HashMap<>();

  /** Instruments each class file in Java runtime and puts the result in dest. */
  public static void main(String[] args) throws IOException {

    System.out.println("BuildJDK starting at " + new Date());

    Options options =
        new Options(
            "daikon.BuildJDK [options] dest [classfiles...]", BuildJDK.class, DynComp.class);
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
      DCInstrument.restore_static_map(new File(dest_dir, static_map_fname));
      System.out.printf("Restored %d entries in static map.%n", DCInstrument.static_map.size());

      // Read in each specified classfile
      for (File class_file : class_files) {
        String className = class_file.toString();
        if (className.endsWith("java/lang/Object.class")) {
          System.out.printf("Skipping %s%n", className);
          continue;
        }

        // Convert the class file to internal BCEL form.
        JavaClass jc;
        try {
          ClassParser parser = new ClassParser(className);
          jc = parser.parse();
        } catch (Throwable e) {
          throw new Error("Failed to parse classfile " + className, e);
        }

        // Instrument the class file.
        try {
          build.processClassFile(jc, dest_dir, className);
        } catch (Throwable e) {
          throw new Error("Couldn't instrument " + className, e);
        }
      }

    } else { // translate from jar file or modules file

      if (BcelUtil.javaVersion > 8) {
        build.translate_modules();
      } else {
        build.translate_jar();
      }
      build.translate_classes(dest_dir);

      // Write out the static map
      System.out.printf("Found %d statics.%n", DCInstrument.static_map.size());
      DCInstrument.save_static_map(new File(dest_dir, static_map_fname));

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
   * Check the arguments for legality. Verify java.home and JAVA_HOME match. Prints a message and
   * returns false if there is an error.
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
        System.out.println("must specify source jar and destination dir");
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
        System.out.println("JAVA_HOME not defined; using java.home:");
        System.out.println("  " + java_home);
      }
      JAVA_HOME = java_home;
    }

    File jrt = new File(JAVA_HOME);
    if (!jrt.exists()) {
      System.out.printf("Directory at JAVA_HOME (%s) does not exist.%n", jrt);
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

  void translate_jar() throws java.io.IOException {

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

  void translate_modules() throws java.io.IOException {

    FileSystem fs = FileSystems.getFileSystem(URI.create("jrt:/"));
    Path modules = fs.getPath("/modules");
    System.out.printf("using modules file %s%n", java_home + "/lib/modules");
    try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(modules, "java.base*")) {
      for (Path module : directoryStream) {
        translate_modules_directory(module, module.toString().length());
      }
    } catch (IOException e) {
      throw new Error(e);
    }
  }

  void translate_modules_directory(Path path, int modulePrefixLength) {

    if (Files.isDirectory(path)) {
      try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(path)) {
        for (Path subpath : directoryStream) {
          translate_modules_directory(subpath, modulePrefixLength);
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

  void translate_classes(File dest_dir) throws java.io.IOException {

    try {
      // Create the destination directory
      dest_dir.mkdirs();

      // Process each file.
      for (String className : class_stream_map.keySet()) {
        if (verbose) System.out.println("translate_classes: " + className);

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
          processClassFile(jc, dest_dir, className);
        } catch (Throwable e) {
          throw new Error("Couldn't instrument " + className, e);
        }
      }

      // Convert from JDK version number to ClassFile major_version.
      // A bit of a hack, but seems OK.
      int java_class_version = BcelUtil.javaVersion + 44;
      String dest_dir_name = dest_dir.getName();

      // Create the DcompMarker class (used to identify instrumented calls)
      // Needed for all JDK versions.
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
   * Instruments the JavaClass jc (whose name is classname). Writes the resulting class to its
   * corresponding location in the directory dfile.
   */
  private void processClassFile(JavaClass jc, File dfile, String classname)
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
      dir = dfile;
    } else {
      dir = new File(dfile, classfile.getParent());
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
   * Print out information about any methods that were not instrumented. This happens when a method
   * fails BCEL's verifier (which is more strict than Java's). Any failures which have not been
   * previously seen are noted.
   */
  private static void print_skipped_methods() {

    if (skipped_methods.isEmpty()) {
      System.out.printf("No methods were skipped.%n");
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
