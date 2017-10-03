
package daikon.dcomp;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.DynComp;
import daikon.util.Option;
import daikon.util.Options;
import java.io.*;
import java.util.*;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.zip.ZipException;
import org.apache.bcel.*;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.generic.*;

/**
 * Converts each file in the JDK. Each method is doubled. The new methods are distinguished by a
 * final parameter of type ?? and are instrumented to track comparability. User code will call the
 * new methods, but instrumentation code is unchanged and calls the original methods.
 */
public class BuildJDK {

  @Option(
      "Instrument the given classfiles from the specified source directory (by default, src must be a jar file)")
  public static boolean classfiles = false;

  /** Synopsis for the dcomp command line */
  public static final String synopsis =
      "daikon.BuildJDK [options] src dest [class_prefix | classfiles...]";

  /**
   * Given an explicit {@code rt.jar} filename, or a root JDK or JRE directory, finds the {@code
   * rt.jar} file.
   */
  public static class RtJarFinder {
    private final String arg;

    public RtJarFinder(String arg) {
      super();
      this.arg = arg;
    }

    /**
     * @param spaceSeparatedJarLocations a list of potential jar filenames to look for, in case arg
     *     was a directory. For example, if arg was /usr/bin/jd2sdk, and spaceSeparatedJarLocations
     *     was "jre/lib/rt.jar", this could return /usr/bin/jd2sdk/jre/lib/rt.jar, if it existed.
     */
    public String findRtJar(String spaceSeparatedJarLocations) {
      String jarFilename = arg;
      if (!arg.endsWith("jar")) {
        for (String candidate : spaceSeparatedJarLocations.split(" ")) {
          String rt = jarFilename + candidate;
          if (exists(rt)) {
            return rt;
          }
        }
      }
      return jarFilename;
    }

    /** @return true iff libRt exists in the filesystem */
    protected boolean exists(String libRt) {
      return new File(libRt).exists();
    }
  }

  private static boolean verbose = false;

  /** Whether or not to instrument java.lang.Object */
  private static boolean skip_object = true;

  private int _numFilesProcessed = 0;

  private static String static_map_fname = "dcomp_jdk_static_map";

  private static List<String> all_classes = new ArrayList<String>();

  private static List<String> skipped_methods = new ArrayList<String>();

  public static String[] known_skipped_methods = new String[] {
        /*
        "sun.rmi.transport.proxy.RMIMasterSocketFactory.createSocket",
        "sun.awt.X11.XWindowPeer.handleButtonPressRelease",
        "com.sun.jmx.snmp.daemon.CommunicatorServer.run",
        "sun.nio.ch.SocketChannelImpl.read0",
        "sun.nio.ch.SocketChannelImpl.read",
        "sun.rmi.transport.proxy.RMIMasterSocketFactory.createSocket",
        "java.nio.channels.SocketChannel.open",
        "sun.security.provider.PolicyFile.init",
        "java.io.Console.readPassword",
        "sun.tools.jps.Jps.main",
        "sun.net.www.MimeTable.saveAsProperties",
        "sun.misc.Service.parse",
        "sun.font.Type1Font.readFile",
        "sun.misc.Resource.getBytes",
        "java.util.ServiceLoader.parse",
        "sun.jkernel.Bundle.loadReceipts",
        "sun.nio.ch.PipeImpl$Initializer.run",
        "com.sun.tools.javac.jvm.ClassReader.readInputStream",
        "com.sun.tools.javac.processing.ServiceProxy.parse",
        "com.sun.tools.javac.zip.ZipFileIndex$DirectoryEntry.initEntries",
        "com.sun.tools.javac.zip.ZipFileIndex.readIndex",
        "com.sun.tools.javac.zip.ZipFileIndex.writeIndex",
        */
      };

  /**
   * Invoke as: BuildJDK jarfile dest prefix
   *
   * <dl>
   *   <dt>jarfile
   *   <dd>jarfile to process
   *   <dt>dest
   *   <dd>destination directory in which to place instrumented classes
   *   <dt>prefix
   *   <dd>optional prefix of classes to be translated
   * </dl>
   *
   * Instruments each class file in jarfile that begins with prefix and puts the results in dest.
   */
  public static void main(String[] args) throws IOException {

    System.out.println("Starting at " + new Date());

    Options options = new Options(synopsis, BuildJDK.class, DynComp.class);
    // options.ignore_options_after_arg (true);
    String[] cl_args = options.parse_or_usage(args);
    boolean ok = check_args(options, cl_args);
    if (!ok) System.exit(1);
    verbose = DynComp.verbose;

    if (classfiles) {

      // Arguments are <srcdir> <destdir> <classfiles>...
      File src_dir = new File(cl_args[0]);
      File dest_dir = new File(cl_args[1]);
      File[] class_files = new File[cl_args.length - 2];
      for (int ii = 2; ii < cl_args.length; ii++) {
        class_files[ii - 2] = new File(cl_args[ii]);
      }

      BuildJDK build = new BuildJDK();

      // Restore the static map from field names to ids
      DCInstrument.restore_static_map(new File(dest_dir, static_map_fname));
      System.out.printf("Restored %d entries in static map%n", DCInstrument.static_map.size());

      // Read in each specified classfile
      Map<String, JavaClass> classmap = new LinkedHashMap<String, JavaClass>();
      for (File class_file : class_files) {
        if (class_file.toString().endsWith("java/lang/Object.class")) {
          System.out.printf("Skipping %s%n", class_file);
          continue;
        }
        ClassParser parser = new ClassParser(class_file.toString());
        JavaClass jc = parser.parse();
        classmap.put(jc.getClassName(), jc);
      }

      // Process each classfile
      for (String classname : classmap.keySet()) {
        JavaClass jc = classmap.get(classname);
        try {
          build.processClassFile(classmap, dest_dir, classname);
        } catch (Throwable e) {
          throw new Error("Couldn't instrument " + classname, e);
        }
      }

    } else { // translate from jar file

      final String potential_jar_file_name = cl_args[0];
      String dest_dir = cl_args[1];
      String prefix = cl_args.length == 3 ? cl_args[2] : "";

      BuildJDK build = new BuildJDK();
      JarFile jfile = getJarFile(potential_jar_file_name);

      build.translate_classes(jfile, dest_dir, prefix, "");

      // Create the various helper classes
      // build.dump_helper_classes(dest_dir);

      // Write out the static map
      System.out.printf("Found %d statics%n", DCInstrument.static_map.size());
      DCInstrument.save_static_map(new File(dest_dir, static_map_fname));

      // Write out the list of all classes in the jar file
      File jdk_classes_dir = new File(dest_dir, "java/lang");
      File jdk_classes_file = new File(jdk_classes_dir, "jdk_classes.txt");
      PrintWriter pw = new PrintWriter(jdk_classes_file, UTF_8.name());
      System.out.printf("Writing all classes to %s%n", jdk_classes_file);
      for (String classname : all_classes) {
        pw.println(classname);
      }
      pw.flush();
      pw.close();

      // Print out any methods that could not be instrumented
      print_skipped_methods();
    }
    System.out.println("done at " + new Date());
  }

  /**
   * Check the resulting arguments for legality. Prints a message and returns false if there was an
   * error.
   */
  public static boolean check_args(Options options, String[] target_args) {

    if (classfiles) {
      if (target_args.length < 2) {
        options.print_usage("must specify source jar and destination dir");
        return false;
      }
      if (target_args.length < 3) {
        options.print_usage("must specify classfiles to instrument");
        return false;
      }
    } else {
      if (target_args.length < 2) {
        options.print_usage("must specify source jar and destination dir");
        return false;
      }
      if (target_args.length > 3) {
        options.print_usage("too many arguments");
        return false;
      }
    }

    return true;
  }

  private static JarFile getJarFile(String potentialJarFileName) throws IOException, ZipException {
    JarFile jfile;
    final String p = potentialJarFileName;
    try {
      String jar_name = findRtJarFilename(p);
      System.out.printf("using jar file %s\n", jar_name);
      jfile = new JarFile(jar_name);
    } catch (ZipException e) {
      throw new ZipException(e.getMessage() + "; filename was " + p);
    }
    return jfile;
  }

  private static String findRtJarFilename(final String arg) {
    return new RtJarFinder(arg).findRtJar("/lib/rt.jar /jre/lib/rt.jar");
  }

  void copyStreams(InputStream fis, OutputStream fos) throws Exception {
    byte[] buf = new byte[1024];
    int i = 0;
    while ((i = fis.read(buf)) != -1) {
      fos.write(buf, 0, i);
    }
    fis.close();
    fos.close();
  }

  void translate_classes(JarFile jfile, String dest, String prefix, String prefixOfFilesToInclude)
      throws java.io.IOException {

    // Map from classname to class so we can find out information about
    // classes we have not yet instrumented.
    Map<String, JavaClass> classmap = new LinkedHashMap<String, JavaClass>();

    try {

      // Create the destination directory
      File dfile = new File(dest);
      dfile.mkdirs();

      // Get each class to be instrumented and store it away
      Enumeration<JarEntry> entries = jfile.entries();
      while (entries.hasMoreElements()) {
        JarEntry entry = entries.nextElement();
        // System.out.printf ("processing entry %s%n", entry);
        final String entryName = entry.getName();
        if (!entryName.startsWith(prefixOfFilesToInclude) && !entryName.startsWith("META-INF")) {
          continue;
        }
        if ((entryName.endsWith("/")) || (entryName.endsWith("~"))) {
          continue;
        }
        if (entryName.endsWith(".class")) all_classes.add(entryName.replace(".class", ""));
        if (!entryName.endsWith(".class")
            || (skip_object && entryName.equals("java/lang/Object.class"))
            || (!prefix.equals("") && !entryName.startsWith(prefix))) {
          File destfile = new File(entryName);
          if (destfile.getParent() == null) {
            System.out.printf("Skipping file %s%n", destfile);
            continue;
          }
          File dir = new File(dfile, destfile.getParent());
          dir.mkdirs();
          File destpath = new File(dir, destfile.getName());
          if (verbose) System.out.println("Copying Object or non-classfile: " + destpath);
          copyStreams(jfile.getInputStream(entry), new FileOutputStream(destpath));
          continue;
        }

        // Get the binary for this class
        InputStream is = jfile.getInputStream(entry);
        JavaClass jc;
        try {
          ClassParser parser = new ClassParser(is, entryName);
          jc = parser.parse();
        } catch (Exception e) {
          throw new Error("Failed to parse entry " + entry, e);
        }
        classmap.put(jc.getClassName(), jc);
      }

      if (false) {
        processClassFile(classmap, dfile, "sun.rmi.registry.RegistryImpl_Skel");
        System.exit(0);
      }

      // Process each file read.
      for (String classname : classmap.keySet()) {
        JavaClass jc = classmap.get(classname);
        try {
          processClassFile(classmap, dfile, classname);
        } catch (Throwable e) {
          throw new Error("Couldn't instrument " + classname, e);
        }
      }

      // Create the DcompMarker class (used to identify instrumented calls)
      ClassGen dcomp_marker =
          new ClassGen(
              "java.lang.DCompMarker",
              "java.lang.Object",
              "DCompMarker.class",
              Const.ACC_INTERFACE | Const.ACC_PUBLIC | Const.ACC_ABSTRACT,
              new String[0]);
      dcomp_marker
          .getJavaClass()
          .dump(
              new File(
                  dest, "java" + File.separator + "lang" + File.separator + "DCompMarker.class"));

    } catch (Exception e) {
      throw new Error(e);
    }
  }

  /**
   * Looks up classname in classmap and instruments the class that is found. Writes the resulting
   * class to its corresponding location in the directory dfile.
   */
  private void processClassFile(Map<String, JavaClass> classmap, File dfile, String classname)
      throws java.io.IOException {
    if (verbose) System.out.printf("processing target %s\n", classname);
    JavaClass jc = classmap.get(classname);
    assert jc != null : "@AssumeAssertion(nullness): seems to be non-null";
    DCInstrument dci = new DCInstrument(jc, true, null);
    JavaClass inst_jc;
    if (DynComp.no_primitives) {
      inst_jc = dci.instrument_jdk_refs_only();
    } else {
      inst_jc = dci.instrument_jdk();
    }
    skipped_methods.addAll(dci.get_skipped_methods());
    File classfile = new File(classname.replace('.', '/') + ".class");
    File dir;
    if (classfile.getParent() == null) {
      dir = dfile;
    } else {
      dir = new File(dfile, classfile.getParent());
    }
    dir.mkdirs();
    File classpath = new File(dir, classfile.getName());
    inst_jc.dump(classpath);
    _numFilesProcessed++;
    if (((_numFilesProcessed % 100) == 0) && (System.console() != null)) {
      System.out.printf(
          "Processed %d/%d classes at %tc%n", _numFilesProcessed, classmap.size(), new Date());
    }
  }

  /** Copy our various helper classes to java/lang */
  private void dump_helper_classes(String dest) throws java.io.IOException {

    File dir = new File(dest, "java" + File.separator + "lang");

    ClassParser parser = new ClassParser("bin/java/lang/ArrayAccessors.class");
    JavaClass jc = parser.parse();
    jc.dump(new File(dir, "ArrayAccessors.class"));

    parser = new ClassParser("bin/java/lang/GenericInterface.class");
    jc = parser.parse();
    jc.dump(new File(dir, "GenericInterface.class"));

    parser = new ClassParser("bin/java/lang/ObjectHelper.class");
    jc = parser.parse();
    jc.dump(new File(dir, "ObjectHelper.class"));

    parser = new ClassParser("bin/java/lang/StaticInterface.class");
    jc = parser.parse();
    jc.dump(new File(dir, "StaticInterface.class"));
  }

  private List<String> classesWithoutInterfaces() {
    return Arrays.<String>asList("java.lang.Object", "java.lang.String", "java.lang.Class");
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

    // Determine if all of them were known to be bad
    List<String> known_bad_list = Arrays.asList(known_skipped_methods);
    boolean all_known = true;
    for (String method : skipped_methods) {
      if (!known_bad_list.contains(method)) {
        all_known = false;
        break;
      }
    }

    if (all_known) {
      System.out.printf(
          "Warning, the following JDK methods could not be instrumented.%n"
              + "These are known problems.  DynComp will still work as long as%n"
              + "these methods are not called by your applications.%n"
              + "If your application calls one, it will throw a NoSuchMethodException.%n");
      for (String method : skipped_methods) {
        System.out.printf("  %s%n", method);
      }
    } else { // some methods have not been previously seen
      System.out.printf(
          "Warning: the following JDK methods could not be instrumented.%n"
              + "Please report any line that starts with [unexpected] so we can look into them.%n"
              + "Please give sufficient details; see \"Reporting problems\" in the Daikon manual.%n"
              + "DynComp will still work as long as these methods are not called%n"
              + "by your applications.%n"
              + "If your application calls one, it will throw a NoSuchMethodException.%n");
      for (String method : skipped_methods) {
        if (known_bad_list.contains(method)) {
          System.out.printf("  %s%n", method);
        } else {
          System.out.printf("  [unexpected] %s%n", method);
        }
      }
    }
  }
}
