package daikon.chicory;

// import harpoon.ClassFile.HMethod;

import static daikon.tools.nullness.NullnessUtil.castNonNull;

import daikon.Chicory;
import daikon.plumelib.bcelutil.SimpleLog;
import daikon.plumelib.options.Option;
import daikon.plumelib.options.Options;
import daikon.plumelib.util.FilesPlume;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.Instrumentation;
import java.lang.reflect.Member;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.jar.JarFile;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.checkerframework.checker.signature.qual.BinaryName;

/**
 * This class is the entry point for the Chicory instrumentation agent. It is the only code in
 * ChicoryPremain.jar.
 */
public class ChicoryPremain {

  // These command-line options cannot be accessed from Chicory.  These are internal debugging
  // options that may be used when ChicoryPremain is invoked directly from the command line.

  /** Socket port to communicate with Daikon. */
  @Option("socket port to communicate with Daikon")
  public static int daikon_port = -1;

  /** Turn on most Runtime debugging options. */
  @Option("Turn on most Runtime debugging options")
  public static boolean debug_runtime = false;

  /** Print information about the classes being transformed. */
  public static boolean verbose = false;

  /** Set of pure methods returned by Alexandru Salcianu's purity analysis. */
  // Non-null if doPurity == true
  private static @MonotonicNonNull Set<String> pureMethods = null;

  /** True iff Chicory should add variables based on pure methods during instrumentation. */
  private static boolean doPurity = false;

  /**
   * This method is the entry point of the Java agent. Its main purpose is to set up the transformer
   * so that when classes from the target app are loaded, they are first transformed.
   *
   * <p>This method also sets up some other initialization tasks: it connects to Daikon over a port
   * if necessary, or reads in a purity analysis.
   */
  public static void premain(String agentArgs, Instrumentation inst) throws IOException {

    // System.out.format ("In premain, agentargs ='%s', " +
    //                   "Instrumentation = '%s'%n", agentArgs, inst);

    // Because Chicory started ChicoryPremain in a separate process, we must rescan
    // the options to set up the Chicory static variables.
    Options options = new Options(Chicory.synopsis, Chicory.class, ChicoryPremain.class);
    String[] target_args = options.parse(true, Options.tokenize(agentArgs));
    if (target_args.length > 0) {
      System.err.printf("Unexpected ChicoryPremain arguments %s%n", Arrays.toString(target_args));
      options.printUsage();
      System.exit(1);
    }

    verbose = Chicory.verbose || Chicory.debug;
    if (debug_runtime) {
      Runtime.debug = true;
    }

    if (verbose) {
      System.out.printf(
          "In Chicory premain, agentargs ='%s', Instrumentation = '%s'", agentArgs, inst);
      System.out.printf("Options settings: %n%s%n", options.settings());
    }

    // Open the dtrace file
    if (Chicory.daikon_online) {
      Runtime.setDtraceOnlineMode(daikon_port);
    } else if (Chicory.dtrace_file == null) {
      File trace_file_path = new File(Chicory.output_dir, "dtrace.gz");
      Runtime.setDtraceMaybe(trace_file_path.toString());
    } else {
      File trace_file_path = new File(Chicory.output_dir, Chicory.dtrace_file.getPath());
      Runtime.setDtraceMaybe(trace_file_path.toString());
    }

    // Setup argument fields in Runtime
    Runtime.nesting_depth = Chicory.nesting_depth;
    // daikon.chicory.Instrument.shouldIgnore is shared by Chicory and DynComp.
    // It uses the Runtime copy of the patterns.
    Runtime.ppt_omit_pattern = Chicory.ppt_omit_pattern;
    Runtime.ppt_select_pattern = Chicory.ppt_select_pattern;
    Runtime.sample_start = Chicory.sample_start;
    DaikonVariableInfo.std_visibility = Chicory.std_visibility;
    DaikonVariableInfo.debug_vars.enabled = Chicory.debug_decl_print;
    if (Chicory.comparability_file != null) {
      Runtime.comp_info = new DeclReader();
      try {
        castNonNull(Runtime.comp_info).read(castNonNull(Chicory.comparability_file));
      } catch (FileNotFoundException e) {
        System.err.printf("%nCould not find comparability file: %s%n", Chicory.comparability_file);
        Runtime.chicoryLoaderInstantiationError = true;
        System.exit(1);
      }
      if (verbose) {
        System.out.printf("Read comparability from %s%n", Chicory.comparability_file);
        // Runtime.comp_info.dump();
      }
    }

    if (Chicory.doPurity()) {
      System.err.println("Executing a purity analysis is currently disabled");
      System.exit(1);
    } else if (Chicory.get_purity_file() != null) {
      readPurityFile(Chicory.get_purity_file(), Chicory.config_dir);
      doPurity = true;
    }

    initializeDeclAndDTraceWriters();

    // Setup the transformer
    ClassFileTransformer transformer;
    // use a special classloader to ensure correct version of BCEL is used
    ClassLoader loader = new ChicoryLoader();
    try {
      transformer =
          (ClassFileTransformer)
              loader.loadClass("daikon.chicory.Instrument").getDeclaredConstructor().newInstance();
    } catch (Exception e) {
      throw new RuntimeException("Unexpected error loading Instrument", e);
    }
    if (Chicory.debug) {
      System.out.printf(
          "Classloader of transformer = %s%n", transformer.getClass().getClassLoader());
    }

    // now turn on instrumentation
    if (verbose) {
      System.out.println("call addTransformer");
    }
    inst.addTransformer(transformer);

    if (verbose) {
      System.out.println("exit premain");
    }
  }

  /** Set up the declaration and dtrace writer. */
  // Runtime.dtrace is @GuardedBy("<self>") because in the Runtime class,
  // the printing of final lines and then closing of dtrace only happens
  // when the monitor of dtrace is held in order for the closing of the
  // trace to happen only once.  See Runtime.noMoreOutput() and
  // Runtime.addShutdownHook() for more details.  DeclWriter and DTraceWriter
  // never perform this operation (print final lines and close) on the
  // value of dtrace passed in, therefore they do not need to make use
  // of synchronization and their references to dtrace do not need to
  // be annotated with @GuardedBy("<self>").
  @SuppressWarnings("lock:argument")
  private static void initializeDeclAndDTraceWriters() {
    // The include/exclude filter are implemented in the transform,
    // so they don't need to be handled here.
    // (It looks like these can be called even if Runtime.dtrace is null...)
    Runtime.decl_writer = new DeclWriter(Runtime.dtrace);
    Runtime.dtrace_writer = new DTraceWriter(Runtime.dtrace);
  }

  /**
   * Reads purity file. Each line should contain exactly one method. Care must be taken to supply
   * the correct format.
   *
   * <p>From the Sun JDK API:
   *
   * <p>"The string is formatted as the method access modifiers, if any, followed by the method
   * return type, followed by a space, followed by the class declaring the method, followed by a
   * period, followed by the method name, followed by a parenthesized, comma-separated list of the
   * method's formal parameter types. If the method throws checked exceptions, the parameter list is
   * followed by a space, followed by the word throws followed by a comma-separated list of the
   * thrown exception types. For example:
   *
   * <p>public boolean java.lang.Object.equals(java.lang.Object)
   *
   * <p>The access modifiers are placed in canonical order as specified by "The Java Language
   * Specification". This is public, protected or private first, and then other modifiers in the
   * following order: abstract, static, final, synchronized native.
   *
   * @param purityFileName the purity file
   * @param pathLoc the relative path; interpret {@code purityFileName} with respect to it
   */
  private static void readPurityFile(File purityFileName, @Nullable File pathLoc) {
    pureMethods = new HashSet<String>();
    File purityFile = new File(pathLoc, purityFileName.getPath());
    String purityFileAbsolutePath = purityFile.getAbsolutePath();

    BufferedReader reader;
    try {
      reader = FilesPlume.newBufferedFileReader(purityFile);
    } catch (FileNotFoundException e) {
      System.err.printf(
          "%nCould not find purity file %s = %s%n", purityFileName, purityFileAbsolutePath);
      Runtime.chicoryLoaderInstantiationError = true;
      System.exit(1);
      throw new Error("Unreachable control flow");
    } catch (IOException e) {
      throw new UncheckedIOException(
          "Problem reading purity file " + purityFileName + " = " + purityFileAbsolutePath, e);
    }

    if (verbose) {
      System.out.printf("Reading '%s' for pure methods %n", purityFileName);
    }

    String line = null;
    do {
      try {
        line = reader.readLine();
      } catch (IOException e) {
        try {
          reader.close();
        } catch (IOException e2) {
          // Do nothing
        }
        throw new UncheckedIOException(
            "Error reading file " + purityFileName + " = " + purityFileAbsolutePath, e);
      }

      if (line != null) {
        pureMethods.add(line.trim());
        // System.out.printf("Adding '%s' to list of pure methods%n",
        //                   line);
      }
    } while (line != null);

    try {
      reader.close();
    } catch (IOException e) {
      System.err.println("Error while closing " + purityFileName + " after reading.");
      System.exit(1);
    }

    // System.out.printf("leaving purify file%n");

  }

  /** Return true iff Chicory has run a purity analysis or read a {@code *.pure} file. */
  @SuppressWarnings("nullness") // dependent:  pureMethods is non-null if doPurity is true
  // @EnsuresNonNullIf(result=true, expression="ChicoryPremain.pureMethods")
  @EnsuresNonNullIf(result = true, expression = "pureMethods")
  public static boolean shouldDoPurity() {
    return doPurity;
  }

  /**
   * Checks if member is one of the pure methods found in a purity analysis or supplied from a
   * {@code *.pure} file.
   *
   * @return true iff member is a pure method
   */
  // @RequiresNonNull("ChicoryPremain.pureMethods")
  @RequiresNonNull("pureMethods")
  public static boolean isMethodPure(Member member) {
    assert shouldDoPurity() : "Can't query for purity if no purity analysis was executed";

    // TODO just use Set.contains(member.toString()) ?
    for (String methName : pureMethods) {
      if (methName.equals(member.toString())) {
        return true;
      }
    }

    return false;
  }

  /** Return an unmodifiable Set of the pure methods. */
  // @RequiresNonNull("ChicoryPremain.pureMethods")
  @RequiresNonNull("pureMethods")
  public static Set<String> getPureMethods() {
    return Collections.unmodifiableSet(pureMethods);
  }

  /**
   * Classloader for the BCEL code. Using this classloader guarantees that we get the correct
   * version of BCEL and not a possible incompatible version from elsewhere on the user's classpath.
   * We also load daikon.chicory.Instrument via this (since that class is the user of all of the
   * BCEL classes). All references to BCEL must be within that class (so that all references to BCEL
   * will get resolved by this classloader).
   *
   * <p>There are several versions of BCEL that have been released:
   *
   * <ul>
   *   <li>the original 5.2 version
   *   <li>an interim 6.0 version
   *   <li>the offical 6.0 release version
   *   <li>the offical 6.1 release version
   *   <li>the PLSE 6.1 release version (includes LocalVariableGen fix)
   *   <li>the offical 6.2 release version (includes LocalVariableGen fix)
   *   <li>the offical 6.3 release version
   *   <li>the offical 6.3.1 release version
   *   <li>the offical 6.4.1 release version
   *   <li>the PLSE 6.4.1.1 release version (includes JDK 11 support)
   * </ul>
   *
   * <p>Note that both Chicory and DynComp use the ChicoryLoader to load BCEL and to verify that the
   * version loaded is acceptable. However, the official 6.1 release version is sufficient for
   * Chicory while DynComp requires the latest PLSE 6.4.1.1 version. Hence, this loader only checks
   * for the official 6.1 release version (or newer). After loading BCEL, DynComp will make an
   * additional check to verify that the 6.4.1.1 version has been loaded.
   *
   * <p>There are two classes present in 6.1 and subsequent releases that are not in previous
   * versions. Thus, we can identify version 6.1 (and later) of BCEL by the presence of the class:
   * org.apache.bcel.classfile.ConstantModule.class.
   *
   * <p>Earlier versions of Chicory inspected all version of BCEL found on the path and selected the
   * correct one, if present. We now (9/15/16) simplify this to say the first BCEL found must be the
   * correct one. This allows us to use the normal loader for all of the classes.
   */
  public static class ChicoryLoader extends ClassLoader {

    /** Log file if verbose is enabled. */
    public static final SimpleLog debug = new SimpleLog(ChicoryPremain.verbose);

    /**
     * Constructor for special BCEL class loader.
     *
     * @throws IOException if unable to load class
     */
    @SuppressWarnings("StaticAssignmentInConstructor") // sets static variable only if aborting
    public ChicoryLoader() throws IOException {

      String bcel_classname = "org.apache.bcel.Constants";
      String plse_marker_classname = "org.apache.bcel.classfile.ConstantModule";

      List<URL> bcel_urls = get_resource_list(bcel_classname);
      List<URL> plse_urls = get_resource_list(plse_marker_classname);

      if (plse_urls.size() == 0) {
        System.err.printf(
            "%nBCEL 6.1 or newer must be on the classpath.  Normally it is found in daikon.jar.%n");
        Runtime.chicoryLoaderInstantiationError = true;
        System.exit(1);
      }
      if (bcel_urls.size() < plse_urls.size()) {
        System.err.printf("%nCorrupted BCEL library, bcel %s, plse %s%n", bcel_urls, plse_urls);
        Runtime.chicoryLoaderInstantiationError = true;
        System.exit(1);
      }

      // No need to do anything if only our versions of bcel are present
      if (bcel_urls.size() == plse_urls.size()) {
        return;
      }

      URL bcel = bcel_urls.get(0);
      URL plse = plse_urls.get(0);
      if (!plse.getProtocol().equals("jar")) {
        System.err.printf("%nDaikon BCEL must be in jar file.  Found at %s%n", plse);
        Runtime.chicoryLoaderInstantiationError = true;
        System.exit(1);
      }
      if (!same_location(bcel, plse)) {
        System.err.printf(
            "%nDaikon BCEL (%s) is not first BCEL on the classpath (%s).%n", plse, bcel);
        Runtime.chicoryLoaderInstantiationError = true;
        System.exit(1);
      } else {
        try (JarFile bcel_jar = new JarFile(extract_jar_path(plse))) {
          debug.log("Daikon BCEL found in jar %s%n", bcel_jar.getName());
        }
      }
    }

    /**
     * Returns whether or not the two URL represent the same location for org.apache.bcel. Two
     * locations match if they refer to the same jar file or the same directory in the filesystem.
     */
    private static boolean same_location(URL url1, URL url2) {
      if (!url1.getProtocol().equals(url2.getProtocol())) {
        return false;
      }

      if (url1.getProtocol().equals("jar")) {
        // System.out.printf("url1 = %s, file=%s, path=%s, protocol=%s, %s%n",
        //                  url1, url1.getFile(), url1.getPath(),
        //                  url1.getProtocol(), url1.getClass());
        // System.out.printf("url2 = %s, file=%s, path=%s, protocol=%s, %s%n",
        //                    url2, url2.getFile(), url2.getPath(),
        //                    url2.getProtocol(), url1.getClass());
        String jar1 = extract_jar_path(url1);
        String jar2 = extract_jar_path(url2);
        return jar1.equals(jar2);
      } else if (url1.getProtocol().equals("file")) {
        String loc1 = url1.getFile().replaceFirst("org\\.apache\\.bcel\\..*$", "");
        String loc2 = url2.getFile().replaceFirst("org\\.apache\\.bcel\\..*$", "");
        return loc1.equals(loc2);
      } else {
        throw new Error("unexpected protocol " + url1.getProtocol());
      }
    }

    /**
     * Returns the pathname of a jar file specified in the URL. The protocol must be 'jar'. Only
     * file jars are supported.
     */
    private static String extract_jar_path(URL url) {
      assert url.getProtocol().equals("jar") : url.toString();

      // Remove the preceeding 'file:' and trailing '!filename'
      String path = url.getFile();
      path = path.replaceFirst("^[^:]*:", "");
      path = path.replaceFirst("![^!]*$", "");

      return path;
    }

    /**
     * Get all of the URLs that match the specified name in the classpath. The name should be in
     * normal classname format (eg, org.apache.bcel.Const). An empty list is returned if no names
     * match.
     */
    @SuppressWarnings("JdkObsolete") // ClassLoader.getSystemResources returns an Enumeration
    static List<URL> get_resource_list(String classname) throws IOException {

      String name = classname_to_resource_name(classname);
      Enumeration<URL> enum_urls = ClassLoader.getSystemResources(name);
      List<URL> urls = new ArrayList<>();
      while (enum_urls.hasMoreElements()) {
        urls.add(enum_urls.nextElement());
      }
      return urls;
    }

    /**
     * Changes a class name in the normal format (eg, org.apache.bcel.Const) to that used to lookup
     * resources (eg, org/apache/bcel/Const.class).
     */
    private static String classname_to_resource_name(String name) {
      return (name.replace(".", "/") + ".class");
    }

    @Override
    protected Class<?> loadClass(@BinaryName String name, boolean resolve)
        throws java.lang.ClassNotFoundException {

      return super.loadClass(name, resolve);
    }
  }
}
