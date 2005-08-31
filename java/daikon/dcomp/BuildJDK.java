// Created on Nov 9, 2004 by saff for edu.mit.csail.pag.testfactoring
package daikon.dcomp;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Date;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.zip.ZipException;

import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.generic.*;
import org.apache.bcel.*;

/**
 * Converts each file in the JDK.  Each method is doubled.  The new methods
 * are distinguished by a final parameter of type ?? and are instrumented to
 * track comparabiity.  User code will call the new methods, but
 * instrumentation code is unchanged and calls the original methods.
 */
public class BuildJDK {

  /**
   * Given an explicit rt.jar filename, or a root JDK or JRE directory, finds
   * the rt.jar file.
   */
  public static class RtJarFinder {
    private final String arg;

    /**
     * @param arg
     */
    public RtJarFinder(String arg) {
      super();
      this.arg = arg;
    }

    /**
     * @param spaceSeparatedJarLocations a list of potential jar filenames to
     *          look for, in case arg was a directory. For example, if arg was
     *          /usr/bin/jd2sdk, and spaceSeparatedJarLocations was
     *          "jre/lib/rt.jar", this could return
     *          /usr/bin/jd2sdk/jre/lib/rt.jar, if it existed.
     * @return
     */
    public String findRtJar(String spaceSeparatedJarLocations) {
      String jarFilename = arg;
      if (!arg.endsWith("jar")) {
        for (String candidate : spaceSeparatedJarLocations.split(" ")) {
          String rt = jarFilename + candidate;
          if (exists(rt))
            return rt;
        }
      }
      return jarFilename;
    }

    /**
     * @param libRt
     * @return true iff libRt exists in the filesystem.
     */
    protected boolean exists(String libRt) {
      return new File(libRt).exists();
    }
  }

  private static boolean verbose = false;

  /** Whether or not to instrument java.lang.Object **/
  private static boolean skip_object = true;

  private static boolean test_stack = true;

  private int _numFilesProcessed = 0;

  /**
   * BuildJDK <jarfile> <dest> <prefix>
   *
   *    <jarfile>   - jarfile to process
   *    <dest>      - destination directory in which to place instrumented
   *                  classes
   *    <prefix>    - prefix of classes to be translated
   *
   * Instruments each class file in jarfile that begins with prefix
   * and puts the results in dest.
   */
  public static void main(String[] args) throws java.io.IOException {


    assert (args.length == 2 || args.length == 3)
      : "2 args req: jarfile dest-dir [class-prefix]";

    final String potential_jar_file_name = args[0];
    String dest_dir = args[1];
    String prefix = args.length == 3 ? args[2] : "";

    BuildJDK build = new BuildJDK();
    System.out.println("Starting at " + new Date());
    JarFile jfile = getJarFile(potential_jar_file_name);

    build.translate_classes(jfile, dest_dir, prefix, "");

    // Create the various helper classes
    // build.dump_helper_classes(dest_dir);

    System.out.println("done at " + new Date());
  }

  private static JarFile getJarFile(String potentialJarFileName)
      throws IOException, ZipException {
    JarFile jfile;
    final String p = potentialJarFileName;
    try {
      String jar_name = findRtJarFilename(p);
      System.out.printf ("using jar file %s\n", jar_name);
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

  void translate_classes(JarFile jfile, String dest, String prefix,
      String prefixOfFilesToInclude) throws java.io.IOException {

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
        final String entryName = entry.getName();
        if (!entryName.startsWith(prefixOfFilesToInclude)
            && !entryName.startsWith("META-INF"))
          continue;
        if (entryName.endsWith("/"))
          continue;
        if (!entryName.endsWith(".class")
            || (skip_object && entryName.equals("java/lang/Object.class"))
            || (!prefix.equals ("") && !entryName.startsWith (prefix))) {
          File destfile = new File(entryName);
          File dir = new File(dfile, destfile.getParent());
          dir.mkdirs();
          File destpath = new File(dir, destfile.getName());
          if (verbose)
            System.out.println("Copying Object or non-classfile: " + destpath);
          copyStreams(jfile.getInputStream(entry), new FileOutputStream(
              destpath));
          continue;
        }

        // Get the binary for this class
        InputStream is = jfile.getInputStream(entry);
        ClassParser parser = new ClassParser(is, entryName);
        if (entryName.equals ("java/lang/ClassLoader.class"))
          parser
            = new ClassParser ("/scratch/jhp/jdk/java/lang/ClassLoader.class");
        JavaClass jc = parser.parse();

        classmap.put(jc.getClassName(), jc);
      }

      if (false) {
        processClassFile (classmap, dfile,
                          "java.awt.TexturePaintContext$Byte");
        System.exit(0);
      }

      // Process each file read.
      for (String classname : classmap.keySet()) {
        JavaClass jc = classmap.get (classname);
        if (test_stack) {
          System.out.printf ("Testing class %s%n", classname);
          try {
            TypeStack.testJavaClass (jc);
          } catch (Throwable t) {
            System.out.printf ("Error: %s%n", t.getMessage());
            t.printStackTrace();
          }
          continue;
        }
        DCInstrument dci = new DCInstrument (jc, true, null);
        try {
          processClassFile(classmap, dfile, classname);
        } catch (Throwable e) {
          throw new Error ("Couldn't instrument " + classname, e);
        }
      }

      // Create the InterfaceMarker class (used to identify our empty
      // constructors)
      ClassGen dcomp_marker = new ClassGen("java.lang.DCompMarker",
        "java.lang.Object", "DCompMarker.class", Constants.ACC_INTERFACE
        | Constants.ACC_PUBLIC | Constants.ACC_ABSTRACT, new String[0]);
      dcomp_marker.getJavaClass().dump (new File(dest, "java"
         + File.separator + "lang" + File.separator + "DCompMarker.class"));

    } catch (Exception e) {
      throw new Error(e);
    }
  }

  private void processClassFile(Map<String, JavaClass> classmap, File dfile,
                                String classname) throws java.io.IOException {
    JavaClass jc = classmap.get(classname);
    if (verbose)
      System.out.printf("processing target %s\n", classname);
    DCInstrument dci = new DCInstrument (jc, true, null);
    JavaClass inst_jc = dci.instrument_jdk();
    // Build the translated class, wrapper class, and matching interface
    // JavaClass ijc = ii.getInstrumentedClass(jc);
    // Write them out
    File classfile = new File(classname.replace('.', '/') + ".class");
    File dir = new File(dfile, classfile.getParent());
    dir.mkdirs();
    File classpath = new File(dir, classfile.getName());
    inst_jc.dump (classpath);
    _numFilesProcessed++;
    if ((_numFilesProcessed % 100) == 0)
      System.out.printf("Processed %d/%d classes at " + new Date() + "\n",
          _numFilesProcessed, classmap.size());
  }

  /** Copy our various helper classes to java/lang **/
  private void dump_helper_classes (String dest) throws java.io.IOException {

    File dir = new File (dest, "java" + File.separator + "lang");

    ClassParser parser = new ClassParser ("bin/java/lang/ArrayAccessors.class");
    JavaClass jc = parser.parse();
    jc.dump (new File (dir, "ArrayAccessors.class"));

    parser = new ClassParser("bin/java/lang/GenericInterface.class");
    jc = parser.parse();
    jc.dump (new File (dir, "GenericInterface.class"));

    parser = new ClassParser("bin/java/lang/ObjectHelper.class");
    jc = parser.parse();
    jc.dump (new File (dir, "ObjectHelper.class"));

    parser = new ClassParser("bin/java/lang/StaticInterface.class");
    jc = parser.parse();
    jc.dump (new File (dir, "StaticInterface.class"));

  }

  private List<String> classesWithoutInterfaces() {
    return Arrays.asList("java.lang.Object", "java.lang.String",
                         "java.lang.Class");
  }
}
