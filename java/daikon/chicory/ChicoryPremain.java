package daikon.chicory;

//import harpoon.ClassFile.HMethod;

import java.lang.instrument.*;
import java.lang.reflect.Member;
import java.io.*;
import java.io.File;
import java.util.*;

import daikon.Chicory;

public class ChicoryPremain {

  public static boolean debug = false;

  /** Set of pure methods returned by Alexandru Salcianu's purity analysis **/
  private static Set<String> pureMethods = null;

  /** True iff Chicory should add variables based on pure methods during instrumentation **/
  private static boolean doPurity = false;

  /**
   * This method is the entry point of the java agent.  Its main
   * purpose is to set up the transformer so that when classes from
   * the target app are loaded, they are first transformed.
   *
   * This method also sets up some other initialization tasks: it
   * connects to Daikon over a port if necessary, or reads in a purity
   * analysis.
   */
  public static void premain (String agentArgs, Instrumentation inst) {

    //System.out.format ("In premain, agentargs ='%s', " +
    //                   "Instrumentation = '%s'%n", agentArgs, inst);


    // Parse our arguments using Chicory's argument parser
    String[] args = agentArgs.split ("  *");
    Chicory chicory = new Chicory();
    chicory.parse_args (args, true);
    debug = chicory.debug;

    // Open the dtrace file
    if (chicory.getDaikonPort() != -1)
    {
        Runtime.setDtraceOnlineMode(chicory.getDaikonPort());
    }
    else if (chicory.trace_file_name == null) {
      File trace_file_path = new File (chicory.output_dir, "dtrace.gz");
      Runtime.setDtraceMaybe (trace_file_path.toString());
    } else {
      File trace_file_path = new File (chicory.output_dir,
                                       chicory.trace_file_name);
      Runtime.setDtrace (trace_file_path.toString(), false);
    }

    // Setup argument fields in Runtime
    Runtime.nesting_depth       = chicory.nesting_depth;
    Runtime.linked_lists        = chicory.linked_lists;
    Runtime.daikon_omit_regex   = chicory.daikon_omit_regex;
    Runtime.daikon_include_regex= chicory.daikon_include_regex;
    if (chicory.comparability_filename != null) {
      Runtime.comp_info = new DeclReader();
      Runtime.comp_info.read (chicory.comparability_filename);
      if (debug) {
        System.out.printf ("Read comparability from %s%n",
                           chicory.comparability_filename);
        Runtime.comp_info.dump();
      }
    }

    if (chicory.doPurity())
    {
        throw new RuntimeException("Executing a purity analysis is currently disabled");

        //runPurityAnalysis(chicory.target_program);
        //writePurityFile(chicory.target_program + ".pure", chicory.getConfigDir());
        //doPurity = true;
    }
    else if (chicory.getPurityFileName() != null)
    {
        readPurityFile(chicory.getPurityFileName(), chicory.getConfigDir());
        doPurity = true;
    }

    // Setup the declaration and dtrace writer.  The include/exclude filter are
    // implemented in the transform, so they don't need to be handled
    // here.
    Runtime.decl_writer = new DeclWriter (Runtime.dtrace);
    Runtime.dtrace_writer = new DTraceWriter (Runtime.dtrace);

    // Setup the transformer
    RetTransform transformer = new RetTransform ();
    inst.addTransformer (transformer);
  }

  /**
   * Reads purityFileName.  Each line should contain exactly one method.
   * Care must be taken to supply the correct format.
   *
   * From the Sun JDK API:
   *
   * "The string is formatted as the method access modifiers, if any,
   * followed by the method return type, followed by a space, followed
   * by the class declaring the method, followed by a period, followed
   * by the method name, followed by a parenthesized, comma-separated
   * list of the method's formal parameter types. If the method throws
   * checked exceptions, the parameter list is followed by a space,
   * followed by the word throws followed by a comma-separated list of
   * the thrown exception types. For example:
   *
   * public boolean java.lang.Object.equals(java.lang.Object)
   *
   * The access modifiers are placed in canonical order as specified
   * by "The Java Language Specification".  This is public, protected
   * or private first, and then other modifiers in the following
   * order: abstract, static, final, synchronized native."
   */
   private static void readPurityFile(String purityFileName, String pathLoc)
   {
        pureMethods = new HashSet<String>();

        BufferedReader reader = null;
        try
        {
            reader = new BufferedReader(new FileReader(
                    new File(pathLoc, purityFileName)));
        }
        catch (FileNotFoundException e)
        {
            throw new Error("Could not find file " + purityFileName + ". Got exception " + e);
        }

        if (Chicory.verbose)
            System.out.printf("Reading %s for pure methods %n", purityFileName);

        String line;
        do
        {
            try
            {
                line = reader.readLine();
            }
            catch (IOException e)
            {
                throw new Error("Error reading file " + purityFileName + ". Got exception e");
            }

            if (line != null)
                pureMethods.add(line.trim());
        }
        while (line != null);

        try
        {
            reader.close();
        }
        catch (IOException e)
        {
        }

    }

   /**
    * Write a *.pure file to the given location
    * @param fileName Where to write the file to (full path)
    */
   private static void writePurityFile(String fileName, String parentDir)
    {
        PrintWriter pureFileWriter = null;
        try
        {
            pureFileWriter = new PrintWriter(new File(parentDir, fileName));
        }
        catch (FileNotFoundException e)
        {
            throw new Error("Could not open " + fileName + " for writing. Got exception " + e);
        }

        System.out.printf("Writing pure methods to %s%n", fileName);

        for (String methodName : pureMethods)
        {
            pureFileWriter.println(methodName);
        }

        pureFileWriter.close();
    }

  /**
   * Invokes Alexandru Salcianu's purity analysis on given application.
   * Populates the pureMethods Set with pure (non side-effecting) methods.
   * @param targetApp Name of the class whose main method is the entry point of the application
   */
//  private static void runPurityAnalysis(String targetApp)
//  {
//      //Example args: --pa:assignable -q  -c DataStructures.StackAr
//      String[] args = new String[] {"--pa:assignable", "-c", targetApp};
//
//      Set<HMethod> pureHMethods = harpoon.Main.SAMain.getPureMethods(args);
//
//      pureMethods = new HashSet<String> ();
//      for(HMethod meth: pureHMethods)
//      {
//          pureMethods.add(meth.toString());
//      }
//  }

  /**
   * Return true iff Chicory has run a purity analysis or read a *.pure file
   */
  public static boolean shouldDoPurity()
  {
      return doPurity;
  }

  /**
   * Checks if member is one of the pure methods found in a purity analysis
   * or supplied from a *.pure file.
   *
   * @return true iff member is a pure method
   */
  public static boolean isMethodPure(Member member)
  {
      assert shouldDoPurity() : "Can't query for purity if no purity analysis was executed";

      //TODO just use Set.contains(member.toString()) ?
      for(String methName: pureMethods)
      {
          if (methName.equals(member.toString()))
              return true;
      }

      return false;
  }

  /**
   * Return an unmodifiable Set of the pure methods
   */
  public static Set<String> getPureMethods()
  {
      return Collections.unmodifiableSet(pureMethods);
  }


}
