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

  public static void premain (String agentArgs, Instrumentation inst) {

    //System.out.format ("In premain, agentargs ='%s', " +
    //                   "Instrumentation = '%s'%n", agentArgs, inst);
      
    
    // Parse our arguments using Chicory's argument parser
    String[] args = agentArgs.split ("  *");
    Chicory chicory = new Chicory();
    chicory.parse_args (args, true);
    debug = chicory.debug;

    // Open the dtrace file
    if(chicory.getDaikonPort() != -1)
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
    
    
    if(chicory.doPurity())
    {
        doPurity = true;
        runPurityAnalysis(chicory.target_program);
        writePurityFile(chicory.target_program + ".pure");
    }
    else if(chicory.getPurityFileName() != null)
    {
        doPurity = true;
        readPurityFile(chicory.getPurityFileName());
    }

    // Setup the declaration and dtrace writer.  The include/exclude filter are
    // implemented in the transform, so they don't need to be handled
    // here.
    Runtime.decl_writer = new DeclWriter (Runtime.dtrace,
                                          Runtime.nesting_depth);
    Runtime.dtrace_writer = new DTraceWriter (Runtime.dtrace);

    // Setup the transformer
    RetTransform transformer = new RetTransform ();
    inst.addTransformer (transformer);
  }
  
  /**
   * Reads purityFileName 
   */
   private static void readPurityFile(String purityFileName)
   {
        pureMethods = new HashSet<String>();

        BufferedReader reader = null;
        try
        {
            reader = new BufferedReader(new FileReader(
                    purityFileName));
        }
        catch (FileNotFoundException e)
        {
            throw new Error("Could not find file " + purityFileName + ". Got exception " + e);
        }

        if(Chicory.verbose)
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
            
            if(line != null)
                pureMethods.add(line);
        }
        while(line != null);
        
        try
        {
            reader.close();
        }
        catch (IOException e)
        {
        }

    }

   private static void writePurityFile(String fileName)
    {
        PrintWriter pureFileWriter = null;
        try
        {
            pureFileWriter = new PrintWriter(fileName);
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
  private static void runPurityAnalysis(String targetApp)
  {
      //Example args: --pa:assignable -q  -c DataStructures.StackAr
      String[] args = new String[] {"--pa:assignable", "-c", targetApp};
      
      //Set<HMethod> pureHMethods = harpoon.Main.SAMain.getPureMethods(args);
      
      pureMethods = new HashSet<String> ();
      //for(HMethod meth: pureHMethods)
      {
      //    pureMethods.add(meth.toString());
      }
  }
  
  public static boolean shouldDoPurity()
  {
      return doPurity;
  }

  public static boolean isMethodPure(Member member)
  {
      assert shouldDoPurity() : "Can't query for purity if no purity analysis was executed";
      
      for(String methName: pureMethods)
      {
          //TODO a more robust check for equality than string comparison?
          if(methName.equals(member.toString()))
              return true;
      }
      
      return false;
  }


}
