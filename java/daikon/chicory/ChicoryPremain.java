package daikon.chicory;

import java.lang.instrument.*;
import java.security.*;
import java.io.*;
import java.io.File;

import daikon.Chicory;

public class ChicoryPremain {

  public static void premain (String agentArgs, Instrumentation inst) {

    // System.out.format ("In premain, agentargs ='%s', " +
    //                   "Instrumentation = '%s'\n", agentArgs, inst);

    int local = Runtime.nonce;

    // Parse our arguments using Chicory's argument parser
    String[] args = agentArgs.split ("  *");
    Chicory chicory = new Chicory();
    chicory.parse_args (args, true);

    // Open the dtrace file
    if(chicory.daikon_cmd_online != null)
    {       
        chicory.runDaikon();
     
        
        
        final StreamRedirectThread err_thread
          = new StreamRedirectThread("daik_err", chicory.daikon_proc.getErrorStream(), System.err);
        final StreamRedirectThread out_thread
          = new StreamRedirectThread("daik_out", chicory.daikon_proc.getInputStream(), System.out);
        System.out.println("starting daikon error and output threads...");
        err_thread.start();
        out_thread.start();
        
        Runtime.setDaikonInfo(err_thread, out_thread, chicory.daikon_proc);
       
       
        Runtime.setDtraceOnlineMode(chicory.daikon_proc.getOutputStream());
        //Runtime.setDtraceMaybe("dtrace.dtrace");
        
        java.lang.Runtime.getRuntime().addShutdownHook(new Thread()
                {
                    public void run()
                    {
                     System.out.println("OTHER HOOK");
                     Runtime.endDaikon();
                     
                    }
                });
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

    // Setup the declaration and dtrace writer.  The include/exclude filter are
    // implemented in the transform, so they don't need to be handled
    // here.
    Runtime.decl_writer = new DeclWriter (Runtime.dtrace,
                                          Runtime.nesting_depth, new String[0],
                                          null);
    Runtime.dtrace_writer = new DTraceWriter (Runtime.dtrace,
                                          Runtime.nesting_depth, new String[0],
                                          null);

    // Setup the transformer
    RetTransform transformer = new RetTransform (chicory);
    inst.addTransformer (transformer);
  }


}
