// Main routine for Daikon invariant detector

package daikon;

import daikon.split.*;

import java.util.*;
import java.io.*;

public class Daikon {

  public final static boolean disable_splitting = false;
  // public final static boolean disable_splitting = true;

  public final static boolean disable_ternary_invariants = false;
  // public final static boolean disable_ternary_invariants = true;

  // Change this at your peril; high costs in time and space for "false".
  public final static boolean check_program_types = true;
  // public final static boolean check_program_types = false;

  public final static boolean disable_modbit_check_message = false;
  public final static boolean disable_modbit_check_error = true;

  // The two arguments to daikon.Daikon are a comma-separated list of
  // declaration files, and a comma-separated list of data trace files.
  //
  // So you should be able to do
  //
  //   java daikon.Daikon a.decls,b.decls,c.decls  g.dtrace
  //
  // I realize this isn't the ideal user interface; I can change it if you
  // like.  (For instance, it would probably be more convenient for you if you
  // just supplied all the files on the command line and I inferred which was a
  // declaration file and which was a trace file, right?)

  /**
   * Two arguments:  comma-separated list of declaration files,
   * comma-separated list of data trace files.
   */
  public static void main(String[] args) {

    // First check that all the file names are OK, so we don't do lots of
    // processing only to bail out at the end.
    for (int i=0; i<args.length; i++) {
      String file = args[i];
      if ((file.indexOf(".decls") == -1) &&
          (file.indexOf(".dtrace") == -1)) {
        throw new Error("Unrecognized file type: " + file);
      }
    }

    PptMap all_ppts = new PptMap();

    int num_decl_files = 0;
    int num_dtrace_files = 0;

    System.out.print("Reading input files ");
    try {
      for (int i=0; i<args.length; i++) {
        System.out.print(".");
        String file = args[i];
        if (file.indexOf(".decls") != -1) {
          FileIO.read_declaration_file(file, all_ppts, null);
          num_decl_files++;
        } else if (file.indexOf(".dtrace") != -1) {
          FileIO.read_data_trace_file(file, all_ppts, null);
          num_dtrace_files++;
        } else {
          System.out.println();
          throw new Error("Unrecognized file type: " + file);
        }
      }
      System.out.println();
    } catch (IOException e) {
      System.out.println();
      e.printStackTrace();
      throw new Error(e.toString());
    }
    // Jikes complains when I put this all in one big string.
    System.out.print("Read " + num_decl_files + " declaration file" +
                       ((num_decl_files == 1) ? "" : "s"));
    System.out.print(", " + num_dtrace_files + " dtrace file");
    System.out.println(((num_dtrace_files == 1) ? "" : "s") + ".");

    // Old version that took only two arguments
    // if (args.length != 2)
    //   throw new Error("Got " + args.length + " aguments, expected two:  comma-separated lists of decl files, trace files.");
    // Vector decl_files = UtilMDE.tokens(args[0], ",");
    // FileIO.read_declaration_files(decl_files, all_ppts, null);
    // // System.out.println("Program points:");
    // // for (Iterator itor = all_ppts.keySet().iterator() ; itor.hasNext() ; ) {
    // //   System.out.println(itor.next());
    // // }
    // Vector dtrace_files = UtilMDE.tokens(args[1], ",");
    // FileIO.read_data_trace_files(dtrace_files, all_ppts, null);


    for (Iterator itor = new TreeSet(all_ppts.keySet()).iterator() ; itor.hasNext() ; ) {
      String ppt_name = (String) itor.next();
      PptTopLevel ppt = (PptTopLevel) all_ppts.get(ppt_name);
      if (ppt.num_samples() > 0) {
        // System.out.println(ppt.name + ": " + ppt.num_samples() + " samples, "
        //                    + ppt.num_values() + " values, "
        //                    + "ratio = " + ((double)ppt.num_samples()) / ((double)ppt.num_values()));
        // System.out.println("start dump-----------------------------------------------------------------");
        // System.out.println(ppt.name);
        // ppt.values.dump();
        // System.out.println("end dump-------------------------------------------------------------------");
        ppt.initial_processing();
        if (! disable_splitting) {
          Splitter[] pconds = SplitterList.get(ppt.name);
          if (Global.debugPptSplit)
            System.out.println("Got " + ((pconds == null)
                                         ? "no"
                                         : Integer.toString(pconds.length))
                               + " splitters for " + ppt.name);
          if (pconds != null)
            ppt.addConditions(pconds);
        }
        ppt.print_invariants_maybe();
        // Clear memory
        ppt.values = null;
        ppt.clear_view_caches();
        for (int i=0; i<ppt.views_cond.size(); i++) {
          PptConditional pcond = (PptConditional) ppt.views_cond.elementAt(i);
          pcond.values = null;
          pcond.clear_view_caches();
        }
      }
    }

    Global.output_statistics();

    // Old implementation that didn't interleave invariant inference and
    // reporting.
    // for (Iterator itor = all_ppts.values().iterator() ; itor.hasNext() ; ) {
    //   PptTopLevel ppt = (PptTopLevel) itor.next();
    //   ppt.initial_processing();
    // }
    // // Now examine the invariants.
    // System.out.println("Examining the invariants.");
    // for (Iterator itor = new TreeSet(all_ppts.keySet()).iterator() ; itor.hasNext() ; ) {
    //   String ppt_name = (String) itor.next();
    //   PptTopLevel ppt_tl = (PptTopLevel) all_ppts.get(ppt_name);
    //   ppt_tl.print_invariants_maybe();
    // }

    System.out.println("Exiting");

  }

}
