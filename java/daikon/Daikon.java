// Main routine for Daikon invariant detector

package daikon;

import daikon.split.*;
import daikon.inv.Invariant;

import java.util.*;
import java.io.*;

import com.oroinc.text.regex.*;
import gnu.getopt.*;

public final class Daikon {

  public final static boolean disable_splitting = false;
  // public final static boolean disable_splitting = true;

  public final static boolean disable_ternary_invariants = false;
  // public final static boolean disable_ternary_invariants = true;

  // Change this at your peril; high costs in time and space for "false",
  // because so many more invariants get instantiated.
  public final static boolean check_program_types = true;
  // public final static boolean check_program_types = false;

  // Problem with setting this to true:
  //  get no invariants over any value that can ever be missing
  // Problem with setting this to false:
  //  due to differrent number of samples, IsEquality is non-transitive
  // public final static boolean invariants_check_canBeMissing = false;
  public final static boolean invariants_check_canBeMissing = true;

  public final static boolean disable_modbit_check_message = false;
  // Not a good idea to set this to true, as it is too easy to ignore the
  // warnings and the modbit problem can cause an error later.
  public final static boolean disable_modbit_check_error = false;

  // When true, don't print invariants when their controlling ppt
  // already has them.  For example, this is the case for invariants
  // in public methods which are already given as part of the object
  // invariant for that class.
  public static boolean suppress_implied_controlled_invariants = false; 
  // public static boolean suppress_implied_controlled_invariants = true; 

  public static Pattern ppt_regexp;
  // I appear to need both of these variables.  Or do I?  I don't know.
  public static FileOutputStream inv_ostream;
  public static ObjectOutputStream inv_oostream;


  static String usage =
    "Daikon invariant detector.\n"
    + "Copyright 1998-2001 by Michael Ernst <mernst@lcs.mit.edu>\n"
    + "Usage:\n"
    + "    java daikon.Daikon [flags...] files...\n"
    + "  Each file is a declaration file or a data trace file; the file type\n"
    + "  is determined by the file name (containing \".decls\" or \".dtrace\").\n"
    + "  Flags:\n"
    + "    -h		     Print this usage message\n"
    + "    -r ppt_regexp     Only process program points matching the regexp\n"
    + "    -o inv_file       Serialize invariants to the specified file;\n"
    + "                        they can later be postprocessed, compared, etc.\n"
    + "    --suppress_cont   Suppress display of implied invariants (by controlling ppt).\n"
    + "    --prob_limit pct  Sets the probability limit for justifying invariants.\n"
    + "                        The default is 1%.  Smaller values yield stronger filtering.\n"
    ;

  /**
   * The arguments to daikon.Daikon are file names; declaration file names end
   * in ".decls" and data trace file names end in ".dtrace".
   **/
  public static void main(String[] args) {
    Set decl_files = new HashSet();
    Set dtrace_files = new HashSet();

    if (args.length == 0) {
      System.out.println("Daikon error: no files supplied on command line.");
      System.out.println(usage);
      System.exit(1);
    }

    LongOpt[] longopts = new LongOpt[] {
      new LongOpt("suppress_cont", LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt("prob_limit", LongOpt.REQUIRED_ARGUMENT, null, 0)
    };
    Getopt g = new Getopt("daikon.Daikon", args, "ho:r:", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch(c) {
      case 0:
	// got a long option
	String option_name = longopts[g.getLongind()].getName();
	if ("suppress_cont".equals(option_name)) {
	  suppress_implied_controlled_invariants = true;
	} else if ("prob_limit".equals(option_name)) {
	  Invariant.probability_limit = 0.01 * Double.parseDouble(g.getOptarg());
	} else {
	  throw new RuntimeException("Unknown long option received: " + option_name);
	}
	break;
      case 'h':
        System.out.println(usage);
        System.exit(1);
        break;
      case 'r':
        // I should permit multiple regexps; also negated regexps.
        if (ppt_regexp != null)
          throw new Error("multiple regular expressions supplied on command line");
        try {
          String regexp_string = g.getOptarg();
          // System.out.println("Regexp = " + regexp_string);
          ppt_regexp = Global.regexp_compiler.compile(regexp_string);
        } catch (Exception e) {
          throw new Error(e.toString());
        }
        break;
        //
      case 'o':
        if (inv_ostream != null)
          throw new Error("multiple serialization output files supplied on command line");
        try {
          String inv_filename = g.getOptarg();
          System.out.println("Inv filename = " + inv_filename);
          inv_ostream = new FileOutputStream(inv_filename);
          inv_oostream = new ObjectOutputStream(inv_ostream);
          // This sends the header immediately; irrelevant for files.
          // inv_oostream.flush();
        } catch (Exception e) {
          throw new Error(e.toString());
        }
        break;
        //
      case '?':
        break; // getopt() already printed an error
        //
      default:
        System.out.print("getopt() returned " + c + "\n");
        break;
      }
    }

    // First check that all the file names are OK, so we don't do lots of
    // processing only to bail out at the end.
    for (int i=g.getOptind(); i<args.length; i++) {
      String arg = args[i];
      // These aren't "endsWith()" because there might be a suffix on the end
      // (eg, a date).
      if (! new File(arg).exists()) {
        throw new Error("File " + arg + " not found.");
      }
      if (arg.indexOf(".decls") != -1) {
        decl_files.add(arg);
      } else if (arg.indexOf(".dtrace") != -1) {
        dtrace_files.add(arg);
      } else {
        throw new Error("Unrecognized argument: " + arg);
      }
    }

    PptMap all_ppts = new PptMap();

    int num_decl_files = decl_files.size();
    int num_dtrace_files = dtrace_files.size();

    try {
      System.out.print("Reading declaration files ");
      for (Iterator i = decl_files.iterator(); i.hasNext(); ) {
        System.out.print(".");  // show progress
        String file = (String) i.next();
        FileIO.read_declaration_file(file, all_ppts, ppt_regexp);
      }
      System.out.println();

      System.out.print("Reading data trace files ");
      for (Iterator i = dtrace_files.iterator(); i.hasNext(); ) {
        System.out.print(".");
        String file = (String) i.next();
        FileIO.read_data_trace_file(file, all_ppts, ppt_regexp);
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

    // Retrieve Ppt objects in sorted order.
    // Use a custom comparator for a specific ordering
    TreeSet all_ppts_sorted = new TreeSet(new PptOrderComparator());
    all_ppts_sorted.addAll(all_ppts.values());
    for (Iterator itor = all_ppts_sorted.iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
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
        {
          // Clear memory
          ppt.set_values_null();
          ppt.clear_view_caches();
          for (int i=0; i<ppt.views_cond.size(); i++) {
            PptConditional pcond = (PptConditional) ppt.views_cond.elementAt(i);
            pcond.set_values_null();
            pcond.clear_view_caches();
          }
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

    if (inv_ostream != null) {
      try {
        inv_oostream.writeObject(all_ppts);
        inv_oostream.flush();
        inv_oostream.close();
        // inv_ostream.close();    // is this necessary?
      } catch (IOException e) {
        e.printStackTrace();
        throw new Error(e.toString());
      }
    }

    System.out.println("Exiting");

  }

  // It might make more sense to put the sorting into
  // PptMap.sortedIterator(), for example, but it's in here for now

  // Orders ppts by the name, except . and : are swapped
  //   so that Foo:::OBJECT and Foo:::CLASS are processed before Foo.method.
  private static class PptOrderComparator
    implements Comparator
  {
    static String swap(String s, char a, char b)
    {
      final char magic = '\255';
      return s.replace(a, magic).replace(b, a).replace(magic, b);
    }

    public int compare(Object o1, Object o2)
    {
      String name1 = ((Ppt) o1).name;
      String name2 = ((Ppt) o2).name;

      String swapped1 = swap(name1, '.', ':');
      String swapped2 = swap(name2, '.', ':');
      return swapped1.compareTo(swapped2);
    }

    public boolean equals(Object o)
    {
      return (o instanceof PptOrderComparator);
    }
  }

}
