package daikon;

import java.util.*;
import java.io.*;
import gnu.getopt.*;
import utilMDE.UtilMDE;

public class UnionInvariants {

  public static final String lineSep = Global.lineSep;

  // Non-empty program points in the input files must be distinct.
  private static String usage =
    UtilMDE.join(new String[] {
      "Usage: java daikon.UnionInvariants [OPTION]... FILE",
      "  -h, --" + Daikon.help_SWITCH,
      "      Display this usage message",
      "  --" + Daikon.suppress_redundant_SWITCH,
      "      Suppress display of logically redundant invariants.",
    }, lineSep);

  public static void main(String[] args)
    throws Exception
  {
    daikon.Logger.setupLogs(daikon.Logger.INFO);

    File inv_file = null;

    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.suppress_cont_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.suppress_post_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.suppress_redundant_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
    };
    Getopt g = new Getopt("daikon.UnionInvariants", args, "ho:", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch(c) {
      case 0:
        // got a long option
        String option_name = longopts[g.getLongind()].getName();
        if (Daikon.help_SWITCH.equals(option_name)) {
          System.out.println(usage);
          System.exit(1);
        } else if (Daikon.suppress_cont_SWITCH.equals(option_name)) {
	  Daikon.suppress_implied_controlled_invariants = true;
	} else if (Daikon.suppress_post_SWITCH.equals(option_name)) {
	  Daikon.suppress_implied_postcondition_over_prestate_invariants = true;
	} else if (Daikon.suppress_redundant_SWITCH.equals(option_name)) {
	  Daikon.suppress_redundant_invariants_with_simplify = true;
	} else {
	  throw new RuntimeException("Unknown long option received: " +
                                     option_name);
	}
        break;
      case 'h':
        System.out.println(usage);
        System.exit(1);
        break;
      case 'o':
	if (inv_file != null)
	  throw new Error("multiple serialization output files supplied on command line");

        String inv_filename = g.getOptarg();
        System.out.println("Inv filename = " + inv_filename);
        inv_file = new File(inv_filename);

        if (! UtilMDE.canCreateAndWrite(inv_file)) {
          throw new Error("Cannot write to file " + inv_file);
        }
        break;
        //
      case '?':
        break; // getopt() already printed an error
      default:
        System.out.println("getopt() returned " + c);
        break;
      }
    }

    // The index of the first non-option argument
    int fileIndex = g.getOptind();
    if ((inv_file == null) || (args.length - fileIndex == 0)) {
        System.out.println(usage);
        System.exit(1);
    }

    List pptmaps = new ArrayList();
    for (int i = fileIndex; i < args.length; i++) {
      String filename = args[fileIndex];
      PptMap ppt_map = FileIO.read_serialized_pptmap(new File(filename),
						     true // use saved config
						     );
      pptmaps.add(ppt_map);
    }

    PptMap result = new PptMap();
    union(result, pptmaps);
    // Mark redundant invariants (may have more given additional
    // surrounding program points)

    if (Daikon.suppress_redundant_invariants_with_simplify) {
      System.out.print("Invoking Simplify to identify redundant invariants...");
      System.out.flush();
      long start = System.currentTimeMillis();
      for (Iterator i = result.iterator() ; i.hasNext() ; ) {
	PptTopLevel ppt = (PptTopLevel) i.next();
	ppt.mark_implied_via_simplify();
      }
      long end = System.currentTimeMillis();
      double elapsed = (end - start) / 1000.0;
      System.out.println((new java.text.DecimalFormat("#.#")).format(elapsed) + "s");
    }

    // Write serialized output
    FileIO.write_serialized_pptmap(result, inv_file);
  }

  /**
   * Union multiple PptMaps into one.
   **/
  public static void union(PptMap collector,  // mutated
			   Collection pptmaps // [PptMap], unmodified (but aliased into)
			   )
  {
    for (Iterator i = pptmaps.iterator(); i.hasNext(); ) {
      PptMap ppt_map = (PptMap) i.next();
      for (Iterator ppts = ppt_map.iterator(); ppts.hasNext(); ) {
	PptTopLevel ppt = (PptTopLevel) ppts.next();

	/* [INCR] ...
	if (! ppt.has_samples())
	  continue;
	*/ // ... [INCR]
	if ((ppt.views.size() == 0) && (ppt.implication_view.invs.size() == 0))
	  continue;

	if (collector.get(ppt.ppt_name) != null) {
	  throw new RuntimeException("Cannot merge two non-empty ppts named " + ppt.name);
	}
	System.out.println("Adding ppt " + ppt.name);
	collector.add(ppt);
      }
    }

    // We should check consistency things, such as entry_ppt not
    // pointing outside of the PptMap.  (What else?)
  }

}
