package daikon;

import java.util.*;
import java.io.*;
import gnu.getopt.*;
import org.apache.log4j.Category;
import org.apache.log4j.FileAppender;
import org.apache.log4j.PatternLayout;
import utilMDE.Assert;
import utilMDE.UtilMDE;
import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;
import daikon.inv.filter.*;

public class PrintInvariants {

  /**
   * Main debug tracer for PrintInvariants (for things unrelated to printing).
   **/
  public static final Category debug = Category.getInstance("daikon.PrintInvariants");

  /**
   * Debug tracer for printing.
   **/
  public static final Category debugRepr = Category.getInstance("daikon.PrintInvariants.repr");

  /**
   * Debug tracer for printing.
   **/
  public static final Category debugPrint = Category.getInstance("daikon.print");

  /**
   * Debug tracer for printing equality.
   **/
  public static final Category debugPrintEquality = Category.getInstance("daikon.print.equality");

  /**
   * Debug tracer for filtering.  When PrintInvariants is invoked it
   * will write a transcript of everything that the filters did to the
   * current directory, into a file called filter_transcript.
   **/
  public static final Category debugFiltering = Category.getInstance("daikon.filtering");

  public static final String daikonFilteringOutputFilename = "filtering_transcript";
  // Set up filter logging to go to a file.
  static {
    if (debugFiltering.isDebugEnabled()) {
      debugFiltering.setAdditivity(false);
      debugFiltering.removeAllAppenders();
      FileAppender fa = null;
      try {
        fa = new FileAppender( new PatternLayout("%m"), daikonFilteringOutputFilename, true);
        fa.setName(daikonFilteringOutputFilename);
      }
      catch (IOException ioe) {
        System.err.println("Warning; unable to open file " + daikonFilteringOutputFilename);
      }
      if (fa != null) {
        debugFiltering.addAppender(fa);
        fa.activateOptions();
      }
    }
  }

  public static final String lineSep = Global.lineSep;

  /**
   * Whether we are doing output for testing.  Used only for IOA output.
   **/
  public static boolean test_output = false;

  // Avoid problems if daikon.Runtime is loaded at analysis (rather than
  // test-run) time.  This might have to change when JTrace is used.
  static { daikon.Runtime.no_dtrace = true; }

  private static String usage =
    UtilMDE.join(new String[] {
      "Usage: java daikon.PrintInvariants [OPTION]... FILE",
      "  -h, --" + Daikon.help_SWITCH,
      "      Display this usage message",
      "  --" + Daikon.suppress_cont_SWITCH,
      "      Suppress display of implied invariants (by controlling ppt).",
      "  --" + Daikon.no_suppress_cont_SWITCH,
      "      Don't suppress display of implied invariants.",
      "  --" + Daikon.suppress_post_SWITCH,
      "      Suppress display of obvious postconditions on prestate.",
      "  --" + Daikon.suppress_redundant_SWITCH,
      "      Suppress display of logically redundant invariants.",
      "  --" + Daikon.esc_output_SWITCH,
      "      Write output in ESC format.",
      "  --" + Daikon.simplify_output_SWITCH,
      "      Write output in Simplify format.",
      "  --" + Daikon.ioa_output_SWITCH,
      "      Write output in IOA format.",
      "  --" + Daikon.java_output_SWITCH,
      "      Write output as java expressions.",
      "  --" + Daikon.jml_output_SWITCH,
      "      Write output in JML format.",
      "  --" + Daikon.output_num_samples_SWITCH,
      "      Output numbers of values and samples for invariants and " +
      "program points; for debugging.",
      "  --" + Daikon.noinvariantguarding_SWITCH,
      "      Disable invariant guarding, which is normally on for JML and ESC output formats."},
      lineSep);

  public static void main(String[] args) throws FileNotFoundException,
  StreamCorruptedException, OptionalDataException, IOException,
  ClassNotFoundException {
    daikon.Logger.setupLogs(daikon.Logger.INFO);

    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.suppress_cont_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.no_suppress_cont_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.suppress_post_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.suppress_redundant_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.esc_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.simplify_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.ioa_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.test_ioa_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.java_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.jml_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.output_num_samples_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.config_option_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(Daikon.noinvariantguarding_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
    };
    Getopt g = new Getopt("daikon.PrintInvariants", args, "h", longopts);
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
        } else if (Daikon.no_suppress_cont_SWITCH.equals(option_name)) {
          Daikon.suppress_implied_controlled_invariants = false;
        } else if (Daikon.suppress_post_SWITCH.equals(option_name)) {
          Daikon.suppress_implied_postcondition_over_prestate_invariants = true;
        } else if (Daikon.suppress_redundant_SWITCH.equals(option_name)) {
          Daikon.suppress_redundant_invariants_with_simplify = true;
        } else if (Daikon.esc_output_SWITCH.equals(option_name)) {
          Daikon.output_style = OutputFormat.ESCJAVA;
        } else if (Daikon.simplify_output_SWITCH.equals(option_name)) {
          Daikon.output_style = OutputFormat.SIMPLIFY;
        } else if (Daikon.java_output_SWITCH.equals(option_name)) {
          Daikon.output_style = OutputFormat.JAVA;
        } else if (Daikon.ioa_output_SWITCH.equals(option_name)) {
          Daikon.output_style = OutputFormat.IOA;
        } else if (Daikon.test_ioa_output_SWITCH.equals(option_name)) {
          Daikon.output_style = OutputFormat.IOATEST;
          test_output = true;
        } else if (Daikon.jml_output_SWITCH.equals(option_name)) {
          Daikon.output_style = OutputFormat.JML;
        } else if (Daikon.output_num_samples_SWITCH.equals(option_name)) {
          Daikon.output_num_samples = true;
        } else if (Daikon.config_option_SWITCH.equals(option_name)) {
          String item = g.getOptarg();
          daikon.config.Configuration.getInstance().apply(item);
          break;
        } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
          Global.debugAll = true;
        } else if (Daikon.debug_SWITCH.equals(option_name)) {
          Logger.setPriority(g.getOptarg(), Logger.DEBUG);
        } else if (Daikon.noinvariantguarding_SWITCH.equals(option_name)) {
          Daikon.noInvariantGuarding = true;
        } else {
          throw new RuntimeException("Unknown long option received: " +
                                     option_name);
        }
        break;
      case 'h':
        System.out.println(usage);
        System.exit(1);
        break;
      case '?':
        break; // getopt() already printed an error
      default:
        System.out.println("getopt() returned " + c);
        break;
      }
    }
    // The index of the first non-option argument -- the name of the file
    int fileIndex = g.getOptind();
    if (args.length - fileIndex != 1) {
        System.out.println(usage);
        System.exit(1);
    }
    String filename = args[fileIndex];
    PptMap ppts = FileIO.read_serialized_pptmap(new File(filename),
                                               true // use saved config
                                               );

    // Make sure ppts' rep invariants hold
    ppts.repCheck();
    ppt_map = ppts;

    if ((Daikon.output_style == OutputFormat.ESCJAVA ||
         Daikon.output_style == OutputFormat.JML) &&
        !Daikon.noInvariantGuarding)
      Daikon.guardInvariants(ppts);

    print_invariants(ppts);
  }

  // To avoid the leading "UtilMDE." on all calls.
  private static String nplural(int n, String noun) {
    return UtilMDE.nplural(n, noun);
  }

  ///////////////////////////////////////////////////////////////////////////
  //

  public static void print_invariants(PptMap ppts) {
    // Retrieve Ppt objects in sorted order.
    PrintWriter pw = new PrintWriter(System.out, true);
    // PptMap iterator uses a custom comparator for a specific ordering
    for (Iterator itor = ppts.pptIterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      // if (ppt.has_samples() &&  // [[INCR]]
      if (! Daikon.no_text_output) {
        print_invariants_maybe(ppt, pw);
      }
    }
    pw.flush();
  }

  /**
   * Print invariants for a single program point and its conditionals.
   * Does no output if no samples or no views.
   **/
  public static void print_invariants_maybe(PptTopLevel ppt,
                                            PrintWriter out)
  {
    // Be silent if we never saw any samples.
    // (Maybe this test isn't even necessary, but will be subsumed by others,
    // as all the invariants will be unjustified.)
    if (ppt.num_samples() == 0) {
      if (Daikon.output_num_samples) {
        out.println("[No samples for " + ppt.name + "]");
      }
      return;
    }
    if ((ppt.numViews() == 0) && (ppt.joiner_view.invs.size() == 0)) {
      if (! (ppt instanceof PptConditional)) {
        // Presumably all the views that were originally there were deleted
        // because no invariants remained in any of them.
        if (Daikon.output_num_samples) {
          out.println("[No views for " + ppt.name + "]");
        }
      }
      return;
    }

    /* [INCR]
    if ((ppt.combined_exit != null) && (Daikon.output_style != Daikon.OUTPUT_STYLE_NORMAL)) {
      if (Daikon.output_num_samples) {
        out.println("[Is combined exit, output style " + Daikon.output_style + ": " + ppt.name + "]");
      }
      return;
    }
    */

    if (Daikon.output_style != OutputFormat.IOA) {
      out.println("===========================================================================");
    } else {
      out.println();
      out.println("% Invariants generated by Daikon for");
    }
    print_invariants(ppt, out);


    if (Daikon.dkconfig_output_conditionals
        && Daikon.output_style == OutputFormat.DAIKON)
    {
      for (int i=0; i<ppt.views_cond.size(); i++) {
        PptConditional pcond = (PptConditional) ppt.views_cond.elementAt(i);
        print_invariants_maybe(pcond, out);
      }
    }
  }

  public static String get_better_name(PptTopLevel ppt) {
    String better_name = ppt.name;
    {
      // Replace <init> with name of class
      int init_pos = better_name.indexOf(".<init>");
      if (init_pos != -1) {
        String before = better_name.substring(0, init_pos);
        String after = better_name.substring(init_pos+7);
        String classname = before;
        int lastdot = before.lastIndexOf('.'); // Not corrent for inners, but oh well
        if (lastdot >= 0) {
          classname = before.substring(lastdot+1);
        }
        better_name = before + "." + classname + after;
      }
    }
    int open_paren_pos = better_name.indexOf("(");
    if ((open_paren_pos != -1)
        // if open paren is first character, don't try to be clever
        && (open_paren_pos != 0)) {
      int close_paren_pos = better_name.indexOf(")");
      int colon_pos = better_name.indexOf(":::");
      String args = better_name.substring(open_paren_pos, close_paren_pos+1);
      String java_args = UtilMDE.arglistFromJvm(args);
      better_name = better_name.substring(0, open_paren_pos)
        + ((java_args != null) ? java_args : args)
        + better_name.substring(colon_pos);
    }

    return(better_name);
  }

  public static void print_sample_data(PptTopLevel ppt, PrintWriter out)
  {
    // System.out.println("entering print_sample_data\n");
    String better_name = get_better_name(ppt);

    if (Daikon.output_num_samples) {
      int num_samps = -111; // [[INCR]]
      out.println(better_name + "  " + nplural(num_samps, "sample"));
      // out.println("    Samples breakdown: " + tuplemod_samples_summary()); // [[INCR]]
    } else {
      if (Daikon.output_style == OutputFormat.IOA) {
        out.print("% ");  // IOA comment style
      }
      out.println(better_name);
    }
    if (Daikon.output_num_samples
        || (Daikon.output_style == OutputFormat.ESCJAVA)
        || (Daikon.output_style == OutputFormat.JML)) {
      out.print("    Variables:");
      for (int i=0; i<ppt.var_infos.length; i++) {
        out.print(" " + ppt.var_infos[i].name.name());
      }
      out.println();
    }
    // System.out.println("entering print_sample_data\n");
  }

  public static void print_modified_vars(PptTopLevel ppt, PrintWriter out)
  {
    Vector modified_vars = new Vector();
    Vector modified_primitive_args = new Vector();
    Vector unmodified_vars = new Vector();
    Vector unmodified_orig_vars = new Vector();

    for (int i=0; i<ppt.var_infos.length; i++) {
      VarInfo vi = ppt.var_infos[i];
      // This test is purely an optimization.
      if (! vi.isPrestate()) {
        VarInfo vi_orig = ppt.findVar(vi.name.applyPrestate());
        if (vi_orig != null) {
          // Assert.assertTrue(vi_orig.postState.name == vi.name, "vi_orig="+vi_orig.name+", vi_orig.postState="+vi_orig.postState+((vi_orig.postState!=null)?"="+vi_orig.postState.name:"")+", vi="+vi+"="+vi.name);
          // Assert.assertTrue(vi_orig.postState == vi, "vi_orig="+vi_orig.name+", vi_orig.postState="+vi_orig.postState+((vi_orig.postState!=null)?"="+vi_orig.postState.name:"")+", vi="+vi+"="+vi.name);
          boolean is_unmodified = false; // vi.equal_to == vi_orig.equal_to // [INCR] XXX
          if (! is_unmodified) {
            java.lang.reflect.Field f = vi.name.resolveField(ppt);
            // System.out.println("Field for " + vi.name.name() + ": " + f);
            if ((f != null)
                && java.lang.reflect.Modifier.isFinal(f.getModifiers())) {
              // System.out.println("Final: " + vi.name.name());
              is_unmodified = true;
            }
          }
          // System.out.println(vi.name.name() + (is_unmodified ? " unmodified" : " modified"));
          if (is_unmodified) {
            unmodified_vars.add(vi);
            unmodified_orig_vars.add(vi_orig);
          } else {
            // out.println("Modified: " + vi.name + " (=" + vi.equal_to.name + "), " + vi_orig.name + " (=" + vi_orig.equal_to.name + ")");
            PptSlice1 view = ppt.findSlice(vi);
            // out.println("View " + view + " num_values=" + ((view!=null)?view.num_values():0));
            // The test "((view != null) && (view.num_values() > 0))" is
            // fallacious becuase the view might have been removed (is now
            // null) because all invariants at it were false.
            if ((view == null) ||
                view.containsOnlyGuardingPredicates()) {
              // Further modified because a view might have otherwise been
              // destroyed if it were not for the guarding invariants put
              // into the PptSlice. This if view != null, and it only contains
              // guarding predicates, it would have been null had invariant
              // guarding been off, thus the variable belongs in modified_vars.
              // [INCR] || (view.num_values() > 0)) {
              // Using only the isPrimitive test is wrong.  We should suppress
              // for only parameters, not all primitive values.  That's why we
              // look for the period in the name.
              if (vi.type.isPrimitive() && (vi.name.name().indexOf(".") == -1)) {
                modified_primitive_args.add(vi);
              } else {
                modified_vars.add(vi);
              }
            }
          }
        }
      }
    }
    if (Daikon.output_num_samples || (Daikon.output_style == OutputFormat.ESCJAVA)) {
      if (modified_vars.size() > 0) {
        out.print("      Modified variables:");
        for (int i=0; i<modified_vars.size(); i++) {
          VarInfo vi = (VarInfo)modified_vars.elementAt(i);
          out.print(" " + vi.name.name());
        }
        out.println();
      }
      if (modified_primitive_args.size() > 0) {
        out.print("      Modified primitive arguments:");
        for (int i=0; i<modified_primitive_args.size(); i++) {
          VarInfo vi = (VarInfo)modified_primitive_args.elementAt(i);
          out.print(" " + vi.name.name());
        }
        out.println();
      }
      if (unmodified_vars.size() > 0) {
        out.print("      Unmodified variables:");
        for (int i=0; i<unmodified_vars.size(); i++)
          out.print(" " + ((VarInfo)unmodified_vars.elementAt(i)).name.name());
        out.println();
      }
    }
    // It would be nice to collect the list of indices which are modified,
    // and create a \forall to specify that the rest aren't.
    if (Daikon.output_style == OutputFormat.ESCJAVA ||
        Daikon.output_style == OutputFormat.JML) {
      Vector mods = new Vector();
      for (int i=0; i<modified_vars.size(); i++) {
        VarInfo vi = (VarInfo)modified_vars.elementAt(i);
        // System.out.println("modified var: " + vi.name.name());
        while (vi != null) {
          Derivation derived = vi.derived;
          VarInfoName vin = vi.name;
          if (vin instanceof VarInfoName.TypeOf) {
            // "VAR.class"
            vi = null;
          } else if (vin instanceof VarInfoName.SizeOf) {
            // "size(VAR)"
            vi = null;
          } else if ((vin instanceof VarInfoName.Field)
                     && ((VarInfoName.Field)vin).term.name().endsWith("]")) {
            // "VAR[..].field" => VAR[..];
            vi = ppt.findVar(((VarInfoName.Field)vin).term.name());
            Assert.assertTrue(vi != null);
          } else if (derived instanceof SequenceScalarSubscript) {
            vi = ((SequenceScalarSubscript)vi.derived).seqvar();
          } else if (derived instanceof SequenceFloatSubscript) {
            vi = ((SequenceFloatSubscript)vi.derived).seqvar();
          } else if (derived instanceof SequenceStringSubscript) {
            vi = ((SequenceStringSubscript)vi.derived).seqvar();
          } else if (derived instanceof SequenceScalarSubsequence) {
            vi = ((SequenceScalarSubsequence)vi.derived).seqvar();
          } else if (derived instanceof SequenceFloatSubsequence) {
            vi = ((SequenceFloatSubsequence)vi.derived).seqvar();
          } else if (derived instanceof SequenceStringSubsequence) {
            vi = ((SequenceStringSubsequence)vi.derived).seqvar();
            Assert.assertTrue(vi != null);
          } else {
            break;
          }
        }
        // Change this.myvector[*] to this.myvector (or would it be
        // best to just remove it?)
        if ((vi != null) && (vi.name instanceof VarInfoName.Elements)) {
          VarInfoName.Elements elems = (VarInfoName.Elements) vi.name;
          VarInfo base = ppt.findVar(elems.term);
          Assert.assertTrue(base != null);
          if (! base.type.isArray()) {
            vi = base;
          }
        }
        // System.out.println("really modified var: " + ((vi == null) ? "null" : vi.name.name()));
        if ((vi != null) && (! mods.contains(vi))) {
          mods.add(vi);
        }
      }
      if (mods.size() > 0) {
        if (Daikon.output_style == OutputFormat.ESCJAVA)
          out.print("modifies ");
        else
          out.print("assignable ");
        for (int i=0; i<mods.size(); i++) {
          if (i>0) {
            out.print(", ");
          }
          VarInfo vi = (VarInfo)mods.elementAt(i);
          String name = vi.name.name();
          if (name.endsWith("[]")) {
            name = name.substring(0, name.length()-1) + "*]";
          }
          out.print(name);
        }
        out.println();
      }
    }

  }

  // Count statistics (via Global) on variables (canonical, missing, etc.)
  public static void count_global_stats(PptTopLevel ppt)
  {
    for (int i=0; i<ppt.var_infos.length; i++) {
      /* [INCR]
      if (! ppt.var_infos[i].isCanonical()) {
        Global.non_canonical_variables++;
      } else if (ppt.var_infos[i].canBeMissingCheck()) {
        Global.can_be_missing_variables++;
      } else {
        Global.canonical_variables++;
      }
      */
      if (ppt.var_infos[i].isDerived()) {
        Global.derived_variables++;
      }
    }
  }

  /* [INCR]
  / **
   * Get all varInfos that are equal to vi.  The result also includes
   * vi, because it may be that vi shouldn't get printed.  Obviously
   * equal vars are included based on the value of
   * PrintInvariants.includeObviouslyEqual.
   ** /
  public static Vector get_equal_vars(VarInfo vi) {
    return(get_equal_vars(vi, includeObviouslyEqual));
  }

  / **
   * Get all varInfos that are equal to vi.  The result also includes
   * vi, because it may be that vi shouldn't get printed.  Obviously
   * equal vars are included based on the value of
   * the parameter includeObviouslyEqual.
   ** /
  public static Vector get_equal_vars(VarInfo vi, boolean includeObviouslyEqual)
  {
    Vector equal_vars = new Vector();
    equal_vars.add(vi);

    if (debugPrintEquality.isDebugEnabled()) {
      debugPrintEquality.debug("Testing equality for " + vi.name.name());
    }


    if (includeObviouslyEqual) {
      equal_vars.addAll(vi.equalTo());
    } else {
      equal_vars.addAll(vi.equalToNonobvious());
    }

    // // Filter for parameters here
    // if (vi.ppt.ppt_name.isExitPoint()) {
    //  for (Iterator i = equal_vars.iterator(); i.hasNext(); ) {
    //   VarInfo var = (VarInfo) i.next();
    //   if (debugPrintEquality.isDebugEnabled()) {
    //    debugPrintEquality.debug (" testing derivedParamAndUnint " + var.name.name());
    //   }
    //   if (var.isDerivedParamAndUninteresting()) {
    //    i.remove();
    //   }
    //  }
    // }
    return(equal_vars);
  }
  */ // ... [INCR]

  public static Vector get_obviously_equal(VarInfo vi)
  {
    Vector obviously_equal = null;

    /* [INCR]
    if (includeObviouslyEqual)
      {
        obviously_equal = new Vector(vi.equalTo());
        obviously_equal.removeAll(vi.equalToNonobvious());

       // System.out.println("equal_vars.size() = " + equal_vars.size());
       // System.out.println("Redundant due to simplify = "
       //                    + (Daikon.suppress_redundant_invariants_with_simplify
       //                       && redundant_invs.contains(vi)));
       }
       else
       {
       obviously_equal = new Vector();
       }
    */ // ... [INCR]
    obviously_equal = new Vector(); // [INCR]

    return(obviously_equal);
  }

  // ppt should only be used for obtaining the number of values and
  // samples, but not for any other purpose.
  public static void print_equality_invariants(VarInfo vi, PrintWriter out, int invCounter, PptTopLevel ppt)
  {
    if (debugPrintEquality.isDebugEnabled()) {
      debugPrintEquality.debug ("Attempting to print equality for: " + vi.name.name());
    }

    // switch commented lines to include obviously equal in output
    Vector equal_vars = new Vector(); // get_equal_vars(); // [INCR] XXX
    Vector obviously_equal = get_obviously_equal(vi);

    if (debugPrintEquality.isDebugEnabled()) {
      StringBuffer sb = new StringBuffer();
        debugPrintEquality.debug("Resultant obviously equality set for "  + vi.ppt.name + " " + vi.name.name());
      for (Iterator j = equal_vars.iterator(); j.hasNext(); ) {
        sb.append ("  " + ((VarInfo) j.next()).name.name());
      }
      debugPrintEquality.debug (sb);
    }

    // Necessary for [INCR].
    if (equal_vars.size() <= 1)
      return;

    if (Daikon.output_style == OutputFormat.DAIKON) {
      StringBuffer sb = new StringBuffer();
      if (equal_vars.size() > 1) {
        for (int j=0; j<equal_vars.size(); j++) {
          VarInfo other = (VarInfo) equal_vars.elementAt(j);
          if (j != 0) sb.append(" == "); // "interned"
          sb.append(other.name.name());
          if (obviously_equal.contains(other)) {
            sb.append(" (obviously)");
          }
        }
      }
      PptTopLevel ppt_tl = (PptTopLevel) vi.ppt;
      PptSlice slice1 = ppt_tl.findSlice(vi);
      if (Daikon.output_num_samples) {
        if (slice1 != null) {
          sb.append("\t\t(" +
                    // [INCR] nplural(slice1.num_values(), "value") + ", " +
                    nplural(slice1.num_samples(), "sample") + ")");
        } else {
          // sb.append("\t\t(no slice)");
        }
      }
      out.println(sb.toString());
    } else if (Daikon.output_style == OutputFormat.ESCJAVA) {
      equal_vars.add(0, vi);    // [???]
      // Separate out those variables which are valid in ESC.
      List valid_equiv = new ArrayList(); // [VarInfo]
      List invalid_equiv = new ArrayList(); // [VarInfo]
      for (int j=0; j<equal_vars.size(); j++) {
        VarInfo other = (VarInfo) equal_vars.elementAt(j);
        if (other.isDerivedSequenceMinMaxSum()) {
          break;
        }
        if (other.isValidEscExpression()) {
          valid_equiv.add(other);
        } else {
          invalid_equiv.add(other);
        }
      }
      // Choose a leader, preferring the valid variables.
      VarInfo leader;
      if (valid_equiv.size() > 0) {
        leader = (VarInfo) valid_equiv.get(0);
      } else {
        Assert.assertTrue(invalid_equiv.size() > 0);
        leader = (VarInfo) invalid_equiv.get(0);
      }
      // Print the equality statements, stating expressible ones first.
      equal_vars.clear();
      equal_vars.addAll(valid_equiv);
      equal_vars.addAll(invalid_equiv);
      for (int j=0; j<equal_vars.size(); j++) {
        VarInfo other = (VarInfo) equal_vars.get(j);
        if (other == leader) continue;
        if (j >= valid_equiv.size()) {
          out.print("warning: method 'equality'.format_esc() needs to be implemented: ");
        }
        if (leader.rep_type.isArray()) {
          String[] form =
            VarInfoName.QuantHelper.format_esc(new VarInfoName[]
              { leader.name, other.name }, true); // elementwise
          out.println(form[0] + "( " + form[1] + " == " + form[2] + " )" + form[3]);
        } else {
          out.println(leader.name.esc_name() + " == " + other.name.esc_name());
        }
        if (obviously_equal.contains(other)) {
          out.println("    (obviously)");
        }
      }
    } else if (Daikon.output_style == OutputFormat.SIMPLIFY) {
      for (int j=1; j<equal_vars.size(); j++) {
        VarInfo me  = (VarInfo) equal_vars.elementAt(0);
        VarInfo other = (VarInfo) equal_vars.elementAt(j);
        if (other.isDerivedSequenceMinMaxSum())
          break;
        if (me.rep_type.isArray()) {
          String[] form =
            VarInfoName.QuantHelper.format_simplify(new VarInfoName[]
              { me.name, other.name }, true); // elementwise
          out.println(form[0] + "(EQ " + form[1] + " " + form[2] + " )" + form[3]);
        } else {
          out.println("(EQ " + me.name.simplify_name() +
                      " " + other.name.simplify_name() + ")");
        }
      }
    } else if (Daikon.output_style == OutputFormat.IOA) {
      StringBuffer sb = new StringBuffer();

      String invName = get_ioa_invname (invCounter, ppt);
      if (debugPrint.isDebugEnabled()) {
        debugPrint.debug("Printing equality for " + invName);
      }
      sb.append("invariant " + invName + " of " + ppt.ppt_name.getFullClassName() + ": ");
      sb.append (get_ioa_precondition (invCounter, ppt));

      StringBuffer sb2 = new StringBuffer();
      for (int j = 1; j < equal_vars.size(); j++) {
        VarInfo one = (VarInfo) equal_vars.get(j-1);
        VarInfo two = (VarInfo) equal_vars.get(j);
        if (j > 1) sb2.append (" /\\ ");
        sb2.append("(" + one.name.ioa_name() + " = " + two.name.ioa_name() + ")");
      }

      String rawOutput = sb2.toString();
      int startPos = rawOutput.indexOf("anIndex");
      if (startPos != -1) {
        int endPos = rawOutput.indexOf ("]", startPos);
        String qvar = rawOutput.substring (startPos, endPos);
        rawOutput = "\\A " + qvar + " (" + rawOutput + ")";
      }

      sb.append(rawOutput);
      out.println(sb.toString());
    } else if (Daikon.output_style == OutputFormat.JAVA) {
      for (int j = 1; j < equal_vars.size(); j++) {
        VarInfo me = (VarInfo) equal_vars.elementAt(0);
        VarInfo other = (VarInfo) equal_vars.elementAt(j);
        if (other.isDerivedSequenceMinMaxSum())
          break;
        if (me.rep_type.isArray()) {
          String[] form =
            VarInfoName.QuantHelper.format_java(new VarInfoName[]
              { me.name, other.name }, true); // elementwise
          out.println(form[0] + "( " + form[1] + " == " + form[2] + " )" + form[3]);
        } else {
          out.println(me.name.java_name() + " == " + other.name.java_name());
        }
        if (obviously_equal.contains(other)) {
          out.println("    (obviously)");
        }
      }
    } else {
      throw new IllegalStateException("Unknown output mode");
    }
  }

  /* [INCR]
  public static boolean accept_varinfo(PptTopLevel ppt, VarInfo vi)
  {
    // this needs to be a seperate if statement because if vi is not
    // canonical, it will fail assert statements in some of the things
    // which are invoked in the return clause.
    if (vi.isCanonical()) {
      return ((get_equal_vars(vi).size() > 1)  &&
              (! (Daikon.suppress_redundant_invariants_with_simplify &&
                  ppt.redundant_invs.contains(vi))));
    } else {
      return false;
    }
  }
  */ // ... [INCR]

  // This is just a temporary thing to provide more info about the
  // reason invariants are rejected.
  private static String reason = "";

  // note - this rejects equality invariants out of hand
  /**
   * Determines whether an invariant should be printed.
   * @return true if the invariant should be printed.
   **/
  public static boolean accept_invariant(Invariant inv)
  {
    throw new Error ("This method has been deprecated.  Use filters");

    /*
    reason = "";

    if (IsEqualityComparison.it.accept(inv)) {
      reason = "is an equality invariant";
      return(false);
    }

    if (!inv.isWorthPrinting())
      {
        reason = "isn't worth printing";
        return(false);
      }

    // Suppressed, not interesting to print
    if (Daikon.suppress_invariants && inv.getSuppressor() != null) return false;

    if (Daikon.suppress_redundant_invariants_with_simplify &&
        inv.ppt.parent.redundant_invs.contains(inv))
      {
        reason = "is redundant";
        daikon.simplify.SessionManager.debugln("Redundant: " + inv.format());
        return(false);
      }

    if (Daikon.output_style != OutputFormat.DAIKON ||
        Daikon.output_style != OutputFormat.IOA) {
      // don't print out invariants with min(), max(), or sum() variables
      boolean mms = false;
      VarInfo[] varbls = inv.ppt.var_infos;
      for (int v=0; !mms && v<varbls.length; v++) {
        mms |= varbls[v].isDerivedSequenceMinMaxSum();
      }
      if (mms) {
        reason = "has min/max/sum";
        return(false);
      }
    }

    if (inv.ppt.ppt_name.isExitPoint()) {
      for (int i = 0; i < inv.ppt.var_infos.length; i++) {
        VarInfo vi = inv.ppt.var_infos[i];
        // ppt has to be a PptSlice, not a PptTopLevel
        if (vi.isDerivedParamAndUninteresting()) {
          reason = "is derived parameter & uninteresting";
          return false;
        }
      }
    }

    reason = "";
    return(true);
    */
  }

  // ppt should only be used for obtaining the number of values and
  // samples, but not for any other purpose.
  public static void print_invariant(Invariant inv, PrintWriter out, int invCounter, PptTopLevel ppt)
  {
    // [INCR] int num_vals = inv.ppt.num_values();
    int inv_num_samps = inv.ppt.num_samples();
    String num_values_samples = "\t\t(" +
      // [INCR] nplural(num_vals, "value") + ", " +
      nplural(inv_num_samps, "sample") + ")";

    String inv_rep;
    // All this should turn into simply a call to format_using.
    if (Daikon.output_style == OutputFormat.DAIKON) {
      inv_rep = inv.format_using(Daikon.output_style);
    } else if (Daikon.output_style == OutputFormat.ESCJAVA) {
      if (inv.isValidEscExpression()) {
        inv_rep = inv.format_using(Daikon.output_style);
      } else {
        if (inv instanceof Equality) {
          inv_rep = "warning: method 'equality'.format_esc() needs to be implemented: " + inv.format();
        } else {
          inv_rep = "warning: method " + inv.getClass().getName() + ".format_esc() needs to be implemented: " + inv.format();
        }
      }
    } else if (Daikon.output_style == OutputFormat.JML) {
      // System.out.println("className: " + inv.getClass().getName());
      // System.out.println("daikon rep: " + inv.format_using(OutputFormat.DAIKON));
      inv_rep = inv.format_using(Daikon.output_style);
      // System.out.println("Representation: " + inv_rep);
    } else if (Daikon.output_style == OutputFormat.SIMPLIFY) {
      inv_rep = inv.format_using(Daikon.output_style);
    } else if (Daikon.output_style == OutputFormat.IOA) {
      String invName = get_ioa_invname (invCounter, ppt);
      if (debugPrint.isDebugEnabled()) {
        debugPrint.debug("Printing normal for " + invName + " with inv " +
                          inv.getClass().getName());
      }

      inv_rep = "invariant " + invName + " of " + ppt.ppt_name.getFullClassName() + ": ";

      inv_rep += get_ioa_precondition (invCounter, ppt);
      // We look for indexed variables and add fake quantifiers to
      // the left.  Should we be doing this with visitors and the
      // quantification engine?  Maybe, but then again, Daikon
      // doesn't really know what it means to sample.
      String rawOutput = inv.format_using(Daikon.output_style);
      int startPos = rawOutput.indexOf("anIndex");
      if (startPos != -1) {
        int endPos = rawOutput.indexOf ("]", startPos);
        String qvar = rawOutput.substring (startPos, endPos);
        rawOutput = "\\A " + qvar + " (" + rawOutput + ")";
      }
      inv_rep += rawOutput;
      if (PptTopLevel.debug.isDebugEnabled()) {
        PptTopLevel.debug.debug(inv.repr());
      }
    } else if (Daikon.output_style == OutputFormat.JAVA) {
      inv_rep = inv.format_using(Daikon.output_style);
    } else {
      throw new IllegalStateException("Unknown output mode");
    }
    if (Daikon.output_num_samples) {
      inv_rep += num_values_samples;
    }

    if (debugRepr.isDebugEnabled()) {
      debugRepr.debug("Printing: [" + inv.repr_prob() + "]");
    } else if (debugPrint.isDebugEnabled()) {
      debugPrint.debug("Printing: [" + inv.repr_prob() + "]");
    }

    // debug
    // out.println(inv.getClass().getName());
    //      out.println("---------------------------");
    //      VarInfo invVarInfos[];
    //      if (inv instanceof Implication) {
    //        VarInfo leftVarInfos[] = ((Implication)inv).left.ppt.var_infos;
    //        VarInfo rightVarInfos[] = ((Implication)inv).right.ppt.var_infos;
    //        int leftVarInfosLength = (leftVarInfos == null ? 0 : leftVarInfos.length);
    //        int rightVarInfosLength = (rightVarInfos == null ? 0 : rightVarInfos.length);

    //        invVarInfos = new VarInfo [leftVarInfosLength + rightVarInfosLength];
    //        for (int i=0; i<leftVarInfosLength; i++) {
    //          invVarInfos[i] = leftVarInfos[i];
    //        }
    //        for (int i=0; i<rightVarInfosLength; i++) {
    //          invVarInfos[i+leftVarInfosLength] = rightVarInfos[i];
    //        }
    //      } else {
    //        invVarInfos = inv.ppt.var_infos;
    //      }

    //      for (int i=0; i<invVarInfos.length; i++) {
    //        out.println(invVarInfos[i].toString());
    //      }

    out.println(inv_rep);

    // this is not guaranteed to work or even compile if uncommented.
//    {
//      // Print out any subexpressions of this which are
//      // non-canonical (and thus could be replaced with a canonical
//      // form).  There are never any of these, though, since we
//      // don't derive from non-canonical variables.
//      Iterator iter = Arrays.asList(inv.ppt.var_infos).iterator();
//      while (iter.hasNext()) {
//        VarInfoName name = ((VarInfo) iter.next()).name;
//        Iterator nodes = (new VarInfoName.InorderFlattener(name)).nodes().iterator();
//        nodes.next(); // skip the root
//        while (nodes.hasNext()) {
//          VarInfoName node = (VarInfoName) nodes.next();
//          VarInfo info = findVar(node);
//          if ((info != null) && !info.isCanonical()) {
//            out.println("** sub node not canonical: " + node);
//          }
//        }
//      }
//    }
  }

  /** Takes a list of Invariants and returns a list of Invariants which
   *  is sorted according to PptTopLevel.icfp.
   */

  public static List sort_invariant_list(List invs) {
    Invariant[] invs_array = (Invariant[]) invs.toArray(new Invariant[invs.size()]);
    Arrays.sort(invs_array, PptTopLevel.icfp);

    Vector result = new Vector(invs_array.length);

    for (int i = 0; i < invs_array.length; i++) {
      result.add(invs_array[i]);
    }
    return(result);
  }

  private static PptMap ppt_map = null;

  public static boolean includeObviouslyEqual = false;


  /***********************************************************/
  /** Print invariants for a single program point. */
  public static void print_invariants(PptTopLevel ppt, PrintWriter out) {
    // System.out.println("print_invariants(" + ppt.name + ")");

    // make names easier to read before printing
    ppt.simplify_variable_names();

    print_sample_data(ppt, out);
    print_modified_vars(ppt, out);

    // Dump some debugging info, if enabled
    if (debugPrint.isDebugEnabled()) {
      debugPrint.debug("Variables for ppt "  + ppt.name);
      for (int i=0; i<ppt.var_infos.length; i++) {
        VarInfo vi = ppt.var_infos[i];
        PptTopLevel ppt_tl = (PptTopLevel) vi.ppt;
        PptSlice slice1 = ppt_tl.findSlice(vi);
        debugPrint.debug("      " + vi.name.name()
                         // + " constant=" + vi.isConstant() // [INCR]
                         // + " canonical=" + vi.isCanonical() // [INCR]
                         // + " equal_to=" + vi.equal_to.name.name() // [INCR]
                         );
      }
    }
    if (debugFiltering.isDebugEnabled()) {
      debugFiltering.debug("------------------------------------------------------------------------------------------------\n");
      debugFiltering.debug(get_better_name(ppt) + "\n\n");
    }

    // Count statistics (via Global) on variables (canonical, missing, etc.)
    count_global_stats(ppt);

    int invCounter = 0; // Count printed invariants for this program point

    // I could instead sort the PptSlice objects, then sort the invariants
    // in each PptSlice.  That would be more efficient, but this is
    // probably not a bottleneck anyway.
    List invs_vector = ppt.getInvariants();
    // System.out.println("Total invs for this ppt: " + invs_vector.size());

    Invariant[] invs_array = (Invariant[]) invs_vector.toArray(new Invariant[invs_vector.size()]);
    Arrays.sort(invs_array, PptTopLevel.icfp);

    Global.non_falsified_invariants += invs_array.length;

    List accepted_invariants = new Vector();

    for (int i = 0; i < invs_array.length; i++) {
      Invariant inv = invs_array[i];
      InvariantFilters fi = new InvariantFilters();
      fi.ppt_map = ppt_map;

      // Deprecated, so I'm commenting this out.  Use new filters system instead
      // boolean pi_accepted = accept_invariant(inv);
      boolean fi_accepted = fi.shouldKeep(inv);

      // Never print the guarding predicates themselves, they should only
      // print as part of GuardingImplications
      if (fi_accepted && !inv.isGuardingPredicate) {
        invCounter++;
        Global.reported_invariants++;
        accepted_invariants.add(inv);
      }
    }

    accepted_invariants = InvariantFilters.addEqualityInvariants(accepted_invariants);

    if (debugFiltering.isDebugEnabled()) {
      Iterator inv_iter = accepted_invariants.iterator();
      while (inv_iter.hasNext()) {
        Invariant current_inv = (Invariant)inv_iter.next();
        if (current_inv instanceof Equality) {
          debugFiltering.debug("Found Equality that says " + current_inv.format() + "\n");
        }
      }
    }

    if (debugFiltering.isDebugEnabled()) {
      for (int i=0; i<ppt.var_infos.length; i++) {
        VarInfo vi = ppt.var_infos[i];

        // if (accept_varinfo(ppt, vi)) { // [INCR] XXX
          /* [INCR]
          StringBuffer sb = new StringBuffer();
          for (Iterator j = vi.equalTo().iterator(); j.hasNext(); ) {
            sb.append (" ==  " + ((VarInfo) j.next()).name.name());
          }
          StringWriter eq_invs = new StringWriter();
          PrintWriter pw = new PrintWriter(eq_invs);
          print_equality_invariants(vi, pw, invCounter, ppt);
          debugFiltering.debug("Found VarInfo that says " + eq_invs.toString());
          */ // [INCR]
        // } [INCR]
      }
    }

    finally_print_the_invariants(accepted_invariants, out, ppt);
  }

  /**
   * Does the actual printing of the invariants.
   **/
  private static void finally_print_the_invariants(List invariants, PrintWriter out, PptTopLevel ppt)
  {
    int index = 0;
    Iterator inv_iter = invariants.iterator();
    while (inv_iter.hasNext()) {
      index++;
      Invariant inv = (Invariant)inv_iter.next();

      // I could imagine printing information about the PptSlice
      // if it has changed since the last Invariant I examined.
      //
      // This code may fail in some cases because slice might be
      // null if the invariant is of type Equality.  if you want
      // this to work, you'll want to modify it to handle that case
      // first.
      // PptSlice slice = inv.ppt;
      // if (debugPrint.isDebugEnabled()) {
      //  debugPrint.debug("Slice: " + slice.varNames() + "  "
      //                   + slice.num_samples() + " samples");
      //  debugPrint.debug("    Samples breakdown: "
      //                   + slice.tuplemod_samples_summary());
      //  slice.values_cache.dump();
      // }
      // Assert.assertTrue(slice.check_modbits());

      print_invariant(inv, out, index, ppt);

      // System.out.println("\t\t" + inv.getClass().getName());
    }
  }

  /**
   * Get name of invariant for IOA output, since IOA invariants have
   * to be given unique names.  The name can be derived from a count
   * of the invariants and the program point name.  We simply change
   * the ppt name's characters to be valid IOA syntax.
   **/
  public static String get_ioa_invname (int numbering, Ppt ppt) {
    String replaced = "";
    if (PrintInvariants.test_output) {
      if (ppt.ppt_name.getFullMethodName() != null) {
        replaced = ppt.ppt_name.getFullMethodName().replace('(', '_').replace(')', '_');
      }
      return "Inv" + replaced;
    } else {
      if (ppt.ppt_name.getFullMethodName() != null) {
        replaced = ppt.ppt_name.getFullMethodName().replace('(', '_').replace(')', '_');
      }
      return "Inv" + replaced + numbering;
    }
  }

  public static String get_ioa_precondition (int numbering, Ppt ppt) {
    if (ppt.ppt_name.isClassStaticSynthetic()) return "";
    return "enabled(" + ppt.ppt_name.getFullMethodName() + ") => ";
  }
}
