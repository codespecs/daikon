package daikon;

import java.util.*;
import java.io.*;
import gnu.getopt.*;
import org.apache.log4j.Category;
import utilMDE.Assert;
import utilMDE.UtilMDE;
import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;

public class PrintInvariants {

  /**
   * Main debug tracer.
   **/
  public static final Category debug = Category.getInstance ("daikon.PrintInvariants");

  public static final String lineSep = Global.lineSep;

  private static String usage =
    UtilMDE.join(new String[] {
      "Usage: java daikon.PrintInvariants [OPTION]... FILE",
      "  -h, --" + Daikon.help_SWITCH,
      "      Display this usage message",
      "  --" + Daikon.suppress_cont_SWITCH,
      "      Suppress display of implied invariants (by controlling ppt).",
      "  --" + Daikon.suppress_post_SWITCH,
      "      Suppress display of obvious postconditions on prestate.",
      "  --" + Daikon.suppress_redundant_SWITCH,
      "      Suppress display of logically redundant invariants.",
      "  --" + Daikon.esc_output_SWITCH,
      "      Write output in ESC-like format.",
      "  --" + Daikon.simplify_output_SWITCH,
      "      Write output in Simplify format.",
      "  --" + Daikon.ioa_output_SWITCH,
      "      Write output in IOA format. (bug exists)",
      "  --" + Daikon.java_output_SWITCH,
      "      Write output as java expressions.",
      "  --" + Daikon.output_num_samples_SWITCH,
      "      Output numbers of values and samples for invariants and " +
      "program points; for debugging."}, lineSep);

  public static void main(String[] args) throws FileNotFoundException,
  StreamCorruptedException, OptionalDataException, IOException,
  ClassNotFoundException {
    daikon.Logger.setupLogs(daikon.Logger.INFO);
    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.suppress_cont_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.suppress_post_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.suppress_redundant_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.esc_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.simplify_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.ioa_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.java_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.output_num_samples_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.config_option_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
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
    // PptMap iteratorator uses a custom comparator for a specific ordering
    for (Iterator itor = ppts.iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      // if (ppt.has_samples() &&  // [[INCR]]
      if (! Daikon.no_text_output) {
        print_invariants_maybe(ppt, pw);
      }
    }
    pw.flush();
  }

  // In original (Python) implementation, known as print_invariants_ppt.
  // I may still want to integrate some more of its logic here.
  /**
   * Print invariants for a single program point and its conditionals.
   * Does no output if no samples or no views.
   **/
  public static void print_invariants_maybe(PptTopLevel ppt, PrintWriter out)
  {
    // Be slient if we never saw any samples
    if (ppt.num_samples() == 0) {
      if (Daikon.output_num_samples) {
	out.println("[No samples for " + ppt.name + "]");
      }
      return;
    }

    // Be slient if we are a conditional ppt with no invariants
    if ((ppt instanceof PptConditional) && (ppt.invariants_vector().size() == 0))
      return;

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


  /** Print invariants for a single program point. */
  public static void print_invariants(PptTopLevel ppt, PrintWriter out) {
    // System.out.println("print_invariants(" + ppt.name + ")");

    // make names easier to read before printing
    ppt.simplify_variable_names();

    // System.out.println("This = " + this + ", Name(2) = " + name + " = " + ppt_name);
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
    if (Daikon.output_num_samples || (Daikon.output_style == OutputFormat.ESCJAVA)) {
      out.print("    Variables:");
      for (int i=0; i<ppt.var_infos.length; i++)
        out.print(" " + ppt.var_infos[i].name);
      out.println();
    }
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
          // Assert.assert(vi_orig.postState.name == vi.name, "vi_orig="+vi_orig.name+", vi_orig.postState="+vi_orig.postState+((vi_orig.postState!=null)?"="+vi_orig.postState.name:"")+", vi="+vi+"="+vi.name);
          // Assert.assert(vi_orig.postState == vi, "vi_orig="+vi_orig.name+", vi_orig.postState="+vi_orig.postState+((vi_orig.postState!=null)?"="+vi_orig.postState.name:"")+", vi="+vi+"="+vi.name);
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
            PptSlice1 view = ppt.getView(vi);
            // out.println("View " + view + " num_values=" + ((view!=null)?view.num_values():0));
            // The test "((view != null) && (view.num_values() > 0))" is
            // fallacious becuase the view might have been removed (is now
            // null) because all invariants at it were false.
	    if (view == null) { // [INCR] || (view.num_values() > 0)) {
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
          out.print(" " + vi.name);
        }
        out.println();
      }
      if (modified_primitive_args.size() > 0) {
        out.print("      Modified primitive arguments:");
        for (int i=0; i<modified_primitive_args.size(); i++) {
          VarInfo vi = (VarInfo)modified_primitive_args.elementAt(i);
          out.print(" " + vi.name);
        }
        out.println();
      }
      if (unmodified_vars.size() > 0) {
        out.print("      Unmodified variables:");
        for (int i=0; i<unmodified_vars.size(); i++)
          out.print(" " + ((VarInfo)unmodified_vars.elementAt(i)).name);
        out.println();
      }
    }
    // It would be nice to collect the list of indices which are modified,
    // and create a \forall to specify that the rest aren't.
    if (Daikon.output_style == OutputFormat.ESCJAVA) {
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
            Assert.assert(vi != null);
          } else if (derived instanceof SequenceScalarSubscript) {
            vi = ((SequenceScalarSubscript)vi.derived).seqvar();
          } else if (derived instanceof SequenceStringSubscript) {
            vi = ((SequenceStringSubscript)vi.derived).seqvar();
          } else if (derived instanceof SequenceScalarSubsequence) {
            vi = ((SequenceScalarSubsequence)vi.derived).seqvar();
          } else if (derived instanceof SequenceStringSubsequence) {
            vi = ((SequenceStringSubsequence)vi.derived).seqvar();
            Assert.assert(vi != null);
          } else {
            break;
          }
        }
        // System.out.println("really modified var: " + ((vi == null) ? "null" : vi.name.name()));
        if ((vi != null) && (! mods.contains(vi))) {
          mods.add(vi);
        }
      }
      if (mods.size() > 0) {
        out.print("modifies ");
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

    // Assert.assert(check_modbits()); // [INCR]

    // Dump some debugging info, if enabled
    if (Invariant.debugPrint.isDebugEnabled()) {
      // out.println("Views:");
      // for (Iterator itor = views.iterator(); itor.hasNext(); ) {
      //   PptSlice slice = (PptSlice) itor.next();
      //   out.println("  " + slice.name);
      //   for (int i=0; i<slice.invs.size(); i++) {
      //     Invariant inv = (Invariant) slice.invs.elementAt(i);
      //     out.println("    " + inv.repr());
      //   }
      // }

      Invariant.debugPrint.debug("    Variables:");
      for (int i=0; i<ppt.var_infos.length; i++) {
        VarInfo vi = ppt.var_infos[i];
        PptTopLevel ppt_tl = (PptTopLevel) vi.ppt;
        PptSlice slice1 = ppt_tl.findSlice(vi);
        out.print("      " + vi.name
		  + " constant=" + vi.isStaticConstant()
		  // + " canonical=" + vi.isCanonical() // [INCR]
		  // + " equal_to=" + vi.equal_to.name // [INCR]
		  );
        // if (slice1 == null) {
        //   out.println(", no slice");
        // } else {
        //   out.println(" slice=" + slice1
        //               + "=" + slice1.name
        //               + " num_values=" + slice1.num_values()
        //               + " num_samples=" + slice1.num_samples());
        //   // slice1.values_cache.dump();
        // }
      }
    }

    // Count statistics on variables (canonical, missing, etc.)
    for (int i=0; i<ppt.var_infos.length; i++) {
      /* [INCR]
      if (! var_infos[i].isCanonical()) {
        Global.non_canonical_variables++;
      } else if (var_infos[i].canBeMissingCheck()) {
	Global.can_be_missing_variables++;
      } else {
        Global.canonical_variables++;
      }
      */
      if (ppt.var_infos[i].isDerived()) {
        Global.derived_variables++;
      }
    }

    int invCounter = 0; // Counter printed invariants for this program point

    // First, print the equality invariants.  They don't show up in the
    // below because one of the two variables is non-canonical!
    // This technique is a bit non-orthogonal, but probably fine.
    // We might do no output if all the other variables are vacuous.
    // We should have already equal_to for each VarInfo.

    for (int i=0; i<ppt.var_infos.length; i++) {
      VarInfo vi = ppt.var_infos[i];
      // System.out.println("Considering equality for "
      //                    + (vi.isCanonical() ? "" : "non")
      //                    + "canonical var " + vi.name.name());
      // if (vi.isCanonical()) // [INCR] XXX
      {

	// switch commented lines to include obviously equal in output
	Vector equal_vars = new Vector(); // vi.equalToNonobvious(); // [INCR] XXX
	Vector obviously_equal = new Vector();
	// Vector equal_vars = vi.equalTo();
	// Vector obviously_equal = new Vector(equal_vars);
	// obviously_equal.removeAll(vi.equalToNonobvious());

        // System.out.println("equal_vars.size() = " + equal_vars.size());
        // System.out.println("Redundant due to simplify = "
        //                    + (Daikon.suppress_redundant_invariants_with_simplify
        //                       && redundant_invs.contains(vi)));


	if (debug.isDebugEnabled()) {
	  debug.debug("Trying to print equality for " + ppt.ppt_name + " + " + equal_vars);
	}


        if (equal_vars.size() > 0 &&
	    // suppress if the equality invariant is implied via simplify
	    (! (Daikon.suppress_redundant_invariants_with_simplify &&
		ppt.redundant_invs.contains(vi)))) {



	  invCounter++;
	  if (Daikon.output_style == OutputFormat.DAIKON) {
            StringBuffer sb = new StringBuffer(vi.name.name());
            for (int j=0; j<equal_vars.size(); j++) {
              VarInfo other = (VarInfo) equal_vars.elementAt(j);
              sb.append(" == "); // "interned"
              sb.append(other.name);
	      if (obviously_equal.contains(other)) {
		sb.append(" (obviously)");
	      }
            }
            PptTopLevel ppt_tl = (PptTopLevel) vi.ppt;
            PptSlice slice1 = ppt_tl.findSlice(vi);
            if (Daikon.output_num_samples) {
              if (slice1 != null) {
                sb.append("\t\t(" +
                          // [INCR] nplural(slice1.num_values(), "
                          nplural(slice1.num_samples(), "sample") + ")");
              } else {
                // sb.append("\t\t(no slice)");
              }
            }
            out.println(sb.toString());
	  } else if (Daikon.output_style == OutputFormat.ESCJAVA) {
            for (int j=0; j<equal_vars.size(); j++) {
              VarInfo other = (VarInfo) equal_vars.elementAt(j);
	      if (other.isDerivedSequenceMinMaxSum())
		break;
	      if (vi.rep_type.isArray()) {
		String[] form =
		  VarInfoName.QuantHelper.format_esc(new VarInfoName[]
		    { vi.name, other.name }, true); // elementwise
                out.println(form[0] + "( " + form[1] + " == " + form[2] + " )" + form[3]);
              } else {
		out.println(vi.name.esc_name() + " == " + other.name.esc_name());
	      }
	      if (obviously_equal.contains(other)) {
		out.println("    (obviously)");
	      }
            }
          } else if (Daikon.output_style == OutputFormat.SIMPLIFY) {
            for (int j=0; j<equal_vars.size(); j++) {
              VarInfo other = (VarInfo) equal_vars.elementAt(j);
	      if (other.isDerivedSequenceMinMaxSum())
		break;
              if (vi.rep_type.isArray()) {
		String[] form =
		  VarInfoName.QuantHelper.format_simplify(new VarInfoName[]
		    { vi.name, other.name }, true); // elementwise
                out.println(form[0] + "(EQ " + form[1] + " " + form[2] + " )" + form[3]);
              } else {
		out.println("(EQ " + vi.name.simplify_name() +
			    " " + other.name.simplify_name() + ")");
	      }
            }
	  } else if (Daikon.output_style == OutputFormat.IOA) {
	    StringBuffer sb = new StringBuffer();
	    String invName = get_ioa_invname (invCounter, ppt);
	    debug.debug("Printing equality for " + invName);
	    sb.append("invariant " + invName + " of " + ppt.ppt_name.getFullClassName() + ": ");
	    sb.append (get_ioa_precondition (invCounter, ppt));
	    sb.append ("(" + vi.name.ioa_name() + " = " +
		       ((VarInfo) equal_vars.get(0)).name.ioa_name() + ")");
            for (int j = 1; j < equal_vars.size(); j++) {
              VarInfo one = (VarInfo) equal_vars.get(j-1);
              VarInfo two = (VarInfo) equal_vars.get(j);
	      sb.append (" /\\ ");
	      sb.append("(" + one.name.ioa_name() + " = " + two.name.ioa_name() + ")");
	    }
	    out.println(sb.toString());
	  } else if (Daikon.output_style == OutputFormat.JAVA) {
	    for (int j = 0; j < equal_vars.size(); j++) {
	      VarInfo other = (VarInfo) equal_vars.elementAt(j);
	      if (other.isDerivedSequenceMinMaxSum())
		break;
	      if (vi.rep_type.isArray()) {
		String[] form =
		  VarInfoName.QuantHelper.format_java(new VarInfoName[]
		    { vi.name, other.name }, true); // elementwise
		out.println(form[0] + "( " + form[1] + " == " + form[2] + " )" + form[3]);
	      } else {
		out.println(vi.name.java_name() + " == " + other.name.java_name());
	      }
	      if (obviously_equal.contains(other)) {
		out.println("    (obviously)");
	      }
	    }
	  } else {
	    throw new IllegalStateException("Unknown output mode");
          }
        }
      }
    }

    // I could instead sort the PptSlice objects, then sort the invariants
    // in each PptSlice.  That would be more efficient, but this is
    // probably not a bottleneck anyway.
    Vector invs_vector = ppt.invariants_vector();
    Invariant[] invs_array = (Invariant[]) invs_vector.toArray(new Invariant[invs_vector.size()]);
    Arrays.sort(invs_array, PptTopLevel.icfp);

    Global.non_falsified_invariants += invs_array.length;
    for (int ia_index = 0; ia_index<invs_array.length; ia_index++) {
      Invariant inv = invs_array[ia_index];
      // [INCR] int num_vals = inv.ppt.num_values();
      int inv_num_samps = inv.ppt.num_samples();
      String num_values_samples = "\t\t(" +
	// [INCR] nplural(num_vals, "value") + ", " +
        nplural(inv_num_samps, "sample") + ")";

      // I could imagine printing information about the PptSlice
      // if it has changed since the last Invariant I examined.
      PptSlice slice = inv.ppt;
      if (Invariant.debugPrint.isDebugEnabled()) {
        Invariant.debugPrint.debug("Slice: " + slice.varNames() + "  "
				   + slice.num_samples() + " samples");
        Invariant.debugPrint.debug("    Samples breakdown: "
				   + slice.tuplemod_samples_summary());
        // slice.values_cache.dump();
      }
      Assert.assert(slice.check_modbits());

      // isWorthPrinting checks many conditions for suppression
      if (! inv.isWorthPrinting()) {
	// out.println("Not worth printing: " + inv.format() + ", " + inv.repr());
	continue;
      }

      // Redundancy is separate from worth printing for now, but it
      // probably should not be, in general.
      if (Daikon.suppress_redundant_invariants_with_simplify &&
	  ((PptTopLevel) inv.ppt.parent).redundant_invs.contains(inv)) {
	daikon.simplify.SessionManager.debugln("Redundant: " + inv.format());
	continue;
      }

      if (Daikon.output_style != OutputFormat.DAIKON ||
	  Daikon.output_style != OutputFormat.IOA) {
	// don't print out invariants with min(), max(), or sum() variables
	boolean mms = false;
	VarInfo[] varbls = inv.ppt.var_infos;
	for (int v=0; !mms && v<varbls.length; v++) {
	  mms |= varbls[v].isDerivedSequenceMinMaxSum();
	}
	if (mms) { continue; }
      }

      String inv_rep;
      invCounter++;
      if ((Daikon.output_style == OutputFormat.JAVA)
	  || (Daikon.output_style == OutputFormat.DAIKON)
	  || (Daikon.output_style == OutputFormat.ESCJAVA)
	  || (Daikon.output_style == OutputFormat.SIMPLIFY))
      {
	inv_rep = inv.format_using(Daikon.output_style);
      } else if (Daikon.output_style == OutputFormat.IOA) {
	String invName = get_ioa_invname (invCounter, ppt);
	if (debug.isDebugEnabled()) {
	  debug.debug ("Printing normal for " + invName + " with inv " + inv.getClass().getName());
	}
	inv_rep = "invariant " + invName + " of " + ppt.ppt_name.getFullClassName() + ": ";
	inv_rep += get_ioa_precondition (invCounter, ppt);
	inv_rep += inv.format_using(OutputFormat.IOA);
	if (PptTopLevel.debug.isDebugEnabled()) {
	 PptTopLevel.debug.debug (inv.repr());
	}
      } else {
	throw new IllegalStateException("Unknown output mode");
      }
      if (Daikon.output_num_samples) {
        inv_rep += num_values_samples;
      }
      out.println(inv_rep);
      if (Invariant.debugPrint.isDebugEnabled()) {
        Invariant.debugPrint.debug("  [" + inv.repr_prob() + "]");
      }
      Global.reported_invariants++;

//    {
//  	// Print out any subexpressions of this which are
//  	// non-canonical (and thus could be replaced with a canonical
//  	// form).  There are never any of these, though, since we
//  	// don't derive from non-canonical variables.
//  	Iterator iter = Arrays.asList(inv.ppt.var_infos).iterator();
//  	while (iter.hasNext()) {
//  	  VarInfoName name = ((VarInfo) iter.next()).name;
//  	  Iterator nodes = (new VarInfoName.InorderFlattener(name)).nodes().iterator();
//  	  nodes.next(); // skip the root
//  	  while (nodes.hasNext()) {
//  	    VarInfoName node = (VarInfoName) nodes.next();
//  	    VarInfo info = findVar(node);
//  	    if ((info != null) && !info.isCanonical()) {
//  	      out.println("** sub node not canonical: " + node);
//  	    }
//  	  }
//  	}
//    }
    }
  }


  public static String get_ioa_invname (int numbering, Ppt ppt) {
    if (ppt.ppt_name.isClassStaticSynthetic()) return "Inv_" + numbering;
    String replaced = ppt.ppt_name.getFullMethodName().replace('(', '_').replace(')', '_');
    return "Inv_" + replaced + numbering;
  }

  public static String get_ioa_precondition (int numbering, Ppt ppt) {
    if (ppt.ppt_name.isClassStaticSynthetic()) return "";
    return "enabled(" + ppt.ppt_name.getFullMethodName() + ") => ";
  }

}
