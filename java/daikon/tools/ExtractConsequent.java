package daikon.tools;

import java.util.*;
import java.io.*;
import java.lang.Math;
import org.apache.log4j.Logger;
import gnu.getopt.*;
import utilMDE.UtilMDE;
import org.apache.oro.text.regex.*;
import daikon.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;

/**
 * Extract the consequents of all Implication invariants that are predicated
 * by membership in a cluster, from a .inv file.  An example of such an
 * implication would be "(cluster == <NUM>) ==> consequent". The consequent
 * is only true in certain clusters, but is not generally true for all
 * executions of the program point to which the Implication belongs.  These
 * resulting implications are written to standard output in the format of a
 * splitter info file.
 **/
public class ExtractConsequent {

  public static final Logger debug = Logger.getLogger ("daikon.ExtractConsequent");
  public static final String lineSep = Global.lineSep;
  private static Perl5Matcher re_matcher = new Perl5Matcher();
  private static Perl5Compiler re_compiler = new Perl5Compiler();

  private static class HashedConsequent {
    Invariant inv;

    // We prefer "x < y", "x > y", and "x == y" to the conditions
    // "x >= y", "x <= y", and "x != y" that (respectively) give the
    // same split.  When we see a dispreferred form, we index it by
    // the preferred form, and if there's already an entry (from the
    // real preferred one) we throw the new one out. Otherwise, we
    // insert both the dispreferred form and an entry for the
    // preferred form, with a pointer pack to the dispreferred
    // form. If we later see the preferred form, we replace the
    // placeholder and remove the dispreferred form.
    String fakeFor;

    HashedConsequent(Invariant i, String ff) {
      inv = i;
      fakeFor = ff;
    }
  };

  /* A HashMap whose keys are PPT names (Strings) and whose values are
      HashMaps whose keys are predicate names (Strings) and whose values are
       HashMaps whose keys are Strings (normalized java-format invariants)
         and whose values are HashedConsequent objects. */
  private static HashMap pptname_to_conditions = new HashMap();

  private static String usage =
    UtilMDE.join(new String[] {
      "Usage: java daikon.ExtractConsequent [OPTION]... FILE",
      "  -h, --" + Daikon.help_SWITCH,
      "      Display this usage message",
      "  --" + Daikon.suppress_cont_SWITCH,
      "      Suppress display of implied invariants (by controlling ppt).",
      "  --" + Daikon.suppress_post_SWITCH,
      "      Suppress display of obvious postconditions on prestate.",
      "  --" + Daikon.suppress_redundant_SWITCH,
      "      Suppress display of logically redundant invariants."}, lineSep);

  public static void main(String[] args)
    throws FileNotFoundException, IOException, ClassNotFoundException
  {
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);
    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.suppress_cont_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.suppress_post_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.suppress_redundant_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.config_option_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
    };
    Getopt g = new Getopt("daikon.ExtractConsequent", args, "h", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch (c) {
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
        } else if (Daikon.config_option_SWITCH.equals(option_name)) {
          String item = g.getOptarg();
          daikon.config.Configuration.getInstance().apply(item);
          break;
        } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
          Global.debugAll = true;
        } else if (Daikon.debug_SWITCH.equals(option_name)) {
          LogHelper.setLevel(g.getOptarg(), LogHelper.DEBUG);
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
    ppt_map = ppts;
    extract_consequent(ppts);
  }

  private static PptMap ppt_map = null;

  public static void extract_consequent(PptMap ppts) {
    // Retrieve Ppt objects in sorted order.
    // Use a custom comparator for a specific ordering
    Comparator comparator = new Ppt.NameComparator();
    TreeSet ppts_sorted = new TreeSet(comparator);
    ppts_sorted.addAll(ppts.asCollection());

    for (Iterator itor = ppts_sorted.iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      extract_consequent_maybe(ppt, ppts);
    }

    PrintWriter pw = new PrintWriter(System.out, true);

    // All conditions at a program point.  A TreeSet to enable
    // deterministic output.
    TreeSet allConds = new TreeSet();
    for ( Iterator pptNamesIter = (pptname_to_conditions.keySet()).iterator() ;
          pptNamesIter.hasNext() ; ) {
      String pptname = (String) pptNamesIter.next();
      HashMap cluster_to_conditions = (HashMap) pptname_to_conditions.get(pptname);
      for ( Iterator predIter = (cluster_to_conditions.keySet()).iterator() ;
            predIter.hasNext() ; ) {
        String predicate = (String) predIter.next();
        Map conditions = (Map) cluster_to_conditions.get(predicate);
        StringBuffer conjunctionJava = new StringBuffer();
        StringBuffer conjunctionDaikon = new StringBuffer();
        StringBuffer conjunctionIOA = new StringBuffer();
        StringBuffer conjunctionESC = new StringBuffer();
        StringBuffer conjunctionSimplify = new StringBuffer("(AND ");
        int count = 0;
        for (Iterator condsIter = conditions.keySet().iterator();
             condsIter.hasNext(); count++) {
          String condIndex = (String)condsIter.next();
          HashedConsequent cond = (HashedConsequent)conditions.get(condIndex);
          if (cond.fakeFor != null) {
            count--;
            continue;
          }
          String javaStr = cond.inv.format_using(OutputFormat.JAVA);
          String daikonStr = cond.inv.format_using(OutputFormat.DAIKON);
          String ioaStr = cond.inv.format_using(OutputFormat.IOA);
          String escStr = cond.inv.format_using(OutputFormat.ESCJAVA);
          String simplifyStr = cond.inv.format_using(OutputFormat.SIMPLIFY);
          allConds.add(combineDummy(condIndex, "<dummy> " + daikonStr,
                                    ioaStr, escStr, simplifyStr));
//           allConds.add(condIndex);
          if (count > 0) {
            conjunctionJava.append(" && ");
            conjunctionDaikon.append(" and ");
            conjunctionIOA.append(" /\\ ");
            conjunctionESC.append(" && ");
            conjunctionSimplify.append(" ");
          }
          conjunctionJava.append(javaStr);
          conjunctionDaikon.append(daikonStr);
          conjunctionIOA.append(ioaStr);
          conjunctionESC.append(escStr);
          conjunctionSimplify.append(simplifyStr);
        }
        conjunctionSimplify.append(")");
        String conj = conjunctionJava.toString();
        // Avoid inserting self-contradictory conditions such as "x == 1 &&
        // x == 2", or conjunctions of only a single condition.
        if (count < 2 || re_matcher.contains(conj, contradict_inv_pattern)
            || re_matcher.contains(conj, useless_inv_pattern_1)
            || re_matcher.contains(conj, useless_inv_pattern_2)) {
          // System.out.println("Suppressing: " + conj);
        } else {
          allConds.add(combineDummy(conjunctionJava.toString(),
                                    conjunctionDaikon.toString(),
                                    conjunctionIOA.toString(),
                                    conjunctionESC.toString(),
                                    conjunctionSimplify.toString()));
        }
      }

      if (allConds.size() > 0) {
        pw.println("\nPPT_NAME " + pptname);
        Iterator condsIter = allConds.iterator();
        while (condsIter.hasNext())
          pw.println(condsIter.next());
      }
      allConds.clear();
    }

    pw.flush();
  }

  static String combineDummy(String inv, String daikon, String ioa, String esc,
                             String simplify) {
    StringBuffer combined = new StringBuffer(inv);
    combined.append("\n\tDAIKON_FORMAT ");
    combined.append(daikon);
    combined.append("\n\tIOA_FORMAT ");
    combined.append(ioa);
    combined.append("\n\tESC_FORMAT ");
    combined.append(esc);
    combined.append("\n\tSIMPLIFY_FORMAT ");
    combined.append(simplify);
    return combined.toString();
  }


  /**
   * Extract consequents from a implications at a single program
   * point. It only searches for top level Program points because
   * Implications are produced only at those points.
   **/
  public static void extract_consequent_maybe(PptTopLevel ppt,
                                              PptMap all_ppts) {
    /* [INCR]
    if (! ppt.has_samples()) {
      return;
    }

    if ((ppt.numViews() == 0) && (ppt.implication_view.invs.size() == 0)) {
      return;
    }

    // if ((ppt.combined_exit != null)) {
    // return;
    // }
    */ // [INCR]

    ppt.simplify_variable_names();

    // [INCR] Invariants invs = ppt.implication_view.invs;
    Invariants invs = new Invariants();
    if (invs.size() > 0) {
      String pptname = cleanup_pptname(ppt.name);
      Iterator itor = invs.iterator();
      while (itor.hasNext()) {
        Implication maybe = (Implication)itor.next();

        // don't print redundant invariants.
        if (Daikon.suppress_redundant_invariants_with_simplify &&
            maybe.ppt.parent.redundant_invs.contains(maybe)) {
          continue;
        }

        // don't print out invariants with min(), max(), or sum() variables
        boolean mms = false;
        VarInfo[] varbls = maybe.ppt.var_infos;
        for (int v=0; !mms && v<varbls.length; v++) {
          mms |= varbls[v].isDerivedSequenceMinMaxSum();
        }
        if (mms) {
          continue;
        }

        if (maybe.ppt.ppt_name.isExitPoint()) {
          for (int i = 0; i < maybe.ppt.var_infos.length; i++) {
            VarInfo vi = maybe.ppt.var_infos[i];
            if (vi.isDerivedParam()) {
              continue;
            }
          }
        }

        Invariant consequent = maybe.consequent();
        Invariant predicate = maybe.predicate();
        Invariant inv, cluster_inv;
        boolean cons_uses_cluster = false, pred_uses_cluster = false;
        // extract the consequent (predicate) if the predicate
        // (consequent) uses the variable "cluster".  Ignore if they
        // both depend on "cluster"
        if (consequent.usesVarDerived("cluster"))
          cons_uses_cluster = true;
        if (predicate.usesVarDerived("cluster"))
          pred_uses_cluster = true;

        if (!(pred_uses_cluster ^ cons_uses_cluster))
          continue;
        else if (pred_uses_cluster) {
          inv = consequent;
          cluster_inv = predicate;
        } else {
          inv = predicate;
          cluster_inv = consequent;
        }

        if (!inv.isInteresting()) {
          continue;
        }

        if (!inv.isWorthPrinting()) {
          continue;
        }

        if (contains_constant_non_012(inv)) {
          continue;
        }

        // filter out unwanted invariants

        // 1) Invariants involving sequences
        if (inv instanceof daikon.inv.binary.twoSequence.TwoSequence ||
            inv instanceof daikon.inv.binary.sequenceScalar.SequenceScalar ||
            inv instanceof daikon.inv.binary.sequenceString.SequenceString ||
            inv instanceof daikon.inv.unary.sequence.SingleSequence ||
            inv instanceof daikon.inv.unary.stringsequence.SingleStringSequence ) {
          continue;
        }

        if (inv instanceof daikon.inv.ternary.threeScalar.LinearTernary ||
            inv instanceof daikon.inv.binary.twoScalar.LinearBinary) {
          continue;
        }

        String inv_string = inv.format_using(OutputFormat.DAIKON);
        if (re_matcher.contains(inv_string, orig_pattern) || re_matcher.contains(inv_string, dot_class_pattern)) {
          continue;
        }
        String fake_inv_string = simplify_inequalities(inv_string);
        HashedConsequent real = new HashedConsequent(inv, null);
        if (!fake_inv_string.equals(inv_string)) {
          // For instance, inv_string is "x != y", fake_inv_string is "x == y"
          HashedConsequent fake = new HashedConsequent(inv, inv_string);
          boolean added =
            store_invariant(cluster_inv.format_using(OutputFormat.JAVA), fake_inv_string,
                            fake, pptname);
          if (!added) {
            // We couldn't add "x == y", (when we're "x != y") because
            // it already exists; so don't add "x == y" either.
            continue;
          }
        }
        store_invariant(cluster_inv.format_using(OutputFormat.DAIKON), inv_string, real, pptname);
      }
    }
  }

  // Store the invariant for later printing. Ignore duplicate
  // invariants at the same program point.
  private static boolean store_invariant (String predicate,
                                          String index,
                                          HashedConsequent consequent,
                                          String pptname) {
    if (!pptname_to_conditions.containsKey(pptname)) {
      pptname_to_conditions.put(pptname, new HashMap());
    }

    HashMap cluster_to_conditions = (HashMap) pptname_to_conditions.get(pptname);
    if (!cluster_to_conditions.containsKey(predicate)) {
      cluster_to_conditions.put(predicate, new HashMap());
    }

    HashMap conditions = (HashMap)cluster_to_conditions.get(predicate);
    if (conditions.containsKey(index)) {
      HashedConsequent old = (HashedConsequent)conditions.get(index);
      if (old.fakeFor != null && consequent.fakeFor == null) {
        // We already saw (say) "x != y", but we're "x == y", so replace it.
        conditions.remove(index);
        conditions.remove(old.fakeFor);
        conditions.put(index, consequent);
        return true;
      }
      return false;
    } else {
      conditions.put(index, consequent);
      return true;
    }
  }


  private static boolean contains_constant_non_012 (Invariant inv) {
    if (inv instanceof daikon.inv.unary.scalar.OneOfScalar) {
      daikon.inv.unary.scalar.OneOfScalar oneof = (daikon.inv.unary.scalar.OneOfScalar) inv;
      // OneOf invariants that indicate a small set ( > 1 element) of
      // possible values are not interesting, and have already been
      // eliminated by the isInteresting check
      long num = ((Long) oneof.elt()).longValue();
      if (num > 2 || num < -1)
        return true;
    }

    return false;
  }

  // remove non-word characters and everything after ":::" from the
  // program point name, leaving PackageName.ClassName.MethodName
  private static String cleanup_pptname (String pptname) {
    int index;
    if ((index = pptname.indexOf("(")) > 0) {
      pptname = pptname.substring(0, index);
    }

    if (pptname.endsWith("."))
      pptname = pptname.substring(0, pptname.length()-2);

    return Util.substitute(re_matcher, non_word_pattern, non_word_sub, pptname, Util.SUBSTITUTE_ALL);
  }

  /**
   * Prevents the occurence of "equivalent" inequalities, or inequalities
   * which produce the same pair of splits at a program point, for example
   * "x <= y" and "x > y". Replaces ">=" with "<", "<=" with ">", and "!="
   * with "==" so that the occurence of equivalent inequalities can be
   * detected. However it tries not to be smart ... If there is more than
   * one inequality in the expression, it doesn't perform a substitution.
   **/
  private static String simplify_inequalities (String condition) {
    if (contains_exactly_one(condition, inequality_pattern)) {
      if (re_matcher.contains(condition, gteq_pattern))
        condition = Util.substitute(re_matcher, gteq_pattern, lt_subst, condition, 1);
      else if (re_matcher.contains(condition, lteq_pattern))
        condition = Util.substitute(re_matcher, lteq_pattern, gt_subst, condition, 1);
      else if (re_matcher.contains(condition, neq_pattern))
        condition = Util.substitute(re_matcher, neq_pattern, eq_subst, condition, 1);
    }
    return condition;
  }

  private static boolean contains_exactly_one (String string,
                                               Pattern pattern) {
    PatternMatcherInput input = new PatternMatcherInput(string);
    // return true if first call returns true and second returns false
    return (re_matcher.contains(input, pattern)
            && !re_matcher.contains(input, pattern));
  }

  static Pattern orig_pattern, dot_class_pattern, non_word_pattern;
  static Pattern gteq_pattern, lteq_pattern, neq_pattern, inequality_pattern;
  static Pattern contradict_inv_pattern, useless_inv_pattern_1, useless_inv_pattern_2;
  static StringSubstitution gt_subst = new StringSubstitution(">");
  static StringSubstitution eq_subst = new StringSubstitution("==");
  static StringSubstitution lt_subst = new StringSubstitution("<");
  static StringSubstitution non_word_sub = new StringSubstitution(".");
  static {
    try {
      non_word_pattern = re_compiler.compile("\\W+");
      orig_pattern = re_compiler.compile("orig\\s*\\(");
      dot_class_pattern = re_compiler.compile("\\.class");
      inequality_pattern = re_compiler.compile(  "[\\!<>]=");
      gteq_pattern = re_compiler.compile(">=");
      lteq_pattern = re_compiler.compile("<=");
      neq_pattern = re_compiler.compile("\\!=");
      contradict_inv_pattern
        = re_compiler.compile("(^| && )(.*) == -?[0-9]+ &.*& \\2 == -?[0-9]+($| && )");
      useless_inv_pattern_1
        = re_compiler.compile("(^| && )(.*) > -?[0-9]+ &.*& \\2 > -?[0-9]+($| && )");
      useless_inv_pattern_2
        = re_compiler.compile("(^| && )(.*) < -?[0-9]+ &.*& \\2 < -?[0-9]+($| && )");
    } catch (MalformedPatternException me) {
      throw new Error("ExtractConsequent: Error while compiling pattern");
    }
  }
}
