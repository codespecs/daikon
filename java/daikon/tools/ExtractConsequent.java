package daikon.tools;

import java.util.*;
import java.io.*;
import java.lang.Math;
import org.apache.log4j.Category;
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

  public static final Category debug = Category.getInstance ("daikon.ExtractConsequent");
  public static final String lineSep = Global.lineSep;
  private static Perl5Matcher re_matcher = new Perl5Matcher();
  private static Perl5Compiler re_compiler = new Perl5Compiler();

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
    daikon.Logger.setupLogs(daikon.Logger.INFO);
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
                                                true //use saved config
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

    PrintWriter pw = new PrintWriter(System.out, true);
    for (Iterator itor = ppts_sorted.iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      extract_consequent_maybe(ppt, pw, ppts);
    }

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
        Set conditions = (Set) cluster_to_conditions.get(predicate);
        StringBuffer conjunction = new StringBuffer();
        for (Iterator condsIter = conditions.iterator(); condsIter.hasNext(); ) {
          String cond = (String)condsIter.next();
          allConds.add(cond);
          conjunction.append(" && ");
          conjunction.append(cond);
        }
        conjunction.delete(0, 4); // remove leading " && "

        String conj = conjunction.toString();
        // Avoid inserting self-contradictory conditions such as "x == 1 &&
        // x == 2";
        if (re_matcher.contains(conj, contradict_inv_pattern)
            || re_matcher.contains(conj, useless_inv_pattern_1)
            || re_matcher.contains(conj, useless_inv_pattern_2)) {
          // System.out.println("Suppressing: " + conj);
        } else {
          allConds.add(conj);
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

  /**
   * Extract consequents from a implications at a single program
   * point. It only searches for top level Program points because
   * Implications are produced only at those points.
   **/
  public static void extract_consequent_maybe(PptTopLevel ppt,
                                              PrintWriter out,
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

        //don't print redundant invariants.
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
        if (consequent.usesVar("cluster"))
          cons_uses_cluster = true;
        if (predicate.usesVar("cluster"))
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

        //filter out unwanted invariants

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
        inv_string = simplify_inequalities(inv_string);
        store_invariant(cluster_inv.format_using(OutputFormat.DAIKON), inv_string, pptname);
      }
    }
  }

  //Store the invariant for later printing. Ignore duplicate
  //invariants at the same program point.
  private static void store_invariant (String predicate, String consequent, String pptname) {
    if (!pptname_to_conditions.containsKey(pptname)) {
      pptname_to_conditions.put(pptname, new HashMap());
    }

    HashMap cluster_to_conditions = (HashMap) pptname_to_conditions.get(pptname);
    if (!cluster_to_conditions.containsKey(predicate)) {
      cluster_to_conditions.put(predicate, new HashSet());
    }

    HashSet conditions = (HashSet)cluster_to_conditions.get(predicate);
    if (! conditions.contains(consequent)) {
      conditions.add(consequent);
    }
  }


  private static boolean contains_constant_non_012 (Invariant inv) {
    if (inv instanceof daikon.inv.unary.scalar.OneOfScalar) {
      daikon.inv.unary.scalar.OneOfScalar oneof = (daikon.inv.unary.scalar.OneOfScalar) inv;
      //OneOf invariants that indicate a small set ( > 1 element) of
      //possible values are not interesting, and have already been
      //eliminated by the isInteresting check
      long num = ((Long) oneof.elt()).longValue();
      if (num > 2 || num < -1)
        return true;
    }

    return false;
  }

  //remove non-word characters and everything after ":::" from the
  //program point name, leaving PackageName.ClassName.MethodName
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
