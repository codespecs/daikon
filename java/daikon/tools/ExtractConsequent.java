package daikon.tools;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.logging.Level.FINE;
import static java.util.logging.Level.INFO;

import daikon.Daikon;
import daikon.FileIO;
import daikon.Global;
import daikon.Ppt;
import daikon.PptMap;
import daikon.PptTopLevel;
import daikon.ProglangType;
import daikon.VarInfo;
import daikon.inv.Implication;
import daikon.inv.Invariant;
import daikon.inv.OutputFormat;
import daikon.inv.binary.twoScalar.IntEqual;
import daikon.inv.binary.twoScalar.IntNonEqual;
import daikon.inv.unary.scalar.OneOfScalar;
import gnu.getopt.*;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.plumelib.util.StringsPlume;

/**
 * Extract the consequents of all Implication invariants that are predicated by membership in a
 * cluster, from a {@code .inv} file. An example of such an implication would be "(cluster ==
 * <em>NUM</em>) &rArr; consequent". The consequent is only true in certain clusters, but is not
 * generally true for all executions of the program point to which the Implication belongs. These
 * resulting implications are written to standard output in the format of a splitter info file.
 */
public class ExtractConsequent {

  /** Do not instantiate. */
  private ExtractConsequent() {
    throw new UnsupportedOperationException("Do not instantiate");
  }

  public static final Logger debug = Logger.getLogger("daikon.ExtractConsequent");
  private static final String lineSep = Global.lineSep;

  private static class HashedConsequent {
    Invariant inv;

    // This field, `fakeFor`, is a simplified/preferred version of the invariant.
    // We prefer "x < y", "x > y", and "x == y" to the conditions
    // "x >= y", "x <= y", and "x != y" that (respectively) give the
    // same split.  When we see a dispreferred form, we index it by
    // the preferred form, and if there's already an entry (from the
    // real preferred one) we throw the new one out. Otherwise, we
    // insert both the dispreferred form and an entry for the
    // preferred form, with a pointer pack to the dispreferred
    // form. If we later see the preferred form, we replace the
    // placeholder and remove the dispreferred form.
    final @Nullable String fakeFor;

    HashedConsequent(Invariant inv, @Nullable String fakeFor) {
      this.inv = inv;
      this.fakeFor = fakeFor;
    }
  }

  /**
   * Maps a program point name to a map whose keys are cluster keys (see {@link #clusterKey}) and
   * whose values are maps whose keys are Strings (normalized Java-format invariants) and whose
   * values are HashedConsequent objects. Each program point has its own clustering, so conditions
   * from different program points are never combined. The maps are sorted, for deterministic
   * output.
   */
  private static Map<String, Map<String, Map<String, HashedConsequent>>> pptname_to_conditions =
      new TreeMap<>();

  /** The usage message for this program. */
  private static String usage =
      StringsPlume.joinLines(
          "Usage: java daikon.ExtractConsequent [OPTION]... FILE",
          "  -h, --" + Daikon.help_SWITCH,
          "      Display this usage message",
          "  --" + Daikon.suppress_redundant_SWITCH,
          "      Suppress display of logically redundant invariants.",
          "  --" + Daikon.debugAll_SWITCH,
          "      Turn on all debug switches",
          "  --" + Daikon.debug_SWITCH + " <logger>",
          "      Turn on the specified debug logger");

  public static void main(String[] args)
      throws FileNotFoundException, IOException, ClassNotFoundException {
    try {
      mainHelper(args);
    } catch (Daikon.DaikonTerminationException e) {
      Daikon.handleDaikonTerminationException(e);
    }
  }

  /**
   * This does the work of {@link #main(String[])}, but it never calls System.exit, so it is
   * appropriate to be called programmatically.
   *
   * @param args command-line arguments, like those of {@link #main}
   * @throws IOException if there is trouble reading the file
   */
  public static void mainHelper(final String[] args) throws IOException {
    daikon.LogHelper.setupLogs(INFO);
    LongOpt[] longopts =
        new LongOpt[] {
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
            throw new Daikon.NormalTermination();
          } else if (Daikon.suppress_redundant_SWITCH.equals(option_name)) {
            Daikon.suppress_redundant_invariants_with_simplify = true;
          } else if (Daikon.config_option_SWITCH.equals(option_name)) {
            String item = Daikon.getOptarg(g);
            daikon.config.Configuration.getInstance().apply(item);
            break;
          } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
            Global.debugAll = true;
          } else if (Daikon.debug_SWITCH.equals(option_name)) {
            daikon.LogHelper.setLevel(Daikon.getOptarg(g), FINE);
          } else {
            throw new RuntimeException("Unknown long option received: " + option_name);
          }
          break;
        case 'h':
          System.out.println(usage);
          throw new Daikon.NormalTermination();
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
      throw new Daikon.UserError("Wrong number of arguments." + Daikon.lineSep + usage);
    }
    String filename = args[fileIndex];
    PptMap ppts =
        FileIO.read_serialized_pptmap(
            new File(filename), true // use saved config
            );
    extract_consequent(ppts);
  }

  public static void extract_consequent(PptMap ppts) {
    // Retrieve Ppt objects in sorted order.
    // Use a custom comparator for a specific ordering
    Comparator<PptTopLevel> comparator = new Ppt.NameComparator();
    TreeSet<PptTopLevel> ppts_sorted = new TreeSet<>(comparator);
    ppts_sorted.addAll(ppts.asCollection());

    for (PptTopLevel ppt : ppts_sorted) {
      extract_consequent_maybe(ppt, ppts);
    }

    PrintWriter pw =
        new PrintWriter(new BufferedWriter(new OutputStreamWriter(System.out, UTF_8)), true);

    // All conditions at a program point.  A TreeSet to enable
    // deterministic output.
    TreeSet<String> allConds = new TreeSet<>();
    for (String pptname : pptname_to_conditions.keySet()) {
      Map<String, Map<String, HashedConsequent>> cluster_to_conditions =
          pptname_to_conditions.get(pptname);
      for (Map.Entry<@KeyFor("cluster_to_conditions") String, Map<String, HashedConsequent>> entry :
          cluster_to_conditions.entrySet()) {
        Map<String, HashedConsequent> conditions = entry.getValue();
        StringBuilder conjunctionJava = new StringBuilder();
        StringBuilder conjunctionDaikon = new StringBuilder();
        StringBuilder conjunctionESC = new StringBuilder();
        StringBuilder conjunctionSimplify = new StringBuilder("(AND ");
        int count = 0;
        for (Map.Entry<@KeyFor("conditions") String, HashedConsequent> entry2 :
            conditions.entrySet()) {
          String condIndex = entry2.getKey();
          HashedConsequent cond = entry2.getValue();
          if (cond.fakeFor != null) {
            continue;
          }
          String javaStr = javaFormat(cond.inv);
          String daikonStr = cond.inv.format_using(OutputFormat.DAIKON);
          String escStr = cond.inv.format_using(OutputFormat.ESCJAVA);
          String simplifyStr = cond.inv.format_using(OutputFormat.SIMPLIFY);
          allConds.add(combineDummy(condIndex, "<dummy> " + daikonStr, escStr, simplifyStr));
          //           allConds.add(condIndex);
          if (count > 0) {
            conjunctionJava.append(" && ");
            conjunctionDaikon.append(" and ");
            conjunctionESC.append(" && ");
            conjunctionSimplify.append(" ");
          }
          count++;
          conjunctionJava.append(parenthesizeIfNeeded(javaStr));
          conjunctionDaikon.append(parenthesizeIfNeeded(daikonStr));
          conjunctionESC.append(parenthesizeIfNeeded(escStr));
          conjunctionSimplify.append(simplifyStr);
        }
        conjunctionSimplify.append(")");
        String conj = conjunctionJava.toString();
        // Avoid inserting self-contradictory conditions such as "x == 1 &&
        // x == 2", or conjunctions of only a single condition.
        if (count < 2
            || contradict_inv_pattern.matcher(conj).find()
            || useless_inv_pattern_1.matcher(conj).find()
            || useless_inv_pattern_2.matcher(conj).find()) {
          // System.out.println("Suppressing: " + conj);
        } else {
          allConds.add(
              combineDummy(
                  conjunctionJava.toString(),
                  conjunctionDaikon.toString(),
                  conjunctionESC.toString(),
                  conjunctionSimplify.toString()));
        }
      }

      if (!allConds.isEmpty()) {
        pw.println();
        pw.println("PPT_NAME " + pptname);
        for (String s : allConds) {
          pw.println(s);
        }
      }
      allConds.clear();
    }

    pw.flush();
  }

  /**
   * Returns the invariant formatted as a splitting condition: in Java format, but with the return
   * value written as "return", which is how a .spinfo file refers to it.
   *
   * @param inv an invariant
   * @return the invariant formatted as a splitting condition
   */
  static String javaFormat(Invariant inv) {
    return result_pattern.matcher(inv.format_using(OutputFormat.JAVA)).replaceAll("return");
  }

  /**
   * Returns the given expression, parenthesized if it may contain an operator whose precedence is
   * lower than that of conjunction, such as "||", " or ", "?:", or the ESC/JML "==&gt;", "&lt;==",
   * "&lt;==&gt;", and "&lt;=!=&gt;". Such an expression must be parenthesized when it is a
   * conjunct. Operators within string and character literals are ignored.
   *
   * @param expr a Java, ESC, or Daikon expression
   * @return the expression, parenthesized if necessary to be used as a conjunct
   */
  static String parenthesizeIfNeeded(String expr) {
    String withoutLiterals = literal_pattern.matcher(expr).replaceAll("\"\"");
    return low_precedence_pattern.matcher(withoutLiterals).find() ? "(" + expr + ")" : expr;
  }

  /**
   * Returns true if the invariant is legal Java when its variables are booleans. Daikon represents
   * a boolean as an int, so it may infer numeric invariants, such as "b != 0" or "b1 < b2", that
   * are not legal Java over booleans.
   *
   * @param inv an invariant
   * @return true if the invariant uses no boolean variable, or is legal Java over booleans
   */
  static boolean isLegalForBooleans(Invariant inv) {
    // OneOfScalar is the only scalar invariant that formats a boolean specially, as "b == true".
    if (inv instanceof OneOfScalar) {
      return true;
    }
    VarInfo[] vis = inv.ppt.var_infos;
    if (inv instanceof IntEqual || inv instanceof IntNonEqual) {
      // "b == i" is not legal Java if exactly one of the operands is a boolean.
      return isBoolean(vis[0]) == isBoolean(vis[1]);
    }
    for (VarInfo vi : vis) {
      if (isBoolean(vi)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Returns true if the variable is a boolean. A splitter declares such a variable as a Java
   * boolean; see {@code SplitterJavaSource.getVarType}.
   *
   * @param vi a variable
   * @return true if the variable is a boolean
   */
  private static boolean isBoolean(VarInfo vi) {
    return vi.type == ProglangType.BOOLEAN;
  }

  static String combineDummy(String inv, String daikonStr, String esc, String simplify) {
    StringBuilder combined = new StringBuilder(inv);
    combined.append(lineSep + "\tDAIKON_FORMAT ");
    combined.append(daikonStr);
    combined.append(lineSep + "\tESC_FORMAT ");
    combined.append(esc);
    combined.append(lineSep + "\tSIMPLIFY_FORMAT ");
    combined.append(simplify);
    return combined.toString();
  }

  /**
   * Extract consequents from all implications at a single program point. It only searches for
   * top-level program points because Implications are produced only at those points.
   */
  public static void extract_consequent_maybe(PptTopLevel ppt, PptMap all_ppts) {
    ppt.simplify_variable_names();

    List<Invariant> invs = new ArrayList<>();
    // Collect Implication invariants at this program point.
    for (Invariant inv : ppt.invariants_vector()) {
      if (inv instanceof Implication) {
        invs.add(inv);
      }
    }
    if (!invs.isEmpty()) {
      // The full name, which SplitterList.matches matches only to this program point.
      String pptname = ppt.name();
      for (Invariant maybe_as_inv : invs) {
        Implication maybe = (Implication) maybe_as_inv;

        // don't print redundant invariants.
        if (Daikon.suppress_redundant_invariants_with_simplify
            && maybe.ppt.parent.redundant_invs.contains(maybe)) {
          continue;
        }

        // don't print out invariants with min(), max(), or sum() variables
        boolean mms = false;
        VarInfo[] varbls = maybe.ppt.var_infos;
        for (int v = 0; !mms && v < varbls.length; v++) {
          mms |= varbls[v].isDerivedSequenceMinMaxSum();
        }
        if (mms) {
          continue;
        }

        if (maybe.ppt.parent.ppt_name.isExitPoint()) {
          for (int i = 0; i < maybe.ppt.var_infos.length; i++) {
            VarInfo vi = maybe.ppt.var_infos[i];
            if (vi.isDerivedParam()) {
              // continue;
            }
          }
        }

        Invariant consequent = maybe.consequent();
        Invariant predicate = maybe.predicate();
        Invariant inv;
        Invariant cluster_inv;
        boolean cons_uses_cluster = false;
        boolean pred_uses_cluster = false;
        // extract the consequent (predicate) if the predicate
        // (consequent) uses the variable "cluster".  Ignore if they
        // both depend on "cluster"
        if (consequent.usesVarDerived("cluster")) {
          cons_uses_cluster = true;
        }
        if (predicate.usesVarDerived("cluster")) {
          pred_uses_cluster = true;
        }

        if (!(pred_uses_cluster ^ cons_uses_cluster)) {
          continue;
        } else if (pred_uses_cluster) {
          inv = consequent;
          cluster_inv = predicate;
        } else {
          inv = predicate;
          cluster_inv = consequent;
        }

        if (!inv.isWorthPrinting()) {
          continue;
        }

        if (contains_constant_non_012(inv)) {
          continue;
        }

        // filter out unwanted invariants

        // 1) Invariants involving sequences
        if (inv instanceof daikon.inv.binary.twoSequence.TwoSequence
            || inv instanceof daikon.inv.binary.sequenceScalar.SequenceScalar
            || inv instanceof daikon.inv.binary.sequenceString.SequenceString
            || inv instanceof daikon.inv.unary.sequence.SingleSequence
            || inv instanceof daikon.inv.unary.stringsequence.SingleStringSequence) {
          continue;
        }

        if (inv instanceof daikon.inv.ternary.threeScalar.LinearTernary
            || inv instanceof daikon.inv.binary.twoScalar.LinearBinary) {
          continue;
        }

        // 2) Numeric invariants over booleans, such as "b != 0", which are not legal Java
        if (!isLegalForBooleans(inv)) {
          debug.fine("Not legal Java over booleans: " + inv.format_using(OutputFormat.JAVA));
          continue;
        }

        String inv_string = javaFormat(inv);
        if (orig_pattern.matcher(inv_string).find()
            || dot_class_pattern.matcher(inv_string).find()) {
          continue;
        }
        String fake_inv_string = simplify_inequalities(inv_string);
        HashedConsequent real = new HashedConsequent(inv, null);
        if (!fake_inv_string.equals(inv_string)) {
          // For instance, inv_string is "x != y", fake_inv_string is "x == y"
          HashedConsequent fake = new HashedConsequent(inv, inv_string);
          boolean added = store_invariant(clusterKey(cluster_inv), fake_inv_string, fake, pptname);
          if (!added) {
            // We couldn't add "x == y", (when we're "x != y") because
            // it already exists; so don't add "x == y" either.
            continue;
          }
        }
        store_invariant(clusterKey(cluster_inv), inv_string, real, pptname);
      }
    }
  }

  /**
   * Returns a key that identifies the cluster that the given invariant describes. The pre-state
   * value "orig(cluster)" and the post-state value "cluster" are equal, so the key for an invariant
   * over one is the same as the key for the same invariant over the other.
   *
   * @param cluster_inv an invariant over the "cluster" variable, such as "cluster == 1"
   * @return a key that identifies the cluster
   */
  static String clusterKey(Invariant cluster_inv) {
    return orig_cluster_pattern
        .matcher(cluster_inv.format_using(OutputFormat.DAIKON))
        .replaceAll("cluster");
  }

  // Store the invariant for later printing. Ignore duplicate
  // invariants at the same program point.
  private static boolean store_invariant(
      String predicate, String index, HashedConsequent consequent, String pptname) {
    if (!pptname_to_conditions.containsKey(pptname)) {
      pptname_to_conditions.put(pptname, new TreeMap<>());
    }

    Map<String, Map<String, HashedConsequent>> cluster_to_conditions =
        pptname_to_conditions.get(pptname);
    if (!cluster_to_conditions.containsKey(predicate)) {
      cluster_to_conditions.put(predicate, new TreeMap<>());
    }

    Map<String, HashedConsequent> conditions = cluster_to_conditions.get(predicate);
    if (conditions.containsKey(index)) {
      HashedConsequent old = conditions.get(index);
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

  private static boolean contains_constant_non_012(Invariant inv) {
    if (inv instanceof OneOfScalar) {
      OneOfScalar oneof = (OneOfScalar) inv;
      // TODO: isInteresting has been removed.  Do we need to deal with it specially here?
      // OneOf invariants that indicate a small set ( > 1 element) of
      // possible values are not interesting, and have already been
      // eliminated by the isInteresting check
      long num = ((Long) oneof.elt()).longValue();
      if (num > 2 || num < -1) {
        return true;
      }
    }

    return false;
  }

  /**
   * Prevents the occurrence of "equivalent" inequalities, or inequalities which produce the same
   * pair of splits at a program point, for example "x &le; y" and "x &gt; y". Replaces "&ge;" with
   * "&lt;", "&le;" with "&gt;", and "!=" with "==" so that the occurrence of equivalent
   * inequalities can be detected. However it tries not to be smart ... If there is more than one
   * inequality in the expression, it doesn't perform a substitution.
   *
   * @param condition a boolean equation
   * @return the condition, with some equalities canonicalized
   */
  private static String simplify_inequalities(String condition) {
    if (contains_exactly_one(condition, inequality_pattern)) {
      if (gteq_pattern.matcher(condition).find()) {
        condition = gteq_pattern.matcher(condition).replaceFirst("<");
      } else if (lteq_pattern.matcher(condition).find()) {
        condition = lteq_pattern.matcher(condition).replaceFirst(">");
      } else if (neq_pattern.matcher(condition).find()) {
        condition = neq_pattern.matcher(condition).replaceFirst("==");
      } else {
        throw new Error("this can't happen");
      }
    }
    return condition;
  }

  private static boolean contains_exactly_one(String string, Pattern pattern) {
    Matcher m = pattern.matcher(string);
    // return true if first call returns true and second returns false
    return m.find() && !m.find();
  }

  /** Matches a pre-state or post-state value, which a splitter cannot use. */
  static Pattern orig_pattern;

  /** Matches the return value in Java format. */
  static Pattern result_pattern;

  /** Matches the pre-state value of the "cluster" variable, in Daikon format. */
  static Pattern orig_cluster_pattern;

  /**
   * Matches an operator whose precedence is lower than that of conjunction. "&lt;==" is needed for
   * reverse implication, and "&lt;=!=&gt;" contains neither "==&gt;" nor "&lt;==".
   */
  static Pattern low_precedence_pattern;

  /** Matches a string or character literal. */
  static Pattern literal_pattern;

  static Pattern dot_class_pattern;
  static Pattern gteq_pattern;
  static Pattern lteq_pattern;
  static Pattern neq_pattern;
  static Pattern inequality_pattern;
  static Pattern contradict_inv_pattern;
  static Pattern useless_inv_pattern_1;
  static Pattern useless_inv_pattern_2;

  static {
    try {
      orig_pattern = Pattern.compile("\\borig\\s*\\(|\\\\(old|new)\\s*\\(");
      result_pattern = Pattern.compile("\\\\result\\b");
      orig_cluster_pattern = Pattern.compile("\\borig\\(cluster\\)");
      low_precedence_pattern = Pattern.compile("\\|\\|| or |==>|<==|<=!=>|\\?");
      literal_pattern = Pattern.compile("\"(?:[^\"\\\\]|\\\\.)*\"|'(?:[^'\\\\]|\\\\.)*'");
      dot_class_pattern = Pattern.compile("\\.class");
      inequality_pattern = Pattern.compile("[\\!<>]=");
      gteq_pattern = Pattern.compile(">=");
      lteq_pattern = Pattern.compile("<=");
      neq_pattern = Pattern.compile("\\!=");
      contradict_inv_pattern =
          Pattern.compile("(^| && )(.*) == -?[0-9]+ &.*& \\2 == -?[0-9]+($| && )");
      useless_inv_pattern_1 =
          Pattern.compile("(^| && )(.*) > -?[0-9]+ &.*& \\2 > -?[0-9]+($| && )");
      useless_inv_pattern_2 =
          Pattern.compile("(^| && )(.*) < -?[0-9]+ &.*& \\2 < -?[0-9]+($| && )");
    } catch (PatternSyntaxException me) {
      throw new Error("ExtractConsequent: Error while compiling pattern " + me.getMessage());
    }
  }
}
