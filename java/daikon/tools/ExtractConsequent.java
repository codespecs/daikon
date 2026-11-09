package daikon.tools;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.logging.Level.FINE;
import static java.util.logging.Level.INFO;

import daikon.Daikon;
import daikon.DaikonGetopt;
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
import daikon.inv.unary.OneOf;
import daikon.inv.unary.scalar.OneOfScalar;
import gnu.getopt.*;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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

  /* A HashMap whose keys are PPT names (Strings) and whose values are
  HashMaps whose keys are predicate names (Strings) and whose values are
   HashMaps whose keys are Strings (normalized java-format invariants)
     and whose values are HashedConsequent objects. */
  private static Map<String, Map<String, Map<String, HashedConsequent>>> pptname_to_conditions =
      new HashMap<>();

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
    DaikonGetopt g = new DaikonGetopt(args, "", longopts, usage);
    int c;
    while ((c = g.getopt()) != -1) {
      switch (c) {
        case 0:
          // got a long option
          String option_name = longopts[g.getLongind()].getName();
          if (Daikon.suppress_redundant_SWITCH.equals(option_name)) {
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
            throw new Daikon.BugInDaikon("Unhandled long option " + option_name);
          }
          break;
        default:
          throw new Daikon.BugInDaikon("getopt() returned " + c);
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
        List<String> condsJava = new ArrayList<>();
        List<String> condsDaikon = new ArrayList<>();
        List<String> condsESC = new ArrayList<>();
        List<String> condsSimplify = new ArrayList<>();
        List<Invariant> condsInv = new ArrayList<>();
        for (Map.Entry<@KeyFor("conditions") String, HashedConsequent> entry2 :
            conditions.entrySet()) {
          String condIndex = entry2.getKey();
          HashedConsequent cond = entry2.getValue();
          if (cond.fakeFor != null) {
            continue;
          }
          // The key of a condition that is not fake is the condition in Java format.
          String javaStr = condIndex;
          String daikonStr = formatCondition(cond.inv, OutputFormat.DAIKON);
          String escStr = formatCondition(cond.inv, OutputFormat.ESCJAVA);
          String simplifyStr = formatCondition(cond.inv, OutputFormat.SIMPLIFY);
          allConds.add(combineDummy(condIndex, "<dummy> " + daikonStr, escStr, simplifyStr));
          //           allConds.add(condIndex);
          condsJava.add(javaStr);
          condsDaikon.add(daikonStr);
          condsESC.add(escStr);
          condsSimplify.add(simplifyStr);
          condsInv.add(cond.inv);
        }
        String conj = conjunction(condsJava, " && ");
        // Avoid inserting self-contradictory conditions such as "x == 1 &&
        // x == 2", or conjunctions of only a single condition.
        if (condsJava.size() < 2
            || isContradictory(condsInv)
            || useless_inv_pattern_1.matcher(conj).find()
            || useless_inv_pattern_2.matcher(conj).find()) {
          // System.out.println("Suppressing: " + conj);
        } else {
          allConds.add(
              combineDummy(
                  conj,
                  conjunction(condsDaikon, " and "),
                  conjunction(condsESC, " && "),
                  "(AND " + String.join(" ", condsSimplify) + ")"));
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
   * Returns the invariant formatted as a splitting condition in the given format. A OneOfScalar
   * invariant over a boolean variable whose file representation type is int, such as "b == 1", is
   * written as "b == true" (see {@link #intBooleanValue}). In Java format, the result can refer to
   * the return value and to pre-state values as "\result" and "\old(x)"; see {@link
   * #toSpinfoCondition}.
   *
   * @param inv an invariant
   * @param format the format in which to write the invariant
   * @return the invariant formatted as a splitting condition
   */
  static String formatCondition(Invariant inv, OutputFormat format) {
    Boolean value = (inv instanceof OneOfScalar) ? intBooleanValue((OneOfScalar) inv) : null;
    String result;
    if (value == null) {
      result = inv.format_using(format);
    } else {
      VarInfo var = ((OneOfScalar) inv).var();
      if (format == OutputFormat.SIMPLIFY) {
        result = "(EQ " + var.simplifyFixedupName() + " " + (value ? "|@true|" : "|@false|") + ")";
      } else {
        result = var.name_using(format) + " == " + value;
      }
    }
    return result;
  }

  /**
   * Returns true if two of the given invariants give the same variable different values, as "x ==
   * 1" and "x == 2" do, or "b == true" and "b == false". The conjunction of such invariants is
   * always false.
   *
   * @param invs invariants
   * @return true if two of the invariants give the same variable different values
   */
  static boolean isContradictory(Collection<Invariant> invs) {
    // Maps a variable's name to the value that some invariant in invs gives it.
    Map<String, Object> values = new HashMap<>();
    for (Invariant inv : invs) {
      if (!(inv instanceof OneOf)) {
        continue;
      }
      OneOf oneOf = (OneOf) inv;
      if (oneOf.num_elts() != 1) {
        continue;
      }
      Object value = oneOf.elt();
      Object otherValue = values.put(oneOf.var().name(), value);
      if (otherValue != null && !sameValue(value, otherValue)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns true if the given values, which are values of the same variable, are equal. Two
   * floating-point values are equal if they are approximately equal according to {@link
   * Global#fuzzy}.
   *
   * @param v1 a value, such as a Long, Double, or String
   * @param v2 another value of the same variable as v1
   * @return true if the values are equal
   */
  private static boolean sameValue(Object v1, Object v2) {
    if (v1 instanceof Double && v2 instanceof Double) {
      return Global.fuzzy.eq((Double) v1, (Double) v2);
    }
    return v1.equals(v2);
  }

  /**
   * Returns the given condition as a .spinfo file writes it, or null if the condition refers to a
   * post-state value or uses a class literal such as "Foo.class". The result writes each "\result"
   * as "return" and each "\old(x)" as "orig(x)", which is how a .spinfo file refers to the return
   * value and to pre-state values. A splitter cannot use a condition that refers to a post-state
   * value, such as "\post(x)" or "\new(x)", or that uses a class literal. Text within a string or
   * character literal is neither changed nor examined.
   *
   * @param condition a condition in Java format, such as "\result &gt; \old(this.size)"
   * @return the condition as a .spinfo file writes it, such as "return &gt; orig(this.size)", or
   *     null if the condition refers to a post-state value or uses a class literal
   */
  static @Nullable String toSpinfoCondition(String condition) {
    StringBuilder result = new StringBuilder();
    Matcher m = literal_pattern.matcher(condition);
    int start = 0;
    while (true) {
      boolean found = m.find();
      // The text between the previous literal and the next one (or the end of the condition).
      String text = condition.substring(start, found ? m.start() : condition.length());
      if (post_pattern.matcher(text).find() || dot_class_pattern.matcher(text).find()) {
        return null;
      }
      text = result_pattern.matcher(text).replaceAll("return");
      result.append(old_pattern.matcher(text).replaceAll("orig("));
      if (!found) {
        return result.toString();
      }
      result.append(m.group());
      start = m.end();
    }
  }

  /**
   * Returns the given expression, parenthesized if it might contain an operator whose precedence is
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
   * Returns the conjunction of the given conditions, each parenthesized if necessary, such as
   * {@code (x > 0 || y > 0) && z > 0}.
   *
   * @param conditions conditions, in Java, ESC, or Daikon format
   * @param and the conjunction operator, such as {@code " && "} or {@code " and "}
   * @return the conjunction of the conditions
   */
  static String conjunction(List<String> conditions, String and) {
    List<String> conjuncts = new ArrayList<>(conditions.size());
    for (String condition : conditions) {
      conjuncts.add(parenthesizeIfNeeded(condition));
    }
    return String.join(and, conjuncts);
  }

  /**
   * Returns true if the invariant is legal Java when its variables are booleans. Daikon represents
   * a boolean as an int, so it might infer numeric invariants, such as "b != 0" or "b1 < b2", that
   * are not legal Java over booleans.
   *
   * @param inv an invariant
   * @return true if the invariant uses no boolean variable, or is legal Java over booleans
   */
  static boolean isLegalForBooleans(Invariant inv) {
    VarInfo[] vis = inv.ppt.var_infos;
    // OneOfScalar is the only scalar invariant that formats a boolean specially, as "b == true".
    // It decides whether its variable is a boolean from the file representation type, which can
    // differ from the declared type that a splitter uses.  formatCondition writes "b == 1" as
    // "b == true" when the declared type is boolean and the file representation type is int.
    if (inv instanceof OneOfScalar) {
      OneOfScalar oneof = (OneOfScalar) inv;
      return oneof.is_boolean() == isBoolean(vis[0]) || intBooleanValue(oneof) != null;
    }
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

  /**
   * Returns the value of the variable of the given OneOfScalar invariant, if the variable is a
   * boolean whose file representation type is int and the invariant is "b == 0" or "b == 1".
   * OneOfScalar formats such an invariant as "b == 0" or "b == 1", which is not legal Java.
   *
   * @param oneof a OneOfScalar invariant
   * @return false if the invariant is "b == 0", true if it is "b == 1", or null if the variable is
   *     not a boolean whose file representation type is int or the invariant is neither
   */
  static @Nullable Boolean intBooleanValue(OneOfScalar oneof) {
    if (oneof.is_boolean() || !isBoolean(oneof.var()) || oneof.num_elts() != 1) {
      return null;
    }
    long value = ((Long) oneof.elt()).longValue();
    if (value == 0) {
      return false;
    } else if (value == 1) {
      return true;
    } else {
      return null;
    }
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
      String pptname = cleanup_pptname(ppt.name());
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
          if (debug.isLoggable(FINE)) {
            debug.fine("Not legal Java over booleans: " + inv.format_using(OutputFormat.JAVA));
          }
          continue;
        }

        String inv_string = toSpinfoCondition(formatCondition(inv, OutputFormat.JAVA));
        if (inv_string == null) {
          continue;
        }
        String cluster_key = clusterKey(cluster_inv);
        String fake_inv_string = simplify_inequalities(inv_string);
        HashedConsequent real = new HashedConsequent(inv, null);
        if (!fake_inv_string.equals(inv_string)) {
          // For instance, inv_string is "x != y", fake_inv_string is "x == y"
          HashedConsequent fake = new HashedConsequent(inv, inv_string);
          boolean added = store_invariant(cluster_key, fake_inv_string, fake, pptname);
          if (!added) {
            // We couldn't add "x == y", (when we're "x != y") because
            // it already exists; so don't add "x == y" either.
            continue;
          }
        }
        store_invariant(cluster_key, inv_string, real, pptname);
      }
    }
  }

  /**
   * Returns the key that identifies the cluster that the given invariant describes. The pre-state
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
      pptname_to_conditions.put(pptname, new HashMap<>());
    }

    Map<String, Map<String, HashedConsequent>> cluster_to_conditions =
        pptname_to_conditions.get(pptname);
    if (!cluster_to_conditions.containsKey(predicate)) {
      cluster_to_conditions.put(predicate, new HashMap<>());
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
   * Remove non-word characters and everything after "(" (which includes everything after ":::")
   * from the program point name, leaving "PackageName.ClassName.MethodName".
   *
   * @param pptname a program point name
   * @return the argument, without non-word characters and without parens or ":::" suffix
   */
  private static String cleanup_pptname(String pptname) {
    int index;
    if ((index = pptname.indexOf('(')) > 0) {
      pptname = pptname.substring(0, index);
    }

    if (pptname.endsWith(".")) {
      pptname = pptname.substring(0, pptname.length() - 2);
    }

    Matcher m = non_word_pattern.matcher(pptname);
    return m.replaceAll(".");
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

  /**
   * Matches a post-state value in Java format, such as "\post(x)" or "\new(x)", which a splitter
   * cannot use. The Java format uses "\post" for a variable declared in the old declaration format,
   * and "\new" for a post-state index into a pre-state sequence, such as "\new(i)" in
   * "\old(daikon.Quant.getElement_int(a, \new(i)))", in the new declaration format.
   */
  static Pattern post_pattern;

  /** Matches the return value in Java format. */
  static Pattern result_pattern;

  /** Matches the start of a pre-state value in Java format, such as "\old(" in "\old(x)". */
  static Pattern old_pattern;

  /** Matches the pre-state value of the "cluster" variable, in Daikon format. */
  static Pattern orig_cluster_pattern;

  /**
   * Matches an operator whose precedence is lower than that of conjunction. "&lt;==" is needed for
   * reverse implication, and "&lt;=!=&gt;" contains neither "==&gt;" nor "&lt;==". A "?" matches
   * only if a ":" follows it, as in a conditional expression.
   */
  static Pattern low_precedence_pattern;

  /** Matches a string or character literal. */
  static Pattern literal_pattern;

  /** Matches ".class", as in "Foo.class", but not ".classCount" or ".class$0". */
  static Pattern dot_class_pattern;

  static Pattern non_word_pattern;
  static Pattern gteq_pattern;
  static Pattern lteq_pattern;
  static Pattern neq_pattern;
  static Pattern inequality_pattern;

  static Pattern useless_inv_pattern_1;
  static Pattern useless_inv_pattern_2;

  static {
    try {
      non_word_pattern = Pattern.compile("\\W+");
      post_pattern = Pattern.compile("\\\\(?:post|new)\\s*\\(");
      result_pattern = Pattern.compile("\\\\result\\b");
      old_pattern = Pattern.compile("\\\\old\\s*\\(");
      orig_cluster_pattern = Pattern.compile("\\borig\\(cluster\\)");
      low_precedence_pattern = Pattern.compile("\\|\\|| or |==>|<==|<=!=>|\\?.*:");
      literal_pattern = Pattern.compile("\"(?:[^\"\\\\]|\\\\.)*\"|'(?:[^'\\\\]|\\\\.)*'");
      dot_class_pattern = Pattern.compile("\\.class(?!\\p{javaJavaIdentifierPart})");
      inequality_pattern = Pattern.compile("[\\!<>]=");
      gteq_pattern = Pattern.compile(">=");
      lteq_pattern = Pattern.compile("<=");
      neq_pattern = Pattern.compile("\\!=");
      useless_inv_pattern_1 =
          Pattern.compile("(^| && )(.*) > -?[0-9]+ &.*& \\2 > -?[0-9]+($| && )");
      useless_inv_pattern_2 =
          Pattern.compile("(^| && )(.*) < -?[0-9]+ &.*& \\2 < -?[0-9]+($| && )");
    } catch (PatternSyntaxException me) {
      throw new Error("ExtractConsequent: Error while compiling pattern " + me.getMessage());
    }
  }
}
