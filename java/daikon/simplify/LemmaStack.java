package daikon.simplify;

import daikon.inv.Invariant;
import java.io.Closeable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.NavigableSet;
import java.util.Set;
import java.util.Stack;
import java.util.TreeSet;
import org.checkerframework.checker.calledmethods.qual.EnsuresCalledMethods;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.mustcall.qual.CreatesMustCallFor;
import org.checkerframework.checker.mustcall.qual.MustCall;
import org.checkerframework.checker.mustcall.qual.Owning;
import org.checkerframework.checker.nullness.qual.EnsuresNonNull;

/**
 * A stack of Lemmas that shadows the stack of assumptions that Simplify keeps. Keeping this stack
 * is necessary if we're to be able to restart Simplify from where we left off after it hangs, but
 * it's also a convenient place to hang routines that any Simplify client can use.
 */
@SuppressWarnings("JdkObsolete") // Stack has methods that ArrayDeque lacks, such as elementAt()
@MustCall("close") public class LemmaStack implements Closeable {
  /**
   * Boolean. Controls Daikon's response when inconsistent invariants are discovered while running
   * Simplify. If false, Daikon will give up on using Simplify for that program point. If true,
   * Daikon will try to find a small subset of the invariants that cause the contradiction and avoid
   * them, to allow processing to continue. For more information, see the section on troubleshooting
   * contradictory invariants in the Daikon manual.
   */
  public static boolean dkconfig_remove_contradictions = true;

  /**
   * Boolean. Controls Daikon's response when inconsistent invariants are discovered while running
   * Simplify. If true, Daikon will print an error message to the standard error stream listing the
   * contradictory invariants. This is mainly intended for debugging Daikon itself, but can
   * sometimes be helpful in tracing down other problems. For more information, see the section on
   * troubleshooting contradictory invariants in the Daikon manual.
   */
  public static boolean dkconfig_print_contradictions = false;

  /**
   * Boolean. If true, ask Simplify to check a simple proposition after each assumption is pushed,
   * providing an opportunity to wait for output from Simplify and potentially receive error
   * messages about the assumption. When false, long sequences of assumptions may be pushed in a
   * row, so that by the time an error message arrives, it's not clear which input caused the error.
   * Of course, Daikon's input to Simplify isn't supposed to cause errors, so this option should
   * only be needed for debugging.
   */
  public static boolean dkconfig_synchronous_errors = false;

  private Stack<Lemma> lemmas;
  private @Owning SessionManager session;

  /** Tell Simplify to assume a lemma, which should already be on our stack. */
  private void assume(@UnknownInitialization(LemmaStack.class) LemmaStack this, Lemma lemma)
      throws TimeoutException {
    session.request(new CmdAssume(lemma.formula));
  }

  /** Assume a list of lemmas. */
  private void assumeAll(@UnknownInitialization(LemmaStack.class) LemmaStack this, List<Lemma> invs)
      throws TimeoutException {
    for (Lemma lem : invs) {
      assume(lem);
    }
  }

  /** Pop a lemma off Simplify's stack. */
  private void unAssume() {
    try {
      session.request(CmdUndoAssume.single);
    } catch (TimeoutException e) {
      throw new Error("Unexpected timeout on (BG_POP)");
    }
  }

  /**
   * Pop a bunch of lemmas off Simplify's stack. Since it's a stack, it only works to unassume the
   * things you most recently assumed, but we aren't smart enough to check that.
   *
   * @param invs the lemmas to pop off Simplify's stack
   */
  private void unAssumeAll(List<Lemma> invs) {
    for (@SuppressWarnings("UnusedVariable") Lemma lem : invs) {
      unAssume();
    }
  }

  /** Try to start Simplify. */
  @CreatesMustCallFor("this")
  @EnsuresNonNull("session")
  private void startProver(@UnknownInitialization LemmaStack this) throws SimplifyError {
    SessionManager session_try = SessionManager.attemptProverStartup();
    if (session != null) {
      try {
        session.close();
      } catch (Exception e) {
        throw new SimplifyError(e);
      }
    }
    if (session_try != null) {
      session = session_try;
    } else {
      throw new SimplifyError("Couldn't start Simplify");
    }
  }

  /** Try to restart Simplify back where we left off, after killing it. */
  @CreatesMustCallFor("this")
  private void restartProver(@UnknownInitialization(LemmaStack.class) LemmaStack this)
      throws SimplifyError {
    startProver();
    try {
      assumeAll(lemmas);
    } catch (TimeoutException e) {
      throw new SimplifyError("Simplify restart timed out");
    }
  }

  /** Create a new LemmaStack. */
  public LemmaStack() throws SimplifyError {
    startProver();
    lemmas = new Stack<Lemma>();
    if (daikon.inv.Invariant.dkconfig_simplify_define_predicates) {
      pushLemmas(Lemma.lemmasList());
    }
  }

  /** Pop a lemma from our and Simplify's stacks. */
  public void popLemma() {
    unAssume();
    lemmas.pop();
  }

  /**
   * Push an assumption onto our and Simplify's stacks.
   *
   * @param lem the assumption
   * @return true if success, false if Simplify times out
   */
  @SuppressWarnings("builder:reset.not.owning") // only resets conditionally, on exception path
  public boolean pushLemma(@UnknownInitialization(LemmaStack.class) LemmaStack this, Lemma lem)
      throws SimplifyError {
    SimpUtil.assert_well_formed(lem.formula);
    try {
      assume(lem);
      lemmas.push(lem);
      if (dkconfig_synchronous_errors) {
        // The following debugging code causes us to flush all our input
        // to Simplify after each lemma, and is useful to figure out
        // which lemma an error message refers to.
        try {
          checkString("(AND)");
        } catch (SimplifyError err) {
          System.err.println("Error after pushing " + lem.summarize() + " " + lem.formula);
          throw err;
        }
      }

      return true;
    } catch (TimeoutException e) {
      restartProver();
      return false;
    }
  }

  /** Push a vector of assumptions onto our and Simplify's stacks. */
  public void pushLemmas(
      @UnknownInitialization(LemmaStack.class) LemmaStack this, List<Lemma> newLemmas)
      throws SimplifyError {
    for (Lemma lem : newLemmas) {
      pushLemma(lem);
    }
  }

  /**
   * Ask Simplify whether a string is a valid statement, given our assumptions. Returns 'T' if
   * Simplify says yes, 'F' if Simplify says no, or '?' if we have to kill Simplify because it won't
   * answer.
   *
   * @param str the string to check
   * @return 'T' if Simplify says yes, 'F' if Simplify says no, or '?' if Simplify does not answer
   */
  @SuppressWarnings("builder:reset.not.owning") // only resets conditionally, on exception path
  private char checkString(@UnknownInitialization(LemmaStack.class) LemmaStack this, String str)
      throws SimplifyError {
    SimpUtil.assert_well_formed(str);
    CmdCheck cc = new CmdCheck(str);
    try {
      session.request(cc);
    } catch (TimeoutException e) {
      restartProver();
      return '?';
    }
    if (cc.unknown) {
      return '?';
    }
    return cc.valid ? 'T' : 'F';
  }

  /**
   * Ask Simplify whether a lemma is valid, given our assumptions. Returns 'T' if Simplify says yes,
   * 'F' if Simplify says no, or '?' if we have to kill Simplify because it won't answer.
   */
  public char checkLemma(Lemma lemma) throws SimplifyError {
    return checkString(lemma.formula);
  }

  /**
   * Ask Simplify whether the assumptions we've pushed so far are contradictory. Returns 'T' if
   * Simplify says yes, 'F' if Simplify says no, or '?' if we have to kill Simplify because it won't
   * answer.
   */
  public char checkForContradiction() throws SimplifyError {
    return checkString("(OR)"); // s/b always false
  }

  /**
   * Return true if all the invariants in invs[i] in invs[] not between min and max (inclusive) for
   * which excluded[i] is false, together imply the formula conseq.
   */
  private boolean allExceptImply(Lemma[] invs, boolean[] excluded, int min, int max, String conseq)
      throws TimeoutException {
    int assumed = 0;
    for (int i = 0; i < invs.length; i++) {
      if (!excluded[i] && (i < min || i > max)) {
        assume(invs[i]);
        assumed++;
      }
    }
    boolean valid = checkString(conseq) != 'F';
    for (int i = 0; i < assumed; i++) {
      unAssume();
    }
    return valid;
  }

  /** Return true if all the elements of bools between min and max (inclusive) are true. */
  private static boolean allTrue(boolean[] bools, int min, int max) {
    for (int i = min; i <= max; i++) {
      if (!bools[i]) {
        return false;
      }
    }
    return true;
  }

  /**
   * Find a subset of invs[] that imply consequence, such that no subset of that set does. Note that
   * we may not return the smallest such set. The set is currently returned in the same order as the
   * invariants appeared in invs[].
   */
  private List<Lemma> minimizeAssumptions(Lemma[] invs, String consequence)
      throws TimeoutException {
    boolean[] excluded = new boolean[invs.length];

    for (int size = invs.length / 2; size > 1; size /= 2) {
      for (int start = 0; start < invs.length; start += size) {
        int end = Math.min(start + size - 1, invs.length - 1);
        if (!allTrue(excluded, start, end)
            && allExceptImply(invs, excluded, start, end, consequence)) {
          for (int i = start; i <= end; i++) {
            excluded[i] = true;
          }
        }
      }
    }

    boolean reduced;
    do {
      reduced = false;
      for (int i = 0; i < invs.length; i++) {
        if (!excluded[i]) {
          if (allExceptImply(invs, excluded, i, i, consequence)) {
            excluded[i] = true;
            reduced = true;
          }
        }
      }
    } while (reduced);
    List<Lemma> new_invs = new ArrayList<>();
    for (int i = 0; i < invs.length; i++) {
      if (!excluded[i]) {
        new_invs.add(invs[i]);
      }
    }
    return new_invs;
  }

  private static List<Lemma> filterByClass(
      List<Lemma> lems, Set<Class<? extends Invariant>> blacklist) {
    List<Lemma> new_lems = new ArrayList<>();
    for (Lemma lem : lems) {
      Class<? extends Invariant> cls = lem.invClass();
      if (cls != null && !blacklist.contains(cls)) {
        new_lems.add(lem);
      }
    }
    return new_lems;
  }

  private void minimizeClasses_rec(
      String result,
      List<Lemma> lems,
      Set<Class<? extends Invariant>> exclude,
      Set<Set<Class<? extends Invariant>>> black,
      Set<Set<Class<? extends Invariant>>> gray,
      Set<Set<Class<? extends Invariant>>> found)
      throws TimeoutException {
    for (Set<Class<? extends Invariant>> known : found) {
      // If known and exclude are disjoint, return
      Set<Class<? extends Invariant>> exclude2 = new HashSet<Class<? extends Invariant>>(exclude);
      exclude2.retainAll(known);
      if (exclude2.isEmpty()) {
        return;
      }
    }
    int mark = markLevel();
    List<Lemma> filtered = filterByClass(lems, exclude);
    pushLemmas(filtered);
    boolean holds = checkString(result) == 'T';
    popToMark(mark);
    if (holds) {
      List<Lemma> mini = minimizeAssumptions(filtered.toArray(new Lemma[0]), result);
      Set<Class<? extends Invariant>> used = new HashSet<Class<? extends Invariant>>();
      for (Lemma mlem : mini) {
        Class<? extends Invariant> c = mlem.invClass();
        if (c != null) {
          used.add(c);
        }
      }
      for (Lemma mlem : mini) {
        System.err.println(mlem.summarize());
        System.err.println(mlem.formula);
      }
      System.err.println("-----------------------------------");
      System.err.println(result);
      System.err.println();

      found.add(used);
      for (Class<? extends Invariant> c : used) {
        Set<Class<? extends Invariant>> step = new HashSet<Class<? extends Invariant>>(exclude);
        step.add(c);
        if (!black.contains(step) && !gray.contains(step)) {
          gray.add(step);
          minimizeClasses_rec(result, lems, step, black, gray, found);
        }
      }
    }
    black.add(exclude);
  }

  public List<Set<Class<? extends Invariant>>> minimizeClasses(String result) {
    List<Lemma> assumptions = new ArrayList<>(lemmas);
    List<Set<Class<? extends Invariant>>> found = new ArrayList<>();
    try {
      unAssumeAll(lemmas);
      if (checkString(result) == 'F') {
        Set<Class<? extends Invariant>> exclude = new HashSet<Class<? extends Invariant>>();
        Set<Set<Class<? extends Invariant>>> black = new HashSet<Set<Class<? extends Invariant>>>();
        Set<Set<Class<? extends Invariant>>> gray = new HashSet<Set<Class<? extends Invariant>>>();
        Set<Set<Class<? extends Invariant>>> found_set =
            new HashSet<Set<Class<? extends Invariant>>>();
        minimizeClasses_rec(result, assumptions, exclude, black, gray, found_set);
        found.addAll(found_set);
      }
      assumeAll(lemmas);
    } catch (TimeoutException e) {
      throw new Error();
    }
    return found;
  }

  /**
   * Return a minimal set of assumptions from the stack that imply a given string.
   *
   * @param str the expression to make true
   * @return a minimal set of assumptions from the stack that imply the given string
   */
  @SuppressWarnings("builder:reset.not.owning") // only resets conditionally, on exception path
  private List<Lemma> minimizeReasons(String str) throws SimplifyError {
    assert checkString(str) == 'T';
    unAssumeAll(lemmas);
    List<Lemma> result;
    try {
      Lemma[] lemmaAry = lemmas.toArray(new Lemma[0]);
      // shuffle(lemmaAry, new Random());
      result = minimizeAssumptions(lemmaAry, str);
      assumeAll(lemmas);
    } catch (TimeoutException e) {
      System.err.println("Minimzation timed out");
      restartProver();
      return lemmas;
    }
    return result;
  }

  /**
   * Return a set of contradictory assumptions from the stack (as a vector of Lemmas) which are
   * minimal in the sense that no proper subset of them are contradictory as far as Simplify can
   * tell.
   */
  public List<Lemma> minimizeContradiction() throws SimplifyError {
    return minimizeReasons("(OR)");
  }

  /**
   * Return a set of assumptions from the stack (as a vector of Lemmas) that imply the given Lemma
   * and which are minimal in the sense that no proper subset of them imply it as far as Simplify
   * can tell.
   */
  public List<Lemma> minimizeProof(Lemma lem) throws SimplifyError {
    return minimizeReasons(lem.formula);
  }

  /**
   * Remove some lemmas from the stack, such that our set of assumptions is no longer contradictory.
   * This is not a very principled thing to do, but it's better than just giving up. The approach is
   * relatively slow, trying not to remove too many lemmas.
   */
  public void removeContradiction() throws SimplifyError {
    do {
      List<Lemma> problems = minimizeContradiction();
      if (problems.size() == 0) {
        throw new SimplifyError("Minimization failed");
      }
      Lemma bad = problems.get(problems.size() - 1);
      removeLemma(bad);
      System.err.print("x");
    } while (checkForContradiction() == 'T');
  }

  /**
   * Search for the given lemma in the stack, and then remove it from both our stack and Simplify's.
   * This is rather inefficient.
   */
  public void removeLemma(Lemma bad) throws SimplifyError {
    unAssumeAll(lemmas);
    int spliceOut = -1;
    for (int i = 0; i < lemmas.size(); i++) {
      Lemma lem = lemmas.elementAt(i);
      @SuppressWarnings("interning") // value in list
      boolean isBad = (lem == bad);
      if (isBad) {
        spliceOut = i;
      } else {
        try {
          assume(lem);
        } catch (TimeoutException e) {
          throw new SimplifyError("Timeout in contradiction removal");
        }
      }
    }
    assert spliceOut != -1;
    lemmas.removeElementAt(spliceOut);
  }

  /** Blow away everything on our stack and Simplify's. */
  public void clear() {
    unAssumeAll(lemmas);
    lemmas.removeAllElements();
  }

  /**
   * Return a reference to the current position on the lemma stack. If, after pushing some stuff,
   * you want to get back here, pass the mark to popToMark(). This will only work if you use these
   * routines in a stack-disciplined way, of course. In particular, beware that
   * removeContradiction() invalidates marks, since it can remove a lemma from anywhere on the
   * stack.
   */
  public int markLevel() {
    return lemmas.size();
  }

  /** Pop off lemmas from the stack until its level matches mark. */
  public void popToMark(int mark) {
    while (lemmas.size() > mark) popLemma();
  }

  /**
   * Convenience method to print a vector of lemmas, in both their human-readable and Simplify
   * forms.
   */
  public static void printLemmas(java.io.PrintStream out, List<Lemma> v) {
    for (Lemma lem : v) {
      out.println(lem.summarize());
      out.println("    " + lem.formula);
    }
  }

  /** Dump the state of the stack to a file, for debugging manually in Simplify. */
  public void dumpLemmas(java.io.PrintStream out) {
    for (Lemma lem : lemmas) {
      out.println("(BG_PUSH " + lem.formula + ")");
    }
  }

  private static NavigableSet<Long> ints_seen = new TreeSet<>();

  /** Keep track that we've seen this number in formulas, for the sake of pushOrdering. */
  public static void noticeInt(long i) {
    ints_seen.add(i);
  }

  public static void clearInts() {
    ints_seen = new TreeSet<Long>();
  }

  /**
   * Integers smaller in absolute value than this will be printed directly. Larger integers will be
   * printed abstractly (see Invariant.simplify_format_long and a comment there for details).
   */
  public static final long SMALL_INTEGER = 32000;

  /** For all the integers we've seen, tell Simplify about the ordering between them. */
  public void pushOrdering() throws SimplifyError {
    long last_long = Long.MIN_VALUE;
    for (Long ll : ints_seen) {
      long l = ll.longValue();
      if (l == Long.MIN_VALUE) {
        continue;
      }
      assert l != last_long;
      String formula =
          "(< " + SimpUtil.formatInteger(last_long) + " " + SimpUtil.formatInteger(l) + ")";
      Lemma lem = new Lemma(last_long + " < " + l, formula);
      pushLemma(lem);
      if (l > -SMALL_INTEGER && l < SMALL_INTEGER) {
        // Only give the concrete value for "small" integers.
        String eq_formula = "(EQ " + l + " " + SimpUtil.formatInteger(l) + ")";
        Lemma eq_lem = new Lemma(l + " == " + l, eq_formula);
        pushLemma(eq_lem);
      }
      last_long = l;
    }
  }

  /** Releases resources held by this. */
  @SuppressWarnings("builder:contracts.postcondition") // performed on a local alias, not the field
  @EnsuresCalledMethods(value = "session", methods = "close")
  @Override
  public void close(@GuardSatisfied LemmaStack this) {
    // this.session should be effectively final in that it refers
    // to the same value throughout the execution of this method.
    // Unfortunately, the Lock Checker cannot verify this,
    // so a final local variable is used to satisfy the Lock Checker's
    // requirement that all variables used as locks be final or
    // effectively final.  If a bug exists whereby this.session
    // is not effectively final, this would unfortunately mask that error.
    final SessionManager session = this.session;
    session.close();
    synchronized (session) {
      session.notifyAll();
    }
  }
}
