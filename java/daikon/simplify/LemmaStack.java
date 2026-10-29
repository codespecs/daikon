package daikon.simplify;

import daikon.inv.Invariant;
import java.io.Closeable;
import java.util.ArrayList;
import java.util.Collections;
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
  private void unAssume() throws TimeoutException {
    session.request(CmdUndoAssume.single);
  }

  /**
   * Pop a bunch of lemmas off Simplify's stack. Since it's a stack, it only works to unassume the
   * things you most recently assumed, but we aren't smart enough to check that.
   *
   * @param count the number of lemmas to pop off Simplify's stack
   * @throws TimeoutException if Simplify times out
   */
  private void unAssume(int count) throws TimeoutException {
    for (int i = 0; i < count; i++) {
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

  /**
   * Try to restart Simplify, after killing it.
   *
   * @param stack the lemmas that Simplify's stack should hold after the restart
   */
  @CreatesMustCallFor("this")
  private void restartProver(
      @UnknownInitialization(LemmaStack.class) LemmaStack this, List<Lemma> stack)
      throws SimplifyError {
    startProver();
    try {
      assumeAll(stack);
    } catch (TimeoutException e) {
      throw new SimplifyError("Simplify restart timed out");
    }
  }

  /** An operation on Simplify that might time out. */
  @FunctionalInterface
  private interface SimplifyOperation {
    /**
     * Performs the operation.
     *
     * @throws TimeoutException if Simplify times out
     */
    void run() throws TimeoutException;
  }

  /**
   * Performs an operation on Simplify. If Simplify times out, this restarts it.
   *
   * @param operation the operation to perform
   * @param stack the lemmas that Simplify's stack should hold after the operation
   */
  @SuppressWarnings("builder:reset.not.owning") // only resets conditionally, on exception path
  private void runOrRestart(SimplifyOperation operation, List<Lemma> stack) throws SimplifyError {
    try {
      operation.run();
    } catch (TimeoutException e) {
      restartProver(stack);
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

  /** Pop a lemma from our and Simplify's stacks. If Simplify times out, this restarts it. */
  public void popLemma() throws SimplifyError {
    lemmas.pop();
    runOrRestart(this::unAssume, lemmas);
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
      restartProver(lemmas);
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
   * <p>If Simplify times out, this restarts it with the assumptions in {@link #lemmas}. Therefore,
   * call this only when Simplify's stack holds exactly the assumptions in {@link #lemmas}. To check
   * a string under other assumptions, call {@link #checkStringAssuming}, which leaves recovery from
   * a timeout to its caller.
   *
   * @param str the string to check
   * @return 'T' if Simplify says yes, 'F' if Simplify says no, or '?' if Simplify does not answer
   */
  @SuppressWarnings("builder:reset.not.owning") // only resets conditionally, on exception path
  private char checkString(@UnknownInitialization(LemmaStack.class) LemmaStack this, String str)
      throws SimplifyError {
    try {
      return checkStringOrTimeout(str);
    } catch (TimeoutException e) {
      restartProver(lemmas);
      return '?';
    }
  }

  /**
   * Ask Simplify whether a string is a valid statement, given the assumptions on Simplify's stack.
   *
   * @param str the string to check
   * @return 'T' if Simplify says yes, 'F' if Simplify says no, or '?' if Simplify does not know
   * @throws TimeoutException if Simplify times out
   */
  private char checkStringOrTimeout(
      @UnknownInitialization(LemmaStack.class) LemmaStack this, String str)
      throws TimeoutException {
    SimpUtil.assert_well_formed(str);
    CmdCheck cc = new CmdCheck(str);
    session.request(cc);
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
   * Ask Simplify whether the given assumptions imply a string. Simplify's stack must be empty, and
   * it is empty afterward unless Simplify times out.
   *
   * @param assumptions the assumptions
   * @param str the string to check
   * @return 'T' if Simplify says yes, 'F' if Simplify says no, or '?' if Simplify does not know
   * @throws TimeoutException if Simplify times out
   */
  private char checkStringAssuming(List<Lemma> assumptions, String str) throws TimeoutException {
    assumeAll(assumptions);
    char result = checkStringOrTimeout(str);
    unAssume(assumptions.size());
    return result;
  }

  /**
   * Returns true if all the invariants in invs[i] in invs[] not between min and max (inclusive) for
   * which excluded[i] is false, together imply the formula conseq, or if Simplify does not know.
   * Simplify's stack must be empty.
   */
  private boolean allExceptImply(Lemma[] invs, boolean[] excluded, int min, int max, String conseq)
      throws TimeoutException {
    List<Lemma> assumptions = new ArrayList<>();
    for (int i = 0; i < invs.length; i++) {
      if (!excluded[i] && (i < min || i > max)) {
        assumptions.add(invs[i]);
      }
    }
    return checkStringAssuming(assumptions, conseq) != 'F';
  }

  /** Returns true if all the elements of bools between min and max (inclusive) are true. */
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
   * invariants appeared in invs[]. Simplify's stack must be empty.
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
    List<Lemma> filtered = filterByClass(lems, exclude);
    if (checkStringAssuming(filtered, result) == 'T') {
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

  /**
   * Pop the lemmas in {@link #lemmas} off Simplify's stack, leaving it empty. If Simplify times
   * out, this restarts it with an empty stack.
   */
  private void unAssumeLemmas() throws SimplifyError {
    runOrRestart(() -> unAssume(lemmas.size()), Collections.emptyList());
  }

  /**
   * Push the lemmas in {@link #lemmas} onto Simplify's empty stack. If Simplify times out, this
   * restarts it.
   */
  private void reassumeLemmas() throws SimplifyError {
    runOrRestart(() -> assumeAll(lemmas), lemmas);
  }

  /**
   * Returns sets of invariant classes whose lemmas on the stack imply a given string. Each set is
   * the classes of the lemmas in a minimal subset of the stack that implies the string. Lemmas that
   * do not come from an invariant class are never used. Also prints each minimal subset of lemmas
   * to System.err.
   *
   * <p>If Simplify times out, this prints a message, restarts Simplify, and returns the sets found
   * so far.
   *
   * @param result the string to imply
   * @return sets of invariant classes whose lemmas imply {@code result}; empty if {@code result} is
   *     valid without any assumptions or if no such set is found
   * @throws SimplifyError if Simplify cannot be restarted after a timeout
   */
  @SuppressWarnings("builder:reset.not.owning") // only resets conditionally, on exception path
  public List<Set<Class<? extends Invariant>>> minimizeClasses(String result) throws SimplifyError {
    List<Lemma> assumptions = new ArrayList<>(lemmas);
    Set<Set<Class<? extends Invariant>>> found = new HashSet<Set<Class<? extends Invariant>>>();
    unAssumeLemmas();
    try {
      if (checkStringAssuming(Collections.emptyList(), result) == 'F') {
        Set<Class<? extends Invariant>> exclude = new HashSet<Class<? extends Invariant>>();
        Set<Set<Class<? extends Invariant>>> black = new HashSet<Set<Class<? extends Invariant>>>();
        Set<Set<Class<? extends Invariant>>> gray = new HashSet<Set<Class<? extends Invariant>>>();
        minimizeClasses_rec(result, assumptions, exclude, black, gray, found);
      }
    } catch (TimeoutException e) {
      System.err.println("Minimization timed out");
      restartProver(Collections.emptyList());
    }
    reassumeLemmas();
    return new ArrayList<>(found);
  }

  /**
   * Returns a minimal set of assumptions from the stack that imply a given string. If Simplify
   * times out, this prints a message, restarts Simplify, and returns all the assumptions on the
   * stack.
   *
   * @param str the expression to make true
   * @return a minimal set of assumptions from the stack that imply the given string, or all the
   *     assumptions on the stack if Simplify times out
   */
  @SuppressWarnings("builder:reset.not.owning") // only resets conditionally, on exception path
  private List<Lemma> minimizeReasons(String str) throws SimplifyError {
    // TODO: This assertion has a side effect: if Simplify times out, checkString() restarts it.
    // Also, a timeout makes the assertion fail.
    assert checkString(str) == 'T';
    unAssumeLemmas();
    Lemma[] lemmaAry = lemmas.toArray(new Lemma[0]);
    // shuffle(lemmaAry, new Random());
    List<Lemma> result;
    try {
      result = minimizeAssumptions(lemmaAry, str);
    } catch (TimeoutException e) {
      System.err.println("Minimization timed out");
      restartProver(Collections.emptyList());
      result = new ArrayList<>(lemmas);
    }
    reassumeLemmas();
    return result;
  }

  /**
   * Returns a set of contradictory assumptions from the stack (as a vector of Lemmas) which are
   * minimal in the sense that no proper subset of them are contradictory as far as Simplify can
   * tell.
   */
  public List<Lemma> minimizeContradiction() throws SimplifyError {
    return minimizeReasons("(OR)");
  }

  /**
   * Returns a set of assumptions from the stack (as a vector of Lemmas) that imply the given Lemma
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
      if (problems.isEmpty()) {
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
    int spliceOut = -1;
    for (int i = 0; i < lemmas.size(); i++) {
      Lemma lem = lemmas.elementAt(i);
      @SuppressWarnings("interning") // value in list
      boolean isBad = (lem == bad);
      if (isBad) {
        spliceOut = i;
      }
    }
    assert spliceOut != -1;
    // Pop the bad lemma and the lemmas above it, then push back the lemmas that were above it.
    int popCount = lemmas.size() - spliceOut;
    lemmas.removeElementAt(spliceOut);
    List<Lemma> above = lemmas.subList(spliceOut, lemmas.size());
    runOrRestart(
        () -> {
          unAssume(popCount);
          assumeAll(above);
        },
        lemmas);
  }

  /** Blow away everything on our stack and Simplify's. If Simplify times out, this restarts it. */
  public void clear() throws SimplifyError {
    popToMark(0);
  }

  /**
   * Returns a reference to the current position on the lemma stack. If, after pushing some stuff,
   * you want to get back here, pass the mark to popToMark(). This will only work if you use these
   * routines in a stack-disciplined way, of course. In particular, beware that
   * removeContradiction() invalidates marks, since it can remove a lemma from anywhere on the
   * stack.
   */
  public int markLevel() {
    return lemmas.size();
  }

  /**
   * Pop off lemmas from the stack until its level matches mark. If Simplify times out, this
   * restarts it.
   */
  public void popToMark(int mark) throws SimplifyError {
    int popCount = lemmas.size() - mark;
    if (popCount <= 0) {
      return;
    }
    lemmas.setSize(mark);
    runOrRestart(() -> unAssume(popCount), lemmas);
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
  @EnsuresCalledMethods(value = "session", methods = "close")
  @Override
  public void close(@GuardSatisfied LemmaStack this) {
    session.close();
  }
}
