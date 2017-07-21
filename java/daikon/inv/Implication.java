package daikon.inv;

import daikon.*;
import daikon.split.PptSplitter;
import java.util.logging.Level;
import java.util.logging.Logger;
import plume.*;

/*>>>
import org.checkerframework.checker.formatter.qual.*;
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
import typequals.*;
*/

// Here Implication is reimplemented as an extension of the new general
// Joiner class

/**
 * The Implication invariant class is used internally within Daikon to handle invariants that are
 * only true when certain other conditions are also true (splitting).
 */
public class Implication extends Joiner {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030822L;

  // orig_left and orig_right are the original invariants, in the original
  // context (the parent of the conditional ppt where predicate and
  // consequent appear).  predicate and consequent might be permuted from
  // the original.  orig_left and orig_right are used in computeConfidence,
  // and orig_right is used in isObvious*.
  /** The original predicate invariant from its original conditional ppt. */
  private Invariant orig_left;
  /**
   * The original consequent invariant from its original conditional ppt. Or, right itself if right
   * is a DummyInvariant from a splitter file.
   */
  private Invariant orig_right;

  public Invariant predicate() {
    return left;
  }

  public Invariant consequent() {
    return right;
  }

  public boolean iff;

  protected Implication(
      PptSlice ppt,
      Invariant predicate,
      Invariant consequent,
      boolean iff,
      Invariant orig_predicate,
      Invariant orig_consequent) {
    super(ppt, predicate, consequent);
    assert (predicate != null);
    assert (consequent != null);
    assert (orig_predicate != null);
    assert (orig_consequent != null);
    this.iff = iff;
    this.orig_left = orig_predicate;
    this.orig_right = orig_consequent;
  }

  /**
   * Creates a new Implication Invariant and adds it to the PptTopLevel.
   *
   * @return null if predicate and the consequent are the same, or if the PptTopLevel already
   *     contains this Implication
   */
  public static /*@Nullable*/ Implication makeImplication(
      PptTopLevel ppt,
      Invariant predicate,
      Invariant consequent,
      boolean iff,
      Invariant orig_predicate,
      Invariant orig_consequent) {
    if (predicate.isSameInvariant(consequent)) {
      PptSplitter.debug.fine(
          "Not creating implication (pred==conseq): " + predicate + " ==> " + consequent);
      return null;
    }

    Implication result =
        new Implication(
            ppt.joiner_view, predicate, consequent, iff, orig_predicate, orig_consequent);

    // Don't add this Implication to the program point if the program
    // point already has this implication.
    if (ppt.joiner_view.hasImplication(result)) {
      return null;
    }

    if (PptSplitter.debug.isLoggable(Level.FINE)) {
      PptSplitter.debug.fine("Creating implication " + predicate + " ==> " + consequent);
    }
    return result;
  }

  @Override
  protected double computeConfidence() {
    double pred_conf = orig_left.computeConfidence();
    double cons_conf = orig_right.computeConfidence();
    if ((pred_conf == CONFIDENCE_NEVER) || (cons_conf == CONFIDENCE_NEVER)) {
      return CONFIDENCE_NEVER;
    }
    double result = confidence_and(pred_conf, cons_conf);
    log("Confidence %s %s/%s for %s", result, pred_conf, cons_conf, format());
    return result;
  }

  @Override
  public String repr(/*>>>@GuardSatisfied Implication this*/) {
    return "[Implication: " + left.repr() + " => " + right.repr() + "]";
  }

  /*@SideEffectFree*/
  @Override
  public String format_using(/*>>>@GuardSatisfied Implication this,*/ OutputFormat format) {
    String pred_fmt = left.format_using(format);
    String consq_fmt = right.format_using(format);
    if (format == OutputFormat.DAIKON || format == OutputFormat.JML) {
      String arrow = (iff ? "  <==>  " : "  ==>  "); // "interned"
      return "(" + pred_fmt + ")" + arrow + "(" + consq_fmt + ")";
    } else if (format == OutputFormat.ESCJAVA) {
      String arrow = (iff ? "  ==  " : "  ==>  "); // "interned"
      return "(" + pred_fmt + ")" + arrow + "(" + consq_fmt + ")";
    } else if (format == OutputFormat.JAVA) {
      String mid = (iff ? " == " : " || !"); // "interned"
      return "(" + consq_fmt + ")" + mid + "(" + pred_fmt + ")";
    } else if (format == OutputFormat.SIMPLIFY) {
      String cmp = (iff ? "IFF" : "IMPLIES");
      return "(" + cmp + " " + pred_fmt + " " + consq_fmt + ")";
    } else if (format == OutputFormat.DBCJAVA) {
      if (iff) {
        return "((" + pred_fmt + ") == (" + consq_fmt + "))";
      } else {
        return "(" + pred_fmt + " $implies " + consq_fmt + ")";
      }
    } else if (format == OutputFormat.CSHARPCONTRACT) {
      if (iff) {
        return "(" + pred_fmt + ") == (" + consq_fmt + ")";
      } else {
        return "(" + pred_fmt + ").Implies(" + consq_fmt + ")";
      }
    } else {
      return format_unimplemented(format);
    }
  }

  /*@Pure*/
  @Override
  public /*@Nullable*/ DiscardInfo isObviousStatically(VarInfo[] vis) {
    assert vis.length > 0;
    for (int ii = 0; ii < vis.length; ii++) {
      assert vis[ii] != null;
    }
    return orig_right.isObviousStatically(vis);
  }

  /*@Pure*/
  @Override
  public /*@Nullable*/ DiscardInfo isObviousDynamically(VarInfo[] vis) {
    assert vis.length > 0;
    for (int ii = 0; ii < vis.length; ii++) {
      assert vis[ii] != null;
    }
    DiscardInfo di = orig_right.isObviousDynamically(vis);
    if (di != null) {
      log("failed isObviousDynamically with vis = %s", VarInfo.arrayToString(vis));
      return di;
    }

    return null;
  }

  /**
   * Return true if the right side of the implication and some equality combinations of its member
   * variables are statically obvious. For example, if a == b, and f(a) is obvious, then so is f(b).
   * We use the someInEquality (or least interesting) method during printing so we only print an
   * invariant if all its variables are interesting, since a single, static, non interesting
   * occurance means all the equality combinations aren't interesting.
   *
   * <p>This must be overridden for Implication because the right side is the invariant of interest.
   * The standard version passes the vis from the slice containing the implication itself (slice 0).
   */
  /*@Pure*/
  @Override
  public /*@Nullable*/ DiscardInfo isObviousStatically_SomeInEquality() {
    return orig_right.isObviousStatically_SomeInEquality();
    //     DiscardInfo result = isObviousStatically (orig_right.ppt.var_infos);
    //     if (result != null) return result;
    //     assert orig_right.ppt.var_infos.length > 0;
    //     for (int ii = 0; ii < orig_right.ppt.var_infos.length; ii++ )
    //       assert orig_right.ppt.var_infos[ii] != null;
    //     return isObviousStatically_SomeInEqualityHelper (orig_right.ppt.var_infos,
    //                      new VarInfo[orig_right.ppt.var_infos.length], 0);
  }

  /**
   * Return true if the rightr side of the implication some equality combinations of its member
   * variables are dynamically obvious. For example, a == b, and f(a) is obvious, so is f(b). We use
   * the someInEquality (or least interesting) method during printing so we only print an invariant
   * if all its variables are interesting, since a single, dynamic, non interesting occurance means
   * all the equality combinations aren't interesting.
   *
   * <p>This must be overridden for Implication because the right side is the invariant of interest.
   * The standard version passes the vis from the slice containing the implication itself (slice 0).
   */
  /*@Pure*/
  @Override
  public /*@Nullable*/ DiscardInfo isObviousDynamically_SomeInEquality() {

    // If the consequent is ni-suppressed in its original program point,
    // then it is obvious from some set of other invariants.  Those invariants
    // could be other implications or they could be true at both conditional
    // points.
    // JHP: Seemingly it would be better if this invariant was never
    // created, but somehow that creates other implications.  See the
    // disabled code in PptSplitter.add_implication()
    if (orig_right.is_ni_suppressed()) {
      return (new DiscardInfo(
          this, DiscardCode.obvious, "consequent " + orig_right.format() + " is ni suppressed"));
    }

    return orig_right.isObviousDynamically_SomeInEquality();
    //     DiscardInfo result = isObviousDynamically (orig_right.ppt.var_infos);
    //     if (result != null)
    //       return result;
    //     return isObviousDynamically_SomeInEqualityHelper (orig_right.ppt.var_infos,
    //                                  new VarInfo[right.ppt.var_infos.length], 0);
  }

  /*@Pure*/
  @Override
  public boolean isSameFormula(/*@NonNull*/ Invariant other) {
    Implication other_implic = (Implication) other;
    return ((iff == other_implic.iff) && super.isSameFormula(other_implic));
  }

  /*@EnsuresNonNullIf(result=true, expression="#1")*/
  /*@Pure*/
  @Override
  public boolean isSameInvariant(Invariant other) {
    if (other == null) return false;
    if (!(other instanceof Implication)) return false;
    if (iff != ((Implication) other).iff) return false;
    return super.isSameInvariant(other);
  }

  // An implication is only interesting if both the predicate and
  // consequent are interesting
  /*@Pure*/
  @Override
  public boolean isInteresting() {
    return (predicate().isInteresting() && consequent().isInteresting());
  }

  // If a constant managed to appear in a predicate, that's
  // interesting enough for us.
  @Override
  public boolean hasUninterestingConstant() {
    return consequent().hasUninterestingConstant();
  }

  /*@Pure*/
  @Override
  public boolean isAllPrestate() {
    return predicate().isAllPrestate() && consequent().isAllPrestate();
  }

  /**
   * Logs a description of the invariant and the specified msg via the logger as described in {@link
   * daikon.Debug#log(Logger, Class, Ppt, VarInfo[], String)}. Uses the consequent as the logger.
   */
  @Override
  public void log(
      /*NOT: @UnknownInitialization(Implication.class) @Raw(Implication.class) Implication this,*/ Logger
          log,
      String msg) {

    right.log(
        log,
        msg
            + "[for implication "
            + format()
            + " ("
            + (orig_right == null ? "null" : orig_right.format())
            + ")]");
  }

  /**
   * Logs a description of the invariant and the specified msg via the logger as described in {@link
   * daikon.Debug#log(Logger, Class, Ppt, VarInfo[], String)}. Uses the consequent as the logger
   *
   * @return whether or not it logged anything
   */
  @Override
  /*@FormatMethod*/
  @SuppressWarnings({
    "override.receiver.invalid", // sound overriding, not expressible in Checker Framework
    "method.invocation.invalid", // call to format is OK
    "formatter"
  }) // call to format method is correct because of @FormatMethod annotation
  public boolean log(
      /*>>>@UnknownInitialization(Implication.class) @Raw(Implication.class) Implication this,*/ String
          format,
      /*@Nullable*/ Object... args) {
    String msg = format;
    if (args.length > 0) msg = String.format(format, args);
    return (right.log(
        msg
            + " [for implication "
            + format()
            + " ("
            + (orig_right == null ? "null" : orig_right.format())
            + ")]"));
  }

  @Override
  public boolean enabled(/*>>> @Prototype Implication this*/) {
    throw new Error("do not invoke " + getClass() + ".enabled()");
  }

  @Override
  public boolean valid_types(/*>>> @Prototype Implication this,*/ VarInfo[] vis) {
    throw new Error("do not invoke " + getClass() + ".valid_types()");
  }

  @Override
  protected /*@NonPrototype*/ Invariant instantiate_dyn(
      /*>>> @Prototype Implication this,*/ PptSlice slice) {
    throw new Error("do not invoke " + getClass() + ".instantiate_dyn()");
  }
}
