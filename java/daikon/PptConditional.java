package daikon;

import daikon.split.*;

// All information about a single program point.
// A Ppt may also represent just part of the data: a disjunction.
// This probably doesn't do any direct computation, instead deferring that
// to its views that are slices.

// This perhaps shouldn't extend PptTopLevel; fix that in the future.
// For now, it's convenient to take advantage of its functionality.
// And they're so similar that maybe this is the right thing after all.
public class PptConditional extends PptTopLevel {

  Ppt parent;
  Splitter splitter;
  // indicates whether we're on the true side or the false side of the Splitter
  boolean splitter_inverse;

  // This does not install the variable values.  The reason is that it's
  // more efficient to do that for two PptConditional objects at once.

  public PptConditional(PptTopLevel parent_, Splitter splitter_, boolean splitter_inverse_) {
    super(parent_.name, VarInfo.arrayclone_simple(parent_.trace_and_orig_vars()));
    parent = parent_;
    splitter = splitter_.instantiate(this);
    splitter_inverse = splitter_inverse_;
    String splitter_formatted = splitter.condition();
    if (splitter_inverse)
      splitter_formatted = "not(" + splitter_formatted + ")";
    name = name + ";condition=\"" + splitter_formatted + "\"";
  }

  public boolean splitter_valid() {
    return splitter.valid();
  }

  // I could do only one test per value when installing, because I know
  // these are complementary; or I could not do that special-casing.
  // Probably don't...

// I think that now i do this elsewhere, in PptTopLevel.addConditions
//   public PptConditional[] split(PptTopLevel ppt, Splitter splitter) {
//     PptConditional[] result = new PptConditional[2];
//     result[0] = new PptConditional(ppt, splitter, false);
//     result[1] = new PptConditional(ppt, splitter, true);
//
// *****;
//
//     return result;
//   }


  void add(ValueTuple vt, int count) {
    // This try block may be a very inefficient way to do this computation.
    // Perhaps figure out another way, or invalidate the whole PptConditional
    // if any exception is thrown.
    try {
      boolean splitter_test = splitter.test(vt);
      if (splitter_inverse ? (! splitter_test) : splitter_test)
        super.add(vt, count);
    } catch (Exception e) {
      // If an exception is thrown, don't put the data on either side
      // of the split.
    }
  }

  // I'd like to add a version that optimizes based on the observation that
  // PptConditional objects come in pairs, with splitters in inverted
  // senses.  But that observation is not strictly true, because a
  // splitter's internal computation could err, so it ought to return false
  // on both sides.  I'm ignoring that in the name of efficiency for the
  // nonce.


  // Call this for tuples that are guaranteed to pass the test.
  void add_nocheck(ValueTuple vt, int count) {
    super.add(vt, count);
  }


}
