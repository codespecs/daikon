package daikon.derive;

import daikon.*;

import utilMDE.*;

// Just a struct of two elements

// This is a temporary structure for grouping elements to be returned from
// computeValueAndModified, not for permanent storage.
public final class ValueAndModified {
  // The constructor checks that it is interned, contradicting this comment.
  public Object value;		// not necessarily an interned value
  public int modified;

  public final static ValueAndModified MISSING
    = new ValueAndModified(null, ValueTuple.MISSING);

  public ValueAndModified(Object val, int mod) {
    Assert.assert(Intern.isInterned(val));
    // Type should be Long, not Integer
    Assert.assert(! (val instanceof Integer));
    value = val;
    modified = mod;
  }
}
