package daikon.derive;

import daikon.*;

// Just a struct of two elements

// This is a temporary structure for grouping elements to be returned from
// computeValueAndModified, not for permanent storage.
public final class ValueAndModified {
  public Object value;		// not necessarily an interned value
  public int modified;

  public final static ValueAndModified MISSING
    = new ValueAndModified(null, ValueTuple.MISSING);

  public ValueAndModified(Object val, int mod) {
    value = val;
    modified = mod;
  }
}
