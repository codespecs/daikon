package daikon.diff;

import java.util.*;
import daikon.inv.*;

/**
 * Comparator for pairing invariants.  In an invariant in set2 is an
 * implication, its consequent is used instead of the whole invariant.
 * In set1, the whole invariant is always used.  Some examples:
 *
 * this.compare(A, B->A) == c.compare(A, A)
 * this.compare(C, D) == c.compare(C, D)
 **/
public class ConsequentPairComparator implements Comparator {

  private Comparator c;

  public ConsequentPairComparator(Comparator c) {
    this.c = c;
  }

  public int compare(Object o1, Object o2) {
    Invariant inv1, inv2;
    inv1 = (Invariant) o1;

    if (o2 instanceof Implication) {
      Implication imp2 = (Implication) o2;
      inv2 = imp2.consequent();
    } else {
      inv2 = (Invariant) o2;
    }

    return c.compare(inv1, inv2);
  }

}
