package daikon.diff;

import java.util.*;
import daikon.inv.*;


/**
 * Comparator for sorting invariants.  If an invariant is an
 * implication, its consequent is used instead of the whole invariant.
 * If the consequents of two invariants are equal, the predicates are
 * compared.  The predicates and consequents themselves are compared
 * using the Comparator c passed to the constructor.  Some examples:
 *
 * this.compare(A->B, A->C) == c.compare(B, C)
 * this.compare(B, A->C) == c.compare(B, C)
 * this.compare(B, C) == c.compare(B, C)
 * this.compare(A->C, B->C) == c.compare(A, B)
 **/
public class ConsequentSortComparator implements Comparator {

  private Comparator c;

  public ConsequentSortComparator(Comparator c) {
    this.c = c;
  }

  public int compare(Object o1, Object o2) {
    Invariant inv1, inv2;
    if (o1 instanceof Implication) {
      Implication imp1 = (Implication) o1;
      inv1 = imp1.consequent();
    } else {
      inv1 = (Invariant) o1;
    }
    if (o2 instanceof Implication) {
      Implication imp2 = (Implication) o2;
      inv2 = imp2.consequent();
    } else {
      inv2 = (Invariant) o2;
    }

    int result = c.compare(inv1, inv2);

    if (result == 0 &&
        o1 instanceof Implication && o2 instanceof Implication) {
      Implication imp1 = (Implication) o1;
      Implication imp2 = (Implication) o2;
      return c.compare(imp1.predicate(), imp2.predicate());
    } else {
      return result;
    }
  }

}
