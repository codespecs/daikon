package daikon.diff;

import daikon.inv.*;
import java.util.*;

/*>>>
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Comparator for pairing invariants. In an invariant in set2 is an implication, its consequent is
 * used instead of the whole invariant. In set1, the whole invariant is always used. Some examples:
 *
 * <pre>
 * this.compare(A, B&rArr;A) == c.compare(A, A)
 * this.compare(C, D) == c.compare(C, D)
 * </pre>
 */
public class ConsequentPairComparator implements Comparator<Invariant> {

  private Comparator<Invariant> c;

  public ConsequentPairComparator(Comparator<Invariant> c) {
    this.c = c;
  }

  /*@Pure*/
  @Override
  public int compare(Invariant inv1, Invariant inv2) {
    if (inv2 instanceof Implication) {
      Implication imp2 = (Implication) inv2;
      inv2 = imp2.consequent();
    }

    return c.compare(inv1, inv2);
  }
}
