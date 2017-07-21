package daikon.diff;

import daikon.inv.*;
import java.util.*;

/*>>>
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Comparator for sorting invariants. Uses the ConsequentSortComparator, initialized with the
 * ClassVarnameFormulaComparator. See the documentation for those two classes to figure out what
 * this class does.
 */
public class ConsequentCVFSortComparator implements Comparator<Invariant> {
  private Comparator<Invariant> c =
      new ConsequentSortComparator(new Invariant.ClassVarnameFormulaComparator());

  /*@Pure*/
  @Override
  public int compare(Invariant inv1, Invariant inv2) {
    return c.compare(inv1, inv2);
  }
}
