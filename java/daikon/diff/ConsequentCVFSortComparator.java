package daikon.diff;

import java.util.*;
import daikon.inv.*;

/**
 * Comparator for sorting invariants.  Uses the
 * ConsequentSortComparator, initialized with the
 * ClassVarnameFormulaComparator.  See the documentation for those two
 * classes to figure out what this class does.
 **/
public class ConsequentCVFSortComparator implements Comparator {
  private Comparator c = new ConsequentSortComparator
    (new Invariant.ClassVarnameFormulaComparator());

  public int compare(Object o1, Object o2) {
    return c.compare(o1, o2);
  }
}
