package daikon.inv;

import daikon.PptSlice;
import java.util.*;

//  This class is used for displaying several equality Comparison invariants ("x == y", "y
//  == z") as one Equality invariant ("x == y == z").  This class is created after the
//  actual invariant detection, and right before printing (eg, during the GUI stage).
//  Hence this is not a real invariant class; it does not implement many of the methods
//  that most invariant classes do.  Furthermore, calling arbitrary methods on this class
//  may not work.

public final class Equality extends Invariant {

  Set variables;
  String invariantText;		// Looks like "x == y == z".

  public Equality( Set variables, PptSlice ppt ) {
    super( ppt );		// Need ppt because it reports num_values() and num_samples().
    this.variables = variables;

    //  Might as well construct the string here.
    Iterator iter = variables.iterator();
    StringBuffer invariantText = new StringBuffer( (String) iter.next());
    while (iter.hasNext())
      invariantText.append( " == " + (String) iter.next());
    this.invariantText = invariantText.toString();
  }

  //  Here is my rationale for always returning 0.  This Equality invariant aggregates
  //  several Comparison invariants who passed the IsEqualityComparison.it.accept() test.
  //  IsEqualityComparison.it.accept() checks if getProbability() returns less than
  //  Invariant.probability_limit, which is currently .01.  That means the probability for
  //  any of the involved Comparison invariants is at most .01.  In practice, the
  //  probability is always almost 0, and .01 is close to 0, so just report 0.

  public double computeProbability() { return 0; }

  public String repr() { return invariantText; }

  public String format() { return invariantText; }

  //  I will deal with this when the time comes.  I'll need to talk to Jeremy about what
  //  this method would return.
  public String format_esc() {
    throw new Error( "Equality.format_esc(): this method should not be called" );
  }

  public String format_simplify() {
    throw new Error( "Equality.format_simplify(): this method should not be called" );
  }

  //  This method isn't going to be called, but it's declared abstract in Invariant.
  public boolean isSameFormula( Invariant other ) {
    throw new Error( "Equality.isSameFormula(): this method should not be called" );
  }
}












