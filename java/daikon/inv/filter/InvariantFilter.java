package daikon.inv.filter;

import daikon.inv.*;

//  The template for an invariant filter.  Groups of invariant filters are managed by
//  InvariantFilters.
public abstract class InvariantFilter {
  boolean isOn;

  public InvariantFilter( boolean isOn ) {
    this.isOn = isOn;
  }

  public InvariantFilter() {	// TODO:  This is a hack.  Should add constructors that take a boolean
    this( true );		// for every subclass.
  }

  abstract public String getDescription();

  public void turnOn()  { isOn = true; }
  public void turnOff() { isOn = false; }

  public boolean getSetting() {
    return isOn;
  }

  public boolean shouldDiscard( Invariant invariant ) {
    if (! isOn)
      return false;
    else
      return shouldDiscardInvariant( invariant );
  }

  abstract boolean shouldDiscardInvariant( Invariant invariant );
}










