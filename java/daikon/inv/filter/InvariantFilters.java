package daikon.inv.filter;

import java.util.*;
import daikon.inv.*;

public class InvariantFilters {
    //  These id numbers are stored in HashMap's and such, so it's
    //  convenient for them to be String's rather than int's.
    public static final int UNJUSTIFIED_FILTER = 0;
    public static final int OBVIOUS_FILTER = 1;
    public static final int FEW_MODIFIED_SAMPLES_FILTER = 2;
    public static final int NON_CANONICAL_VARIABLES_FILTER = 3;
    public static final int ONLY_CONSTANT_VARIABLES_FILTER = 4;
    public static final int IMPLIED_POSTCONDITION_FILTER = 5;

    List filters = new ArrayList();
    
    public InvariantFilters() {
	filters.add( InvariantFilters.UNJUSTIFIED_FILTER,             new UnjustifiedFilter());
	filters.add( InvariantFilters.OBVIOUS_FILTER,                 new ObviousFilter());
	filters.add( InvariantFilters.FEW_MODIFIED_SAMPLES_FILTER,    new FewModifiedSamplesFilter());
	filters.add( InvariantFilters.NON_CANONICAL_VARIABLES_FILTER, new NonCanonicalVariablesFilter());
	filters.add( InvariantFilters.ONLY_CONSTANT_VARIABLES_FILTER, new OnlyConstantVariablesFilter());
	filters.add( InvariantFilters.IMPLIED_POSTCONDITION_FILTER,   new ImpliedPostconditionFilter());
    }

    public boolean shouldKeep( Invariant invariant ) {
	for (Iterator iter = filters.iterator(); iter.hasNext(); ) {
	    InvariantFilter filter = (InvariantFilter) iter.next();
	    if (filter.shouldDiscard( invariant )) {
		System.out.println( filter.getClass().getName() + " rules out    \t" + invariant.format());
		return false;
	    }
	}
	return true;
    }

    public void changeFilterSetting( int filterID, boolean turnOn ) {
	InvariantFilter filter = (InvariantFilter) filters.get( filterID );
	if (turnOn)
	    filter.turnOn();
	else
	    filter.turnOff();
    }

    public void turnFiltersOn() {
	for (Iterator iter = filters.iterator(); iter.hasNext(); ) {
	    InvariantFilter filter = (InvariantFilter) iter.next();
	    filter.turnOn();
	}
    }

    public void turnFiltersOff() {
	for (Iterator iter = filters.iterator(); iter.hasNext(); ) {
	    InvariantFilter filter = (InvariantFilter) iter.next();
	    filter.turnOff();
	}
    }
}



abstract class InvariantFilter {
    boolean isOn;

    public InvariantFilter( boolean isOn ) {
	this.isOn = isOn;
    }

    public InvariantFilter() {	// TODO:  This is a hack.  Should add constructors that take a boolean
	this( true );		// for every subclass.
    }

    public void turnOn()  { isOn = true; }
    public void turnOff() { isOn = false; }
    
    public boolean shouldDiscard( Invariant invariant ) {
	if (! isOn)
	    return false;
	else
	    return shouldDiscardInvariant( invariant );
    }
    
    abstract boolean shouldDiscardInvariant( Invariant invariant );
}

class UnjustifiedFilter extends InvariantFilter {
    boolean shouldDiscardInvariant( Invariant invariant ) {
	return invariant.isUnjustified();
    }
}

class ObviousFilter extends InvariantFilter {
    boolean shouldDiscardInvariant( Invariant invariant ) {
	return invariant.isObvious();
    }
}

class FewModifiedSamplesFilter extends InvariantFilter {
    boolean shouldDiscardInvariant( Invariant invariant ) {
	return invariant.hasFewModifiedSamples();
    }
}

class NonCanonicalVariablesFilter extends InvariantFilter {
    boolean shouldDiscardInvariant( Invariant invariant ) {
	return invariant.hasNonCanonicalVariables();
    }
}

class OnlyConstantVariablesFilter extends InvariantFilter {
    boolean shouldDiscardInvariant( Invariant invariant ) {
	return invariant.hasOnlyConstantVariables();
    }
}

class ImpliedPostconditionFilter extends InvariantFilter {
    boolean shouldDiscardInvariant( Invariant invariant ) {
	return invariant.isImpliedPostcondition();
    }
}




