package daikon.inv.filter;

import java.util.*;
import daikon.inv.*;

public class InvariantFilters {
    //  These id numbers are stored in HashMap's and such, so it's
    //  convenient for them to be String's rather than int's.
    public static final String UNJUSTIFIED_FILTER = "0";
    public static final String OBVIOUS_FILTER = "1";
    public static final String FEW_MODIFIED_SAMPLES_FILTER = "2";
    public static final String NON_CANONICAL_VARIABLES_FILTER = "3";
    public static final String ONLY_CONSTANT_VARIABLES_FILTER = "4";

    HashMap filters = new HashMap();
    
    public InvariantFilters() {
	filters.put( InvariantFilters.UNJUSTIFIED_FILTER,             new UnjustifiedFilter());
	filters.put( InvariantFilters.OBVIOUS_FILTER,                 new ObviousFilter());
	filters.put( InvariantFilters.FEW_MODIFIED_SAMPLES_FILTER,    new FewModifiedSamplesFilter());
	filters.put( InvariantFilters.NON_CANONICAL_VARIABLES_FILTER, new NonCanonicalVariablesFilter());
	filters.put( InvariantFilters.ONLY_CONSTANT_VARIABLES_FILTER, new OnlyConstantVariablesFilter());
    }

    public boolean shouldKeep( Invariant invariant ) {
	for (Iterator iter = filters.values().iterator(); iter.hasNext(); ) {
	    InvariantFilter filter = (InvariantFilter) iter.next();
	    if (filter.shouldDiscard( invariant )) {
		System.out.println( filter.getClass().getName() + " rules out    \t" + invariant.format());
		return false;
	    }
	}
	return true;
    }
    
    public void changeFilterSetting( String filterID, boolean turnOn ) {
	InvariantFilter filter = (InvariantFilter) filters.get( filterID );
	if (turnOn)
	    filter.turnOn();
	else
	    filter.turnOff();
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




