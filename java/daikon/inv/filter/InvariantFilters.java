package daikon.inv.filter;

import java.util.*;
import daikon.inv.*;
import daikon.VarInfo;

public class InvariantFilters {
    // ID numbers for property filters
    public static final int UNJUSTIFIED_FILTER = 0;
    public static final int OBVIOUS_FILTER = 1;
    public static final int FEW_MODIFIED_SAMPLES_FILTER = 2;
    public static final int NON_CANONICAL_VARIABLES_FILTER = 3;
    public static final int ONLY_CONSTANT_VARIABLES_FILTER = 4;
    public static final int IMPLIED_POSTCONDITION_FILTER = 5;

    public static final int ANY_VARIABLE = 10;
    public static final int ALL_VARIABLES = 11;
    int variableFilterType = ANY_VARIABLE;

    List propertyFilters = new ArrayList();
    List variableFilters = new ArrayList();
    
    public InvariantFilters() {
	propertyFilters.add( InvariantFilters.UNJUSTIFIED_FILTER,             new UnjustifiedFilter());
	propertyFilters.add( InvariantFilters.OBVIOUS_FILTER,                 new ObviousFilter());
	propertyFilters.add( InvariantFilters.FEW_MODIFIED_SAMPLES_FILTER,    new FewModifiedSamplesFilter());
	propertyFilters.add( InvariantFilters.NON_CANONICAL_VARIABLES_FILTER, new NonCanonicalVariablesFilter());
	propertyFilters.add( InvariantFilters.ONLY_CONSTANT_VARIABLES_FILTER, new OnlyConstantVariablesFilter());
	propertyFilters.add( InvariantFilters.IMPLIED_POSTCONDITION_FILTER,   new ImpliedPostconditionFilter());
    }

    public void addVariableFilter( String variable ) {
	variableFilters.add( new VariableFilter( variable ));
    }

    public void removeVariableFilter( String variable ) {
	for (Iterator iter = variableFilters.iterator(); iter.hasNext(); ) {
	    if (((VariableFilter) iter.next()).getVariable().equals( variable )) {
		iter.remove();
		return;
	    }
	}
	throw new Error( "InvariantFilters.removeVariableFilter():  filter for variable '" + variable + "' not found" );
    }

    public void setVariableFilterType( int variableFilterType ) {
	this.variableFilterType = variableFilterType;
    }
    
    public boolean shouldKeep( Invariant invariant ) {
	//  Do variable filters first since they eliminate more invariants.
	if (variableFilters.size() != 0) {
	    if (variableFilterType == InvariantFilters.ANY_VARIABLE) {
		boolean hasAnyVariable = false;
		for (Iterator iter = variableFilters.iterator(); iter.hasNext(); ) {
		    InvariantFilter filter = (InvariantFilter) iter.next();
		    if (! filter.shouldDiscard( invariant ))
			hasAnyVariable = true;
		}
		if (! hasAnyVariable)
		    return false;
	    } else if (variableFilterType == InvariantFilters.ALL_VARIABLES) {
		for (Iterator iter = variableFilters.iterator(); iter.hasNext(); ) {
		    InvariantFilter filter = (InvariantFilter) iter.next();
		    if (filter.shouldDiscard( invariant ))
			return false;
		}
	    }
	}
	//  Property filters.
	for (Iterator iter = propertyFilters.iterator(); iter.hasNext(); ) {
	    InvariantFilter filter = (InvariantFilter) iter.next();
	    if (filter.shouldDiscard( invariant )) {
		//		System.out.println( filter.getClass().getName() + " rules out    \t" + invariant.format());
		return false;
	    }
	}
	return true;
    }

    public void changeFilterSetting( int filterID, boolean turnOn ) {
	InvariantFilter filter = (InvariantFilter) propertyFilters.get( filterID );
	if (turnOn)
	    filter.turnOn();
	else
	    filter.turnOff();
    }

    public void turnFiltersOn() {
	for (Iterator iter = propertyFilters.iterator(); iter.hasNext(); ) {
	    InvariantFilter filter = (InvariantFilter) iter.next();
	    filter.turnOn();
	}
    }

    public void turnFiltersOff() {
	for (Iterator iter = propertyFilters.iterator(); iter.hasNext(); ) {
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



class VariableFilter extends InvariantFilter {
    String variable;

    public VariableFilter( String variable ) {
	this.variable = variable;
    }

    public String getVariable() {
	return variable;
    }

    boolean shouldDiscardInvariant( Invariant invariant ) {
	 if (invariant.ppt.usesVar( variable ))
	    return false;
	else
	    return true;
    }
}
