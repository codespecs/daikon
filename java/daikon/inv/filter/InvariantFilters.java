package daikon.inv.filter;

import java.util.*;
import daikon.inv.*;
import daikon.inv.IsEqualityComparison;	       // For equality invariants work-around
import daikon.PptSlice;			       // For equality invariants work-around
import daikon.VarInfo;

public class InvariantFilters {
    // ID numbers for property filters
    public static final int NON_CANONICAL_VARIABLES_FILTER = 0;// Do this check first, since it's fast
    public static final int UNJUSTIFIED_FILTER = 1;
    public static final int OBVIOUS_FILTER = 2;
    public static final int FEW_MODIFIED_SAMPLES_FILTER = 3;
    public static final int ONLY_CONSTANT_VARIABLES_FILTER = 4;
    public static final int IMPLIED_POSTCONDITION_FILTER = 5;
    public static final int CONTROLLED_INVARIANT_FILTER = 6;

    public static final int ANY_VARIABLE = 10;
    public static final int ALL_VARIABLES = 11;
    int variableFilterType = ANY_VARIABLE;

    List propertyFilters = new ArrayList();
    List variableFilters = new ArrayList();
    
    public InvariantFilters() {
	propertyFilters.add( InvariantFilters.NON_CANONICAL_VARIABLES_FILTER, new NonCanonicalVariablesFilter());
	propertyFilters.add( InvariantFilters.UNJUSTIFIED_FILTER,             new UnjustifiedFilter());
	propertyFilters.add( InvariantFilters.OBVIOUS_FILTER,                 new ObviousFilter());
	propertyFilters.add( InvariantFilters.FEW_MODIFIED_SAMPLES_FILTER,    new FewModifiedSamplesFilter());
	propertyFilters.add( InvariantFilters.ONLY_CONSTANT_VARIABLES_FILTER, new OnlyConstantVariablesFilter());
	propertyFilters.add( InvariantFilters.IMPLIED_POSTCONDITION_FILTER,   new ImpliedPostconditionFilter());

	// This filter should be added last for speed, because its shouldDiscard() is more complicated
	// in that it evaluates shouldDiscard() for other invariants.
	propertyFilters.add( InvariantFilters.CONTROLLED_INVARIANT_FILTER,    new ControlledInvariantFilter());
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

  //  I wasn't sure where to put this method, but this class seems like the best place.
  //  This function takes a list of invariants, finds the equality Comparison invariants
  //  (x==y, y==z), and replaces them with Equality invariants (x==y==z).  The Equality
  //  invariants are inserted into the beginning.  These Equality invariants are useful
  //  when it comes to displaying invariants.
  public static List addEqualityInvariants( List invariants ) {
    Set equivalentSets = new HashSet();	       // A set of Set's of equivalent variables
    List ppts = new ArrayList();	       // A PptSlice for each set.  Equality needs a PptSlice
					       // so it can report num_values() and num_samples().
    
    // Find all equivalent sets of variables.
    for (Iterator iter = invariants.iterator(); iter.hasNext(); ) {
      Invariant invariant = (Invariant) iter.next();
      if (IsEqualityComparison.it.accept( invariant )) {
	iter.remove();			       // We don't need this invariant, since it will be included
					       // in the equality invariant.
	boolean inEquivalentSets = false;      // Are either of the variables in an existing set?
	String variable1 = ((Comparison) invariant).var1().name.name();
	String variable2 = ((Comparison) invariant).var2().name.name();

	for (Iterator iter2 = equivalentSets.iterator(); iter2.hasNext(); ) {
	  Set equivalentSet = (Set) iter2.next();
	  boolean containsVariable1 = equivalentSet.contains( variable1 );
	  boolean containsVariable2 = equivalentSet.contains( variable2 );
	  if (containsVariable1 || containsVariable2)
	    inEquivalentSets = true;
	  if (containsVariable1 && !containsVariable2)
	    equivalentSet.add( variable2 );
	  if (containsVariable2 && !containsVariable1)
	    equivalentSet.add( variable1 );
	}
	if (! inEquivalentSets) {
	  Set set = new HashSet();
	  set.add( variable1 );
	  set.add( variable2 );
	  equivalentSets.add( set );
	  ppts.add( invariant.ppt );
	}
      }
    }

    // Sometimes we'll end up with two sets that are equivalent -- ie, they contain the
    // same variables.  This can happen when there are more than three variables involved.
    // Say we have "a == b", "b == c", "c == d".  If we encounter the first and the third
    // invariants first, they will be put into two seperate sets.  Each set will develop
    // independently and end up having a, b, c, and d.
    // To get around this we create a new HashSet, in which the constructor adds each set
    // one-by-one.  A set will not be added if equals a previously added set.  (Two sets
    // are equal if they contain the same elements.)
    equivalentSets = new HashSet( equivalentSets );

    // Add equivalent sets as equivalent invariants.
    Iterator pptIter = ppts.iterator();
    for (Iterator iter = equivalentSets.iterator(); iter.hasNext(); )
      invariants.add( 0, new Equality( (Set) iter.next(), (PptSlice) pptIter.next()));
 
    return invariants;
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

class NonCanonicalVariablesFilter extends InvariantFilter {
    //  We should discard this invariant only if it has non-canonical variables AND it is
    //  not an equality Comparison invariant.  We need to keep equality Comparison
    //  invariants so that later on, Equality invariants will be made out of them.
    boolean shouldDiscardInvariant( Invariant invariant ) {
      return (invariant.hasNonCanonicalVariable() && ! IsEqualityComparison.it.accept(invariant));
    }
}

class UnjustifiedFilter extends InvariantFilter {
    boolean shouldDiscardInvariant( Invariant invariant ) {
	return ! invariant.justified();
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

class ControlledInvariantFilter extends InvariantFilter {
    boolean shouldDiscardInvariant( Invariant invariant ) {
//  	Invariant controllingInvariant = inv.find_controlling_invariant();
//  	while (controllingInvariant != null) {
//  	    if (shouldKeep( controllingInvariant ))
//  		return true;
//  	    controllingInvariant = controllingInvariant.find_controlling_invariant();
//  	}
	return false;
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
	 if (invariant.usesVar( variable ))
	    return false;
	else
	    return true;
    }
}
