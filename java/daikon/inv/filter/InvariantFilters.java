package daikon.inv.filter;

import utilMDE.Assert;
import java.util.*;
import daikon.inv.*;
import daikon.inv.IsEqualityComparison;	       // For equality invariants work-around
import daikon.PptSlice;			       // For equality invariants work-around
import daikon.VarInfo;

//  This class contains a collection of invariant filters, and allows other
//  code to perform invariant filtering.  To filter invariants, do the
//  following:
//     o  Instantiate an InvariantFilters object.
//     o  At any time, adjust the filters as necessary using the public methods.
//     o  Call:  invariantFilters.shouldKeep( invariant );
//
//  There are two main kinds of filters: property filters and variable
//  filters.  Property filters attempt to eliminate irrelevant invariants,
//  and are all turned on by default.  Variable filters only keep
//  invariants which contain all or any of a set of variables (depending on
//  variableFilterType).  There are no variable filters by default.  See
//  the manual for more information on property and variable filters.

public class InvariantFilters {
  public static final int ANY_VARIABLE = 1;
  public static final int ALL_VARIABLES = 2;
  int variableFilterType = ANY_VARIABLE;

  // propertyFilters is a map from filter description to filter object.  We
  // need this mapping so that the GUI can easily tell InvariantFilters --
  // by passing in a filter description -- which filter was de/selected.
  // Use TreeMap to preserve order of filters (eg, so that
  // ControlledInvariantFilter will always be last).
  Map propertyFilters = new TreeMap();
  List variableFilters = new ArrayList();

  public InvariantFilters() {
    addPropertyFilter( (InvariantFilter) new NonCanonicalVariablesFilter());
    addPropertyFilter( (InvariantFilter) new UnjustifiedFilter());
    addPropertyFilter( (InvariantFilter) new ObviousFilter());
    addPropertyFilter( (InvariantFilter) new FewModifiedSamplesFilter());
    addPropertyFilter( (InvariantFilter) new OnlyConstantVariablesFilter());
    addPropertyFilter( (InvariantFilter) new ImpliedPostconditionFilter());

    // This filter should be added last for speed, because its
    // shouldDiscard() is more complicated in that it evaluates
    // shouldDiscard() for other invariants.
    //    ControlledInvariantFilter filter7 = new ControlledInvariantFilter( this );
    //    propertyFilters.put( filter7.getDescription(), filter7 );
    addPropertyFilter( (InvariantFilter) new ControlledInvariantFilter( this ));
  }

  void addPropertyFilter( InvariantFilter filter ) {
    propertyFilters.put( filter.getDescription(), filter );
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
    for (Iterator iter = propertyFilters.values().iterator(); iter.hasNext(); ) {
      InvariantFilter filter = (InvariantFilter) iter.next();
      if (filter.shouldDiscard( invariant )) {
	//		System.out.println( filter.getClass().getName() + " rules out    \t" + invariant.format());
	return false;
      }
    }
    return true;
  }

  public Iterator getPropertyFiltersIterator() {
    return propertyFilters.values().iterator();
  }

  public void changeFilterSetting( String description, boolean turnOn ) {
    InvariantFilter filter = (InvariantFilter) propertyFilters.get( description );
    if (turnOn)
      filter.turnOn();
    else
      filter.turnOff();
  }

  public void turnFiltersOn() {
    for (Iterator iter = propertyFilters.values().iterator(); iter.hasNext(); ) {
      InvariantFilter filter = (InvariantFilter) iter.next();
      filter.turnOn();
    }
  }

  public void turnFiltersOff() {
    for (Iterator iter = propertyFilters.values().iterator(); iter.hasNext(); ) {
      InvariantFilter filter = (InvariantFilter) iter.next();
      filter.turnOff();
    }
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

  // variableFilterType is either InvariantFilters.ANY_VARIABLE or InvariantFilters.ALL_VARIABLES
  public void setVariableFilterType( int variableFilterType ) {
    this.variableFilterType = variableFilterType;
  }

  //  I wasn't sure where to put this method, but this class seems like the
  //  best place.  Equality invariants only exist to make invariant output
  //  more readable, so this shouldn't be in the main Daikon engine code.
  //  Equality invariants aren't *directly* related to filtering, but their
  //  existence allows us to filter out certain invariants containing
  //  non-canonical variables ("x=y", "x=z", etc).  Also, I am hesitant to
  //  put code dealing with the internal workings of invariants/daikon in
  //  the GUI package.  Therefore, I put the method here rather than in
  //  InvariantsGUI.java.

  //  This function takes a list of invariants, finds the equality
  //  Comparison invariants (x==y, y==z), and deletes and replaces them
  //  with Equality invariants (x==y==z).  The first variable in an
  //  Equality invariant is always the canonical variable of the group.
  //  The Equality invariants are inserted into the beginning.  Equality
  //  invariants are useful when it comes to displaying invariants.
  public static List addEqualityInvariants( List invariants ) {

    // A set of groups of equivalent variables.  The "groups" are actually
    // List's.  We use List's instead of Set's because we need to preserve
    // order, so that canonical variables remain first.
    Set equivalentGroups = new HashSet();

    // A PptSlice for each set.  Equality needs a PptSlice so it can report
    // num_values() and num_samples().
    List ppts = new ArrayList();

    // This method makes two passes through the list of invariants.  The
    // first pass is to set up a group for each canonical variable.  The
    // second pass fills up each group with equivalent variables.  The main
    // advantage of doing the initial first pass is that canonical
    // variables will at the beginning of the List, and thus will be
    // displayed first in the output "x==y==z".  A secondary advantage is
    // that we don't run into the following problem:  Say we have "a == b",
    // "b == c", "c == d".  If we encounter the first and the third
    // invariants first, they will be put into two seperate sets.  Each set
    // will develop independently and end up having a, b, c, d.

    // First pass: set up a group for each canonical variable.  First,
    // construct the Set canonicalVariables.  The advantage of using the
    // Set class is that duplicates are taken care of (we might see a
    // canonical variable more than once).  Second, for each element of
    // canonicalVariables, add a List to equivalentGroups.
    Set canonicalVariables = new HashSet();
    for (Iterator iter = invariants.iterator(); iter.hasNext(); ) {
      Invariant invariant = (Invariant) iter.next();
      if (IsEqualityComparison.it.accept( invariant )) {
	VarInfo[] variables = invariant.ppt.var_infos;
	Assert.assert( variables.length == 2 );
	for (int i = 0; i < variables.length; i++)
	  if (variables[i].isCanonical()) {
	    canonicalVariables.add( variables[i].name.name());
	    ppts.add( invariant.ppt );
	  }
      }
    }
    for (Iterator iter = canonicalVariables.iterator(); iter.hasNext(); ) {
      List list = new ArrayList();
      list.add( iter.next());
      equivalentGroups.add( list );
    }

    // Second pass: fill up each group with equivalent variables.
    for (Iterator iter = invariants.iterator(); iter.hasNext(); ) {
      Invariant invariant = (Invariant) iter.next();
      if (IsEqualityComparison.it.accept( invariant )) {
	iter.remove();		// We don't need this invariant, since it will be
				// included in the equality invariant.

	String variable1 = ((Comparison) invariant).var1().name.name();
	String variable2 = ((Comparison) invariant).var2().name.name();
	for (Iterator iter2 = equivalentGroups.iterator(); iter2.hasNext(); ) {
	  List equivalentGroup = (List) iter2.next();
	  if (equivalentGroup.contains( variable1 )
              &&  ! equivalentGroup.contains( variable2 ))
	    equivalentGroup.add( variable2 );
	  else if (equivalentGroup.contains( variable2 )
                   &&  ! equivalentGroup.contains( variable1 ))
	    equivalentGroup.add( variable1 );
	}
      }
    }

    // Add equivalent groups as equality invariants.
    Iterator pptIter = ppts.iterator();
    for (Iterator iter = equivalentGroups.iterator(); iter.hasNext(); )
      invariants.add( 0, new Equality( (List) iter.next(), (PptSlice) pptIter.next()));

    return invariants;
  }
}


