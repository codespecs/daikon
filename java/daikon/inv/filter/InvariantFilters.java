package daikon.inv.filter;

import utilMDE.Assert;
import java.util.logging.Handler;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;
import daikon.inv.IsEqualityComparison;        // For equality invariants work-around
import daikon.inv.Invariant.OutputFormat;
import daikon.PptMap;
import daikon.PptSlice;
import daikon.PptTopLevel;
import daikon.VarInfo;
import daikon.PrintInvariants;
import daikon.Daikon;

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

  // annoyingly, this doesn't actually do what the original author
  // intended.  TreeMap orders things based on the keys, which in this
  // case is the description of the filter (a string).  What we'd
  // actually like is to order them in order of something like
  // [probability of eliminating an inv]/[expected running time]...in
  // other words, based on a benefit to cost measurement.  hence, this
  // will become a list (in particular a Vector).  This does increase
  // the running time of lookups based on the descriptions from O(log
  // n) to O(n), but that functionality isn't used a whole lot and
  // there are only ~10 filters anyway.

  List propertyFilters = new Vector();
  List variableFilters = new ArrayList();

  // Use public methods {set,get}PptMap to access, if necessary.
  PptMap ppt_map = null;

  public InvariantFilters() {
    if (Daikon.output_style == OutputFormat.JML) {
      addPropertyFilter( new JMLCompilerWorkaroundFilter());
    }

    if (Daikon.output_style == OutputFormat.ESCJAVA) {
      addPropertyFilter( new UnmodifiedVariableEqualityFilter());
    }

    addPropertyFilter( new SuppressionFilter());
    addPropertyFilter( new NonCanonicalVariablesFilter());
    addPropertyFilter( new DerivedParameterFilter());
    addPropertyFilter( new UnjustifiedFilter());
    addPropertyFilter( new ObviousFilter());
    addPropertyFilter( new FewModifiedSamplesFilter());
    addPropertyFilter( new OnlyConstantVariablesFilter());
    // UninterestingConstantFilter is turned off for the moment, since
    // without a static check it's too strong, not to mention being a
    // behavior change.
    //    addPropertyFilter( new UninterestingConstantFilter());
    addPropertyFilter( new ImpliedPostconditionFilter());
    //    addPropertyFilter( new RedundantFilter());
    addPropertyFilter( new SimplifyFilter( this ));
    addPropertyFilter( new ObviousEqualityFilter());

    // This filter should be added last for speed, because its shouldDiscard()
    // is more complicated in that it evaluates shouldDiscard() for other
    // invariants.
    if (Daikon.suppress_implied_controlled_invariants)
      addPropertyFilter( new ControlledInvariantFilter());
  }

  protected InvariantFilters(List l) {
    for (Iterator iter = l.iterator(); iter.hasNext(); ) {
      InvariantFilter filter = (InvariantFilter)iter.next();
      addPropertyFilter(filter);
    }
  }

  public static InvariantFilters emptyFilter() {
    return new InvariantFilters(new Vector());
  }

  public static InvariantFilters isWorthPrintingFilter_sansControlledCheck() {
    Vector v = new Vector();
    v.add(new FewModifiedSamplesFilter());
    v.add(new EnoughSamplesFilter());
    v.add(new NonCanonicalVariablesFilter());
    v.add(new ObviousFilter());
    v.add(new UnjustifiedFilter());
    v.add(new ImpliedPostconditionFilter());
    v.add(new OnlyConstantVariablesFilter());
    // v.add(new UninterestingConstantFilter());
    // v.add(new DerivedParameterFilter());
    return (new InvariantFilters(v));
  }

  public static InvariantFilters isWorthPrintingFilter() {
    Vector v = new Vector();
    v.add(new FewModifiedSamplesFilter());
    v.add(new EnoughSamplesFilter());
    v.add(new NonCanonicalVariablesFilter());
    v.add(new ObviousFilter());
    v.add(new UnjustifiedFilter());
    v.add(new ImpliedPostconditionFilter());
    v.add(new OnlyConstantVariablesFilter());
    //v.add(new UninterestingConstantFilter());
    if (Daikon.suppress_implied_controlled_invariants)
      v.add(new ControlledInvariantFilter());
    return (new InvariantFilters(v));
  }

  /**
   * Set the PptMap that the filters are being applied to.
   **/
  public void setPptMap(PptMap ppt_map) {
    this.ppt_map = ppt_map;
  }

  /**
   * @return the PptMap that the filters are being applied to.
   **/
  public PptMap getPptMap() {
    return ppt_map;
  }

  void addPropertyFilter( InvariantFilter filter ) {
    propertyFilters.add( filter );
  }

  public boolean shouldKeep( Invariant invariant ) {
    Logger df = PrintInvariants.debugFiltering;

    if (invariant.logOn() || df.isLoggable(Level.FINE)) {
      invariant.log (df, "filtering");
    }

    if (invariant instanceof GuardingImplication) {
      invariant = ((Implication) invariant).right;
    }

    // Keep track of old codes so that if it passes this test
    // we can set it back to prevent undesirable side effects
    // from the filters
    DiscardInvariant oldCode = invariant.discardCode;
    String oldString = invariant.discardString;

    //  Do variable filters first since they eliminate more invariants.
    if (variableFilters.size() != 0) {
      if (variableFilterType == InvariantFilters.ANY_VARIABLE) {
        boolean hasAnyVariable = false;
        for (Iterator iter = variableFilters.iterator(); iter.hasNext(); ) {
          InvariantFilter filter = (InvariantFilter) iter.next();
          if (! filter.shouldDiscard( invariant )) {
            hasAnyVariable = true;
          }
        }
        if (! hasAnyVariable) {
          if (invariant.logOn())
            invariant.log ("Failed ANY_VARIABLE filter");
            return false;
        }
      } else if (variableFilterType == InvariantFilters.ALL_VARIABLES) {
        for (Iterator iter = variableFilters.iterator(); iter.hasNext(); ) {
          InvariantFilter filter = (InvariantFilter) iter.next();
          if (filter.shouldDiscard( invariant )) {
            if (invariant.logOn())
              invariant.log ("Failed ALL_VARIABLES filter"
                             + filter.getClass().getName());
              return false;
          }
        }
      }
    }
    // If it made it this far, get rid of the side effects from testing it against
    // variable filters
    invariant.discardCode = oldCode;
    invariant.discardString = oldString;

    //  Property filters.
    invariant.log ("Processing " + propertyFilters.size() + " Prop filters");
    for (Iterator iter = propertyFilters.iterator(); iter.hasNext(); ) {
      InvariantFilter filter = (InvariantFilter) iter.next();
      if (invariant.logDetail() || df.isLoggable(Level.FINE)) {
        invariant.log (df, "applying " + filter.getClass().getName());
      }
      if (filter.shouldDiscard( invariant )) {
        if (invariant.logOn() || df.isLoggable(Level.FINE))
          invariant.log (df, "failed " + filter.getClass().getName());
        return false;
      }
    }
    if (df.isLoggable(Level.FINE)) {
      invariant.log (df, "accepted by InvariantFilters");
    }
    // Doing this since the filters can side effect desirable Invariants
    invariant.discardCode = DiscardInvariant.not_discarded;
    invariant.discardString = "";
    return true;
  }

  public Iterator getPropertyFiltersIterator() {
    return propertyFilters.iterator();
  }

  private InvariantFilter find(String description) {
    InvariantFilter answer = null;
    for (Iterator iter = propertyFilters.iterator(); iter.hasNext(); ) {
      InvariantFilter filter = (InvariantFilter) iter.next();
      if (filter.getDescription().equals(description)) {
        answer = filter;
      }
    }
    return answer;
  }

  public boolean getFilterSetting( String description ) {
    return find(description).getSetting();
  }

  public void changeFilterSetting( String description, boolean turnOn ) {
    InvariantFilter filter = find(description);
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

  public void addVariableFilter( String variable ) {
    variableFilters.add( new VariableFilter( variable ));
  }

  public boolean containsVariableFilter( String variable ) {
    for (Iterator iter = variableFilters.iterator(); iter.hasNext(); ) {
      VariableFilter vf = (VariableFilter) iter.next();
      if (vf.getVariable().equals( variable )) {
        return true;
      }
    }
    return false;
  }

  public void removeVariableFilter( String variable ) {
    boolean foundOnce = false;
    for (Iterator iter = variableFilters.iterator(); iter.hasNext(); ) {
      VariableFilter vf = (VariableFilter) iter.next();
      if (vf.getVariable().equals( variable )) {
        iter.remove();
        foundOnce = true;
      }
    }
    if (foundOnce) return;

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

  /**
   * This function takes a list of invariants, finds the equality
   * Comparison invariants (x==y, y==z), and deletes and replaces them
   * with Equality invariants (x==y==z).  The first variable in an
   * Equality invariant is always the canonical variable of the group.
   * The Equality invariants are inserted into the beginning.  Equality
   * invariants are useful when it comes to displaying invariants.
   **/
  public static List addEqualityInvariants( List invariants ) {

    return invariants;

    /* [INCR]
    if (invariants.isEmpty())
      return invariants;

    {
      // This method is only safe for calling with a list of invariants
      // from a single program point, because it does comparisons based on
      // VarInfoName.
      PptTopLevel ppt = (PptTopLevel) ((Invariant) invariants.get(0)).ppt.parent;
      for (Iterator itor = invariants.iterator(); itor.hasNext(); ) {
        Invariant inv = (Invariant) itor.next();
        PptTopLevel this_ppt = inv.ppt.parent;
        Assert.assertTrue(this_ppt == ppt);
      }
    }

    // Performing this operation using the following struture would
    // make more sense to me: Map[Canonical -> Set[Non-Canonicals]],
    // instead of HashSet[List[Canonical, Non-Canonicals]].  -JWN 7/9/01
    // I have made it a List[List[Canonical, Non-Canonicals]] to get
    // deterministic iterator behavior; the above comment still makes
    // sense, though.  -MDE 7/21

    // A set of groups of equivalent variables.  The "groups" are actually
    // List's.  We use List's instead of Set's because we need to preserve
    // order, so that canonical variables remain first.
    List equivalentGroups = new Vector();

    // We want a map from canonical variable to PptSlice.  Equality needs a
    // PptSlice so it can report num_values() and num_samples().
    // However, this maps to an invariant, from which a ppt can be extracted.
    // The reason is that we want to choose the ppt associated with the
    // lexically first invariant, so that this method is deterministic.
    Map ppts = new HashMap();   // canonical VarInfo -> equality Invariant

    // This method makes two passes through the list of invariants.  The
    // first pass is to set up a group for each canonical variable.  The
    // second pass fills up each group with equivalent variables.  The main
    // advantage of doing the initial first pass is that canonical
    // variables will be at the beginning of the List, and thus will be
    // displayed first in the output "x==y==z".  A secondary advantage is
    // that we don't run into the following problem:  Say we have "a == b",
    // "b == c", "c == d".  If we encounter the first and the third
    // invariants first, they will be put into two separate sets.  Each set
    // will develop independently and end up having a, b, c, d.

    // First pass: set up a group for each canonical variable.  First,
    // construct the Set canonicalVariables.  The advantage of using the
    // Set class is that duplicates are taken care of (we might see a
    // canonical variable more than once).  Second, for each element of
    // canonicalVariables, add a List to equivalentGroups.
    Set canonicalVariables = new TreeSet(new VarInfo.LexicalComparator());
    for (Iterator iter = invariants.iterator(); iter.hasNext(); ) {
      Invariant invariant = (Invariant) iter.next();
      if (IsEqualityComparison.it.accept( invariant )) {
        if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
          PrintInvariants.debugFiltering.fine ("Found invariant which says " + invariant.format());
        }
        // System.out.println("Found equality invariant: " + invariant.format() + " " + invariant.ppt.name);
        // System.out.println("    " + invariant.repr());
        VarInfo[] variables = invariant.ppt.var_infos;
        Assert.assertTrue( variables.length == 2 );
        for (int i = 0; i < variables.length; i++) {
          VarInfo vi = variables[i];
          // System.out.println("  " + vi.name.name() + " canonical=" + vi.isCanonical());
          if (true) { // vi.isCanonical()) { // [INCR] XXX; This whole method sucks now.
            if (! canonicalVariables.contains( vi )) {
              Assert.assertTrue(! ppts.containsKey(vi));
              canonicalVariables.add( vi );
              ppts.put( vi, invariant );
            } else {
              Assert.assertTrue(ppts.containsKey(vi));
              Invariant old_inv = (Invariant) ppts.get(vi);
              if (PptTopLevel.icfp.compare(invariant, old_inv) < 0) {
                ppts.put( vi, invariant );
              }
            }
          }
        }
      }
    }
    for (Iterator iter = canonicalVariables.iterator(); iter.hasNext(); ) {
      VarInfo vi = (VarInfo) iter.next();
      List list = new ArrayList();
      list.add( vi );
      equivalentGroups.add( list );
    }

    // Second pass: fill up each group with equivalent variables.
    for (Iterator iter = invariants.iterator(); iter.hasNext(); ) {
      Invariant invariant = (Invariant) iter.next();
      if (IsEqualityComparison.it.accept( invariant )) {
        // We don't need this invariant, since it will be included in
        // the equality invariant.
        iter.remove();

        VarInfo variable1 = ((Comparison) invariant).var1();
        VarInfo variable2 = ((Comparison) invariant).var2();
        for (Iterator iter2 = equivalentGroups.iterator(); iter2.hasNext(); ) {
          List equivalentGroup = (List) iter2.next();
          if (equivalentGroup.contains( variable1 )
              &&  ! equivalentGroup.contains( variable2 )) {
            equivalentGroup.add( variable2 );
          } else if (equivalentGroup.contains( variable2 )
                     &&  ! equivalentGroup.contains( variable1 )) {
            equivalentGroup.add( variable1 );
          }
        }
      }
    }

    Assert.assertTrue(ppts.size() == equivalentGroups.size());
    List equality_invariants = new Vector();

    // Add equivalent groups as equality invariants.
    for ( Iterator egIter = equivalentGroups.iterator(); egIter.hasNext(); ) {
      List equivalentGroup = (List) egIter.next();
      VarInfo canonicalVar = (VarInfo) equivalentGroup.get(0);
      PptSlice ppt = ((Invariant) ppts.get(canonicalVar)).ppt;

      // ordered_output contains the same elements as equivalentGroup, but
      // ordered according to the canonical variable's equalToNonobvious().

      // Unfortunately, for printing, we want the equivalent
      // expressions to be ordered in the same order that the appear in
      // in the VarInfo.  (No, this won't be true already, we do
      // actually have to do it by hand.)
      Vector ordered_output = new Vector(); // Vector[VarInfo]
      // [INCR] Vector ordered_reference = PrintInvariants.get_equal_vars(canonicalVar, true);
      for ( Iterator varIter = ordered_reference.iterator(); varIter.hasNext(); ) {
        VarInfo vi = (VarInfo) varIter.next();
        if (equivalentGroup.contains(vi)) {
          ordered_output.add(vi);
        }
      }

      // System.out.println("");
      // System.out.println("EquivalentGroup   is " + reprVarInfoList(equivalentGroup));
      // System.out.println("ordered_output    is " + reprVarInfoList(ordered_output));
      // System.out.println("ordered_reference is " + reprVarInfoList(ordered_reference));
      for ( Iterator allVarsIter = equivalentGroup.iterator(); allVarsIter.hasNext(); ) {
        VarInfo vi = (VarInfo) allVarsIter.next();
        Assert.assertTrue(ordered_output.contains(vi));
      }
      equality_invariants.add(new Equality(ordered_output, ppt));
    }

    equality_invariants = PrintInvariants.sort_invariant_list(equality_invariants);
    equality_invariants.addAll(invariants);
    return equality_invariants;
    */ // [INCR]
  }

  // For debugging
  static String reprVarInfoList(List vis) {
    String result = "";
    for (int i=0; i<vis.size(); i++) {
      if (i!=0) result += " ";
      VarInfo vi = (VarInfo)vis.get(i);
      result += vi.name.name();
      // [INCR] result += " { " + vi.canonicalRep().name.name() + " }";
    }
    return "[ " + result + " ]";
  }

}
