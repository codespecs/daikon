package daikon;

import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
import daikon.inv.*;

import java.util.*;

import utilMDE.*;




// All information about a single program point.
// A Ppt may also represent just part of the data: a disjunction.
// This probably doesn't do any direct computation, instead deferring that
// to its views that are slices.

class PptTopLevel extends Ppt {

  // do we need both a num_tracevars for the number of variables in the
  // tracefile and a num_non_dreived_vars for the number of variables
  // actually passed off to this Ppt?  The ppt wouldn't use num_tracevars,
  // but it would make sense to store it here anyway.

  // Maybe I should just use num_orig_vars instead of the first three.

  // number of variables in the trace file; -1 if not yet set
  int num_tracevars;
  int num_orig_vars;

  // Indicates whether derived variables have been introduced.
  // First number: invariants are computed up to this index, non-inclusive
  // Remaining numbers: values have been derived from up to these indices

  // invariant:  len(var_infos) >= invariants_index >= derivation_index[0]
  //   >= derivation_index[1] >= ...
  int[] derivation_indices = new int[derivation_passes+1];

  VarValues values;

  // These accessors are for abstract methods declared in Ppt
  public int num_samples() { return values.num_samples; }
  public int num_mod_non_missing_samples() { return values.num_mod_non_missing_samples(); }
  public int num_values() { return values.num_values; }
  // public int num_missing() { return values.num_missing; }


  // These are now in PptSlice objects instead.
  // Invariants invs;

  PptTopLevel(String name_, VarInfo[] var_infos_) {
    name = name_;
    var_infos = var_infos_;
    for (int i=0; i<var_infos.length; i++) {
      var_infos[i].value_index = i;
      var_infos[i].varinfo_index = i;
    }

    values = new VarValues();

    // While there are no constants, this works.
    num_tracevars = var_infos.length;
    num_orig_vars = 0;

    derivation_indices = new int[] { 0, 0 };

    // views = new WeakHashMap();
    views = new HashSet();
    for (int i=0; i<var_infos.length; i++) {
      PptSliceGeneric slice1 = new PptSliceGeneric(this, var_infos[i]);
      addView(slice1);
      for (int j=i+1; j<var_infos.length; j++) {
	PptSliceGeneric slice2 = new PptSliceGeneric(this, var_infos[i], var_infos[j]);
	addView(slice2);
	/// Arity 3 is not yet implemented.
	// for (int k=j+1; j<var_infos.length; k++) {
	//   PptSliceGeneric slice3 = new PptSliceGeneric(this, var_infos[i], var_infos[j], var_infos[k]);
	//   addView(slice3);
	// }
      }
    }
    System.out.println("" + views.size() + " views for " + name);

  }

  // This is used when merging two sets of data, to create Ppts that were
  // missing in one of them.
  PptTopLevel(PptTopLevel other) {
    name = other.name;
    var_infos = other.var_infos;
    derivation_indices = new int[other.derivation_indices.length];
    System.arraycopy(other.derivation_indices, 0, derivation_indices, 0, derivation_indices.length);
    values = new VarValues();
    // views = new WeakHashMap();
    views = new HashSet();
  }

  // Accessing data
  int num_vars() {
    return var_infos.length;
  }
  Iterator var_info_iterator() {
    return Arrays.asList(var_infos).iterator();
  }



  boolean compatible(Ppt other) {
    // This insists that the var_infos lists are identical.  The Ppt
    // copy constructor does reuse the var_infos field.
    return (var_infos == other.var_infos);
  }

  // // Strangely, this modifies "other" but not "this".  That looks wrong.
  // // Perhaps this should operate over VarValues objects or some such?
  // void merge(PptTopLevel other) {
  //   // ensure that the two program points are compatible
  //   Assert.assert(compatible(other));
  //
  //   Set other_entries = other.values.entrySet();
  //   for (Iterator itor = other_entries.iterator() ; itor.hasNext() ;) {
  //     Map.Entry other_entry = (Map.Entry) itor.next();
  //     ValueTuple value_tuple = (ValueTuple) other_entry.getKey();
  //     int new_count = (values.get(value_tuple)
  //                      + VarValues.getValue(other_entry));
  //     // equivalent but less efficient:  values.put(value_tuple, new_count);
  //     VarValues.setValue(other_entry, new_count);
  //   }


  /// Commented out until I decide to specially deal with constants.
  // // This sets the num_truevars and num_constants fields and also
  // // reorders the VarInfos so that constants are at the end.
  // private void count_vars() {
  //   Assert.assert(num_constants == -1);
  //   // Also, there had better be no values, derived variables, or other
  //   // things that could depend on the index.

  //   // First, move the constants to the end.
  //   Vector constants = new Vector();
  //   for (int i=0; i<var_infos.length; i++) {
  //     if (var_infos[i].constant_value != null) {
  //       constants.add(var_infos[i]);
  //     }
  //   }
  //   num_constants = constants.size();
  //   num_truevars = var_infos.length - num_constants;

  //   if ((num_constants > 0)
  //       && (constants.elementAt(0) != var_infos[num_truevars])) {
  //     // We must reorder.  I'm not sorting based on constness because I
  //     // would want a stable sort.
  //     VarInfo[] truevars = new VarInfo[num_truevars];
  //     int truevarindex = 0;
  //     for (int i=0; i<var_infos.length; i++) {
  //       if (!constants.contains(var_infos[i])) {
  // 	truevars[truevarindex] = var_infos[i];
  // 	truevarindex++;
  //       }
  //     }
  //     System.arraycopy(truevars, 0, var_infos, 0, num_truevars);
  //     for (int i=0; i<num_constants; i++) {
  //       var_infos[i+num_truevars] = (VarInfo)constants.elementAt(i);
  //     }
  //     for (int i=0; i<var_infos.length; i++) {
  //       var_infos[i].index = i;
  //     }
  //   }
  // }



  ///////////////////////////////////////////////////////////////////////////
  /// Adding variables
  ///

  // Some of this should perhaps be moved up into Ppt.

  // I'm not using a Vector for the var_infos field, even though that would
  // simplify adding new elements, because I want static typechecking and I
  // don't want the overheads of lots of casts and of extra space for
  // Vector objects.

  private void addVarInfo(VarInfo vi) {
    VarInfo[] vis = new VarInfo[] { vi };
    addVarInfos(vis);
  }

  private void addVarInfos(VarInfo[] vis) {
    int old_length = var_infos.length;
    VarInfo[] new_var_infos = new VarInfo[var_infos.length + vis.length];
    System.arraycopy(var_infos, 0, new_var_infos, 0, old_length);
    System.arraycopy(vis, 0, new_var_infos, old_length, vis.length);
    // for (int i=old_length; i<new_var_infos.length; i++) {
    //   new_var_infos[i].index = i;
    // }
    var_infos = new_var_infos;
  }

  private void addVarInfos(Vector v) {
    int old_length = var_infos.length;
    VarInfo[] new_var_infos = new VarInfo[var_infos.length + v.size()];
    System.arraycopy(var_infos, 0, new_var_infos, 0, old_length);
    for (int i=0, size=v.size(); i<size; i++) {
      VarInfo vi = (VarInfo) v.elementAt(i);
      // vi.index = i+old_length;
      new_var_infos[i+old_length] = vi;
    }
    var_infos = new_var_infos;

    // Now I still have to add the values of the new variables.

  }




  ///
  /// Adding special variables
  ///

  // I should support this with a HashMapEq which is computed just once.  I
  // don't want to add yet another slot because it's used only while
  // reading files.

  // Given a program point, if it represents a function exit, then
  // return the corresponding function entry point.

  PptTopLevel entry_ppt(PptMap all_ppts) {
    return PptTopLevel.entry_ppt(this, all_ppts);
  }

  static PptTopLevel entry_ppt(PptTopLevel ppt, PptMap all_ppts) {

    // Don't do this, because it returns a value for :::LOOP, etc.
    // if (ppt.name.endsWith(FileIO.enter_tag))
    //   return null;
    // String fn_name = ppt.fn_name();
    // if (fn_name == null)
    //   return null;
    // String entry_ppt_name = (fn_name + FileIO.enter_tag).intern();

    if (!ppt.name.endsWith(FileIO.exit_tag))
      return null;
    String fn_name = ppt.name.substring(0, ppt.name.length() - FileIO.exit_tag.length());
    String entry_ppt_name = (fn_name + FileIO.enter_tag).intern();

    return (PptTopLevel) all_ppts.get(entry_ppt_name);
  }


  // Add "_orig" variables to the program point.
  void add_orig_vars(Ppt entry_ppt) {

    VarInfo[] begin_vis = entry_ppt.var_infos;
    num_orig_vars = begin_vis.length;
    // Don't bother to include the constants.
    Vector new_vis = new Vector(num_orig_vars);
    for (int i=0; i<num_orig_vars; i++) {
      VarInfo vi = begin_vis[i];
      if (vi.isConstant() || vi.isDerived())
	continue;
      new_vis.add(new VarInfo(vi.name + "_orig", vi.type, vi.rep_type, ExplicitVarComparability.makeAlias(vi.name, vi.comparability)));
    }
    addVarInfos(new_vis);
  }



  /// Possibly just blow this off; I'm not sure I care about it.
  /// In any event, leave it until later.
  //
  // void add_invocation_count_vars() {
  //
  //   // Add invocation counts
  //   if (compute_invocation_counts) {
  //     for ppt in fns_to_process {
  //       these_var_infos = fn_var_infos[ppt];
  //       for callee in fn_invocations.keys() {
  // 	calls_var_name = "calls(%s)" % callee;
  // 	these_var_infos.append(var_info(calls_var_name, "integral", "always", len(these_var_infos)));
  // 	these_values.append(fn_invocations[callee]);
  // 	current_var_index++;
  //       }
  //     }
  //   }
  //
  //       (ppt_sans_suffix, ppt_suffix) = (string.split(ppt, ":::", 1) + [""])[0:2]
  //       if ((ppt_suffix != "EXIT")
  // 	  and (ppt_suffix[0:4] != "EXIT")):
  // 	  continue
  //       these_var_infos = fn_var_infos[ppt]
  //       entry_ppt = ppt_sans_suffix + ":::ENTER"
  //       for vi in fn_var_infos[entry_ppt][0:fn_truevars[entry_ppt]]:
  // 	  these_var_infos.append(var_info(vi.name + "_orig", vi.type, comparability_make_alias(vi.name, vi.comparability), len(these_var_infos)))
  //
  // }


  ///////////////////////////////////////////////////////////////////////////
  /// Derived variables
  ///

  // This is here because I think it doesn't make sense to derive except
  // from a PptTopLevel (and possibly a PptConditional?).  Perhaps move it
  // later.

  public static boolean worthDerivingFrom(VarInfo vi) {
    return (vi.canonical());
    // Should add this (back) in:
	    // && !vi.always_missing()
	    // && !vi.always_equal_to_null();
  }


  // There are just two passes of interest right now...
  final static int derivation_passes = 2;

  UnaryDerivationFactory[][] unaryDerivations
    = new UnaryDerivationFactory[][] {
      // pass1
      new UnaryDerivationFactory[] {
	new SequenceLengthFactory() },
      // pass2
      new UnaryDerivationFactory[] {
	new SequenceExtremumFactory(),
	new SequenceMinMaxSumFactory(), } };

  BinaryDerivationFactory[][] binaryDerivations
    = new BinaryDerivationFactory[][] {
      // pass1
      new BinaryDerivationFactory[] { },
      // pass2
      new BinaryDerivationFactory[] {
	new SequenceScalarSubscriptFactory() }
    };


  // This does no inference; it just calls deriveVariablesOnePass once per pass.

  // If derivation_index == (a, b, c) and n = len(var_infos), then
  // the body of this loop:
  //     * does pass1 introduction for b..a
  //     * does pass2 introduction for c..b
  // and afterward, derivation_index == (n, a, b).
  public Vector derive() {
    // Actually, it should be sorted in *reverse* order.
    // Assert.assert(ArraysMDE.sorted(derivation_indices));

    Vector result = new Vector();
    for (int pass=1; pass<derivation_passes+1; pass++) {
      // if (debug_derive)
      //   System.out.println("pass " + pass_no + ", range " + derivation_indices[pass] + "..." + derivation_indices[pass-1]);
      int this_di = derivation_indices[pass];
      int last_di = derivation_indices[pass-1];
      if (this_di == last_di)
	continue;
      result.addAll(deriveVariablesOnePass(this_di, last_di,
					   unaryDerivations[pass-1],
					   binaryDerivations[pass-1]));
    }
    // convert [a,b,c] into [n,a,b]
    for (int i=derivation_passes+1; i>0; i--)
      derivation_indices[i] = derivation_indices[i-1];
    derivation_indices[0] = var_infos.length;


    // derivation_index = (num_vars,) + derivation_indices[:-1]
    // if debug_derive:
    //     print "new derivation_index =", derivation_index, "num_vars =", len(var_infos)

    return result;
  }


  // This routine does one "pass"; that is, it adds some set of derived
  // variables, according to the functions that are passed in.

  // Returns a vector of Derivation objects.
  // The arguments are:
  //   // VAR_INFOS: only values (partially) computed from these are candidates
  //   INDICES:  only values (partially) computed from the VarInfo objects at
  //     these indices are candidates.  I could imagine doing this for an
  //     arbitrary list of VarInfo objects as well, but put that off until later.
  //   FUNCTIONS: (long) list of functions for adding new variables; see the code
  Vector deriveVariablesOnePass(int vi_index_min, int vi_index_limit, UnaryDerivationFactory[] unary, BinaryDerivationFactory[] binary) {

    // will be converted into array at end of function
    Vector result = new Vector();

    for (int i=vi_index_min; i<vi_index_limit; i++) {
      VarInfo vi = var_infos[i];
      if (!worthDerivingFrom(vi))
	continue;
      for (int di=0; di<unary.length; di++) {
	UnaryDerivationFactory d = unary[di];
	if (d.applicable(vi)) {
	  UnaryDerivation[] uderivs = d.instantiate(vi);
	  for (int udi=0; udi<uderivs.length; udi++)
	    result.add(uderivs[udi]);
	}
      }
    }

    // I want to get all pairs such that at least one of the elements is in
    // var_infos, but I want to generate each such pair only once.  This
    // probably isn't the most efficient technique, but it's probably
    // adequate and is not excessively complicated or excessively slow.
    for (int i1=0; i1<var_infos.length; i1++) {
      VarInfo vi1 = var_infos[i1];
      if (!worthDerivingFrom(vi1))
	continue;
      boolean target1 = (ArraysMDE.indexOfEq(var_infos, vi1) != -1);
      int i2_min = (target1 ? i1+1 : Math.max(i1+1, vi_index_min));
      int i2_limit = (target1 ? var_infos.length : vi_index_limit);
      for (int i2=i2_min; i2<i2_limit; i2++) {
	VarInfo vi2 = var_infos[i2];
	if (!worthDerivingFrom(vi2))
	  continue;
	// Not necessary, as the limits guarantee this is of interest.
	// if ((!target1) && (ArraysMDE.indexOfEq(var_infos, vi2) == -1))
	//   // Do nothing if neither of these variables is under consideration.
	//   continue;
	for (int di=0; di<binary.length; di++) {
	  BinaryDerivationFactory d = binary[di];
	  if (d.applicable(vi1, vi2)) {
	    // conceivably make this addAll one day if instantiate returns a Vector
	    BinaryDerivation[] bderivs = d.instantiate(vi1, vi2);
	    for (int bdi=0; bdi<bderivs.length; bdi++)
	      result.add(bderivs[bdi]);
	  }

	}
      }
    }

    return result;
  }


  ///
  /// Adding derived variables
  ///

  // This doesn't compute what the derived variables should be, just adds
  // them after being computed.

  void addDerivedVariables(Vector derivs) {
    IndexedDerivation[] derivs_array
      = (IndexedDerivation[]) derivs.toArray(new IndexedDerivation[0]);
    addDerivedVariables(derivs_array);
  }

  void addDerivedVariables(IndexedDerivation[] derivs) {

    VarInfo[] vis = new VarInfo[derivs.length];
    for (int i=0; i<derivs.length; i++) {
      vis[i] = derivs[i].makeVarInfo(this);
    }
    addVarInfos(vis);

    // Since I am only modifying members, not making new objects, and since
    // I am using an Eq hash table, I don't need to rehash.
    for (Iterator itor = values.entrySet().iterator() ; itor.hasNext() ; ) {
      Map.Entry entry = (Map.Entry) itor.next();
      ValueTuple vt = (ValueTuple) entry.getKey();
      vt.extend(derivs);
    }
  }




  ///////////////////////////////////////////////////////////////////////////
  /// Manipulating values
  ///

  void add(ValueTuple vt, int count) {
    // System.out.println("PptTopLevel " + name + ": add " + vt);

    values.increment(vt, count);

    // Add to all the views
    // for (Iterator itor = views.keySet().iterator() ; itor.hasNext() ; ) {
    for (Iterator itor = views.iterator() ; itor.hasNext() ; ) {
      Ppt view = (Ppt) itor.next();
      view.add(vt, count);
    }

  }

  // void process() {
  //   throw new Error("To implement");
  // }

  boolean contains(ValueTuple vt) {
    return values.containsKey(vt);
  }

  int count(ValueTuple vt) {
    return values.get(vt);
  }

  Iterator entrySet() {
    return values.entrySet().iterator();
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Printing invariants
  ///

  // In original implementation, known as print_invariants_ppt
  /*
   * Print invariants for a single program point.
   * Does no output if no samples or no views.
   */
  public void print_invariants_maybe() {
    if (num_samples() == 0)
      return;
    if (views.size() == 0) {
      System.out.println("[No views for " + name + "]");
      return;
    }
    System.out.println("===========================================================================");
    print_invariants();
  }

  /** Print invariants for a single program point. */
  public void print_invariants() {
    System.out.println(name + "  "
		       + num_samples() + " samples");
    System.out.println("    Samples breakdown: "
		       + values.tuplemod_samples_summary());
    // for (Iterator itor2 = views.keySet().iterator() ; itor2.hasNext() ; ) {
    for (Iterator itor2 = views.iterator() ; itor2.hasNext() ; ) {
      PptSlice slice = (PptSlice) itor2.next();
      Invariants invs = slice.invs;
      int num_invs = invs.size();
      for (int i=0; i<num_invs; i++) {
	Invariant inv = invs.elementAt(i);
	String inv_rep = inv.format();
	if (inv_rep != null) {
	  System.out.println(inv_rep);
	  System.out.println("  " + inv.repr());
	} else {
	  System.out.println("[suppressed: " + inv.repr() + " ]");
	}
      }
    }
  }

  /// I need to incorporate all this into the above.

  // def print_invariants_ppt(fn_name, print_unconstrained=0):
  //   """Print invariants for a single program point."""
  //   print fn_name, fn_samples[fn_name], "samples"
  //   var_infos = fn_var_infos[fn_name]
  //
  //   # Equality invariants
  //   for vi in var_infos:
  //       if not vi.is_canonical():
  //           continue
  //       if vi.equal_to == []:
  //           # Not equal to anything else, so not an equality invariant
  //           continue
  //       if vi.invariant.is_exact():
  //           # was vi.invariant.min
  //           value = "= %s" % (vi.invariant.one_of[0],) # this value may be a sequence
  //       else:
  //           value = ""
  //       print vi.name, "=", string.join(map(lambda idx, vis=var_infos: vis[idx].name, vi.equal_to), " = "), value,
  //       if vi.invariant.values == 1:
  //           print "\t(1 value)"
  //       else:
  //           print "\t(%d values)" % (vi.invariant.values,)
  //
  //   # Single invariants
  //   for vi in var_infos:
  //       if not vi.is_canonical():
  //           continue
  //       this_inv = vi.invariant
  //       if (this_inv.is_exact() and vi.equal_to != []):
  //           # Already printed above in "equality invariants" section
  //           continue
  //       if print_unconstrained or not this_inv.is_unconstrained():
  //           print " ", this_inv.format((vi.name,))
  //   # Pairwise invariants
  //   nonequal_constraints = []
  //   # Maybe this is faster than calling "string.find"; I'm not sure.
  //   nonequal_re = re.compile(" != ")
  //   for vi in var_infos:
  //       if not vi.is_canonical():
  //           continue
  //       vname = vi.name
  //       for (index,inv) in vi.invariants.items():
  //           if type(index) != types.IntType:
  //               continue
  //           if not var_infos[index].is_canonical():
  //               continue
  //           if (not print_unconstrained) and inv.is_unconstrained():
  //               continue
  //           formatted = inv.format((vname, var_infos[index].name))
  //           # Not .match(...), which only checks at start of string!
  //           if nonequal_re.search(formatted):
  //               nonequal_constraints.append(formatted)
  //           else:
  //               print "   ", formatted
  //   for ne_constraint in nonequal_constraints:
  //       print "    ", ne_constraint
  //   # Three-way (and greater) invariants
  //   for vi in var_infos:
  //       if not vi.is_canonical():
  //           continue
  //       vname = vi.name
  //       for (index_pair,inv) in vi.invariants.items():
  //           if type(index_pair) == types.IntType:
  //               continue
  //           (i1, i2) = index_pair
  //           if not var_infos[i1].is_canonical():
  //               # Perhaps err; this shouldn't happen, right?
  //               continue
  //           if not var_infos[i2].is_canonical():
  //               # Perhaps err; this shouldn't happen, right?
  //               continue
  //           if print_unconstrained or not inv.is_unconstrained():
  //               print "     ", inv.format((vname, var_infos[i1].name, var_infos[i2].name))
  //   sys.stdout.flush()



}
