/*
 *  (C) 2002 MIT Laboratory for Computer Science.
 *
 *  Author: Alan Donovan <adonovan@lcs.mit.edu>
 *
 *  JTraceInference.java -- Inference thread of JTrace host program
 *
 *  $Id$
 *
 */

package daikon;

import java.util.Vector;
import java.util.Arrays;
import java.util.List;

import daikon.Daikon;
import daikon.FileIO;
import daikon.PptTopLevel;
import daikon.PptMap;
import daikon.ProglangType;
import daikon.VarComparability;
import daikon.VarInfo;
import daikon.VarInfoName;
import daikon.Dataflow;
import daikon.PrintInvariants;
import daikon.ValueTuple;
import daikon.inv.Invariant;
import daikon.config.Configuration;

import utilMDE.Intern;
import utilMDE.Assert;

class JTraceInference extends Thread
{
    JTraceInference() {
	start(); // execute run() in a new thread
    }

    private PptMap all_ppts = new PptMap();

    public void	run()
    {
	JTrace.println(JTrace.V_INFO, "JTrace: Inference thread start.");

	/////////

	// XXX really all the types are subordinate to a PptTrace,
	// just like pairs of strings are subordinate to a Decl
	// program point. Could rearrange this? Do we want PptTrace
	// Start/End markers, or is it just clutter?

	loop: for(;;)
	{
	    byte control = getControl();
	    JTrace.print(JTrace.V_INFO, "JTrace: getControl : " + types[control] + " = " );

	    switch(control)
	    {
	    case CT_PptTrace: { // Header of a Trace program point
		int key = getInteger();
		String ppt_name = getPptName(key);
		JTrace.println(JTrace.V_INFO, ppt_name);
		PptTopLevel ppt = (PptTopLevel) all_ppts.get(ppt_name);
		ValueTuple vt = read_single_sample(ppt_name);
		FileIO.process_sample(ppt, vt, null);
		break;
	    }

	    case CT_PptDeclStart: { // Header of a Decl program point
		int key = getInteger();
		String pptname = getPptName(key);
		JTrace.println(JTrace.V_INFO, pptname);
		Vector var_infos = new Vector();
		while((control = getControl()) != CT_PptDeclEnd)
		{
		    Assert.assert(control == CT_string);
		    control = getControl();
		    Assert.assert(control == CT_string);

		    String varname = getString();
		    String proglang_type_string = getString();
		    JTrace.println(JTrace.V_INFO, "\tname=" + varname +
			    ", type=" + proglang_type_string);

		    ProglangType prog_type =
			ProglangType.parse(proglang_type_string);

		    ProglangType file_rep_type = prog_type;
		    List prims = Arrays.asList(new String[] {
			"int", "byte", "char", "short", "long", "double",
			"float", "boolean", "java.lang.String" });
		    // not a prim => hashcode
		    if(!prims.contains(proglang_type_string))
			file_rep_type = ProglangType.parse("hashcode");
		    VarComparability comparability =
			VarComparability.parse(VarComparability.NONE,
					       null, null);

		    VarInfo vi = new VarInfo(VarInfoName.parse(varname),
					     prog_type, file_rep_type,
					     comparability, false, null);
		    var_infos.add(vi);
		}
		VarInfo[] vi_array =
		    (VarInfo[])var_infos.toArray(new VarInfo[0]);
		PptTopLevel ppt = new PptTopLevel(pptname, vi_array);
		all_ppts.add(ppt);
		Dataflow.init_partial_order(ppt, all_ppts);
		break;
	    }

	    case CT_NoData:
		JTrace.println(JTrace.V_INFO, "<NoData>");
		for(;;)
		    try {
			currentThread().sleep(100); // sleep and try again
			break;
		    } catch(InterruptedException e) {}
		break;

	    case CT_EOF:
		JTrace.println(JTrace.V_INFO, "<EOF>");
		break loop;

	    default:
		Assert.assert(false, "Bad control code!");
		break;
	    } // endswitch
	}

	JTrace.println(JTrace.V_DEBUG,"JTrace: out of sample loop.");

	//
	//  Finish up...
	//

	JTrace.println(JTrace.V_INFO,"JTrace: Inference thread stop.");

	PrintInvariants.print_invariants(all_ppts);
    }

    // Read all data for a certain ppt, create and fill a sample with it
    // The length of the returned ValueTuple is ...?
    private ValueTuple read_single_sample(String ppt_name)
    {
	// from FileIO:

	PptTopLevel ppt = (PptTopLevel) all_ppts.get(ppt_name);
	Assert.assert(ppt != null);

	// not vis.length, as that includes constants, derived variables, etc.
	// Actually, we do want to leave space for _orig vars.
	// And for the time being (and possibly forever), for derived variables.
	VarInfo[] vis = ppt.var_infos;
	int num_tracevars = ppt.num_tracevars;
	int vals_array_size = ppt.var_infos.length - ppt.num_static_constant_vars;
	Assert.assert(vals_array_size == num_tracevars + ppt.num_orig_vars);

	Object[] vals = new Object[vals_array_size];
	int[] mods = new int[vals_array_size];

	// XXX modbits -- skipped

	// XXX invocation nonce -- skipped

	{
	    JTrace.println(JTrace.V_INFO, "JTrace: getting " + num_tracevars + " values");
	    // XXX oldvalue_reps has been thrown away

	    // foreach variable associated with this ppt:
	    for (int vi_index=0, val_index=0; val_index<num_tracevars; vi_index++) {
		Assert.assert(vi_index < vis.length
			      // , "Got to vi_index " + vi_index + " after " + val_index + " of " + num_tracevars + " values"
		    );
		// XXX handling of final statics (compile-time constants)
		VarInfo vi = vis[vi_index];
		Assert.assert((! vi.is_static_constant)
			      || (vi.value_index == -1)
			      // , "Bad value_index " + vi.value_index + " when static_constant_value = " + vi.static_constant_value + " for " + vi.repr() + " at " + ppt_name
		    );
		if (vi.is_static_constant)
		    continue;
		Assert.assert(val_index == vi.value_index
			      // , "Differing val_index = " + val_index
			      // + " and vi.value_index = " + vi.value_index
			      // + " for " + vi.name + lineSep + vi.repr()
		    );

		// XXX read var name from dtrace file: omitted: we assume that
		// it correctly matches that in the decls file. This is an
		// optimisation. It may make debugging harder.

		// XXX read var value from dtrace file: now we read it from
		// libJTrace:

		Object value = null;
		byte control = getControl();
		JTrace.print(JTrace.V_INFO, "JTrace: getControl : " + types[control] + " = " );


		switch(control)
		{
		case CT_boolean:
		case CT_byte:
		case CT_char:
		case CT_short:
		case CT_int:
		    value = Intern.internedLong(getInteger());
		    break;
		case CT_long:
		    value = Intern.internedLong(getLong());
		    break;
		case CT_double:
		case CT_float:
		    value = Intern.internedDouble(getDouble());
		    break;
		case CT_string:
		    value = Intern.intern(getString());
		    break;
		case CT_Hashcode:
		    value = Intern.internedLong(getInteger());
		    break;
		case CT_ArrayLength:
		    value = Intern.internedLong(getInteger());
		    break;
		default:
		    Assert.assert(false, "freak-up in control stream!");
//			    abort();
		    break;
		}

		JTrace.println(JTrace.V_INFO, "" + value);

		// XXX read modbit of value from dtrace file: omitted. For now
		// assume value is always present. Need to address this.

		int mod = ValueTuple.MODIFIED;

		mods[val_index] = mod;
//      oldvalue_reps[val_index] = value_rep;
		if (ValueTuple.modIsMissing(mod)) {
		    vals[val_index] = null;
		} else {
		    // XXX type check required!
		    vals[val_index] = value;
		}
		val_index++;
	    }

//    ppt_to_value_reps.put(ppt, oldvalue_reps);

	}

	return ValueTuple.makeUninterned(vals, mods);
    }

    // called from another thread:
    public void			joinX() { // join, but suppress failure
	for(;;)
	    try {
		join(); // wait for thread to exit
		return;
	    } catch(InterruptedException e) {}
    }

    private native Object	getSample();

    // returns JTrace-interned reference to program point name.
    // Note: use of invalid keys is catastrophic!
    private native String	getPptName(int key);

    // return next load of samples of the specified kind
    private native double[]	getDoubles();
    private native long[]	getLongs();
    private native int[]	getIntegers();
    private native String[]	getStrings();
    private native byte[]	getControls();

    private double[]		_theDoubles;	private int _doubleCount;
    private long[]		_theLongs;	private int _longCount;
    private int[]		_theIntegers;	private int _integerCount;
    private String[]		_theStrings;	private int _stringCount;
    private byte[]		_theControls;	private int _controlCount;

    // dispense one at a time:
    private double	getDouble() {
	if(_theDoubles == null || _doubleCount == _theDoubles.length)
	{
	    _theDoubles  = getDoubles();
	    _doubleCount = 0;
	}
	return _theDoubles[_doubleCount++];
    }
    private long	getLong() {
	if(_theLongs == null || _longCount == _theLongs.length)
	{
	    _theLongs  = getLongs();
	    _longCount = 0;
	}
	return _theLongs[_longCount++];
    }
    private int		getInteger() {
	if(_theIntegers == null || _integerCount == _theIntegers.length)
	{
	    _theIntegers  = getIntegers();
	    _integerCount = 0;
	}
	return _theIntegers[_integerCount++];
    }
    private String	getString() {
	if(_theStrings == null || _stringCount == _theStrings.length)
	{
	    _theStrings  = getStrings();
	    _stringCount = 0;
	}
	return _theStrings[_stringCount++];
    }
    private byte	getControl() {
	if(_theControls == null || _controlCount == _theControls.length)
	{
	    _theControls  = getControls();
	    _controlCount = 0;
	}
	return _theControls[_controlCount++];
    }

    // enum ControlToken from ev-dispatcher.h -- KEEP CONSISTENT!
    private static final int CT_void		= 0;
    private static final int CT_boolean		= 1;
    private static final int CT_byte		= 2;
    private static final int CT_char		= 3;
    private static final int CT_short		= 4;
    private static final int CT_int		= 5;
    private static final int CT_long		= 6;
    private static final int CT_double		= 7;
    private static final int CT_float		= 8;
    private static final int CT_string		= 9;
    private static final int CT_Hashcode	= 10;
    private static final int CT_ArrayLength	= 11;
    private static final int CT_PptTrace	= 12;
    private static final int CT_PptDeclStart	= 13;
    private static final int CT_PptDeclEnd	= 14;
    private static final int CT_NoData		= 15;
    private static final int CT_EOF		= 16;

    private static final String types[] = new String[] {
	"void",
	"boolean",
	"byte",
	"char",
	"short",
	"int",
	"long",
	"double",
	"float",
	"string",
	"HashCode",
	"ArrayLength",
	"PptTrace",
	"PptDeclStart",
	"PptDeclEnd",
	"NoData",
	"EOF"
    };
}

/*
 * Local Variables:
 * c-basic-offset:	4
 * c-indentation-style: "stroustrup"
 * End:
 */
