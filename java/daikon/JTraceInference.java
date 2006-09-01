/*
 *  Copyright 2002-2003 MIT Laboratory for Computer Science.
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
import java.util.Set;
import java.util.HashSet;
import java.io.PrintWriter;
import java.io.StringWriter;

import utilMDE.Intern;
import utilMDE.Assert;

class JTraceInference extends Thread
{
    JTraceInference() {
        start(); // execute run() in a new thread
    }

    // Public so that others can read out the invariants!
    public PptMap all_ppts = new PptMap();

    /** Non-null when error has occurred. */
    public String failure_message = null;

    // XXX use this for now
    private final VarComparability default_comparability =
        VarComparabilityNone.it;

    private final List<String> prims = Arrays.asList(new String[] {
        "int",
        "byte",
        "char",
        "short",
        "long",
        "double",
        "float",
        "boolean",
        "java.lang.String"
    });

    public void run()
    {
      try {
        JTrace.println(JTrace.V_INFO, "JTrace: Inference thread start.");

        loop: for (;;)
        {
            byte control = getControl();
            JTrace.print(JTrace.V_DEBUG, "JTrace: getControl : " + types[control] + " = " );

            switch(control)
            {
            case CT_PptTraceStart: { // Header of a Trace program point
                int key = getInteger();
                String ppt_name = getPptNameX(key);
                int nonce = getInteger();
                JTrace.println(JTrace.V_DEBUG, ppt_name);
                PptTopLevel ppt = all_ppts.get(ppt_name);
                ValueTuple vt = read_single_sample(ppt_name);
                Integer nonce_ = nonce == 0 ? null : new Integer(nonce);
                FileIO.process_sample(all_ppts, ppt, vt, nonce_);
                control = getControl();
                Assert.assertTrue(control == CT_PptTraceEnd);
                break;
            }

            case CT_PptDeclStart: { // Header of a Decl program point
                int key = getInteger();
                String pptname = getPptNameX(key);
                JTrace.println(JTrace.V_DEBUG, pptname);
                Vector<VarInfo> var_infos = new Vector<VarInfo>();

                while((control = getControl()) != CT_PptDeclEnd)
                {
                    Assert.assertTrue(control == CT_string);
                    control = getControl();
                    Assert.assertTrue(control == CT_string);

                    String varname = getString();
                    String proglang_type_string = getString();
                    JTrace.println(JTrace.V_DEBUG, "\tname=" + varname +
                            ", type=" + proglang_type_string);

                    ProglangType prog_type =
                        ProglangType.parse(proglang_type_string);

                    ProglangType file_rep_type = prog_type;
                    // not a prim => hashcode
                    if (!prims.contains(proglang_type_string))
                        file_rep_type = ProglangType.parse("hashcode");

                    VarInfo vi = new VarInfo(varname, prog_type, file_rep_type,
                                             default_comparability, false,
                                             null, VarInfoAux.getDefault());
                    var_infos.add(vi);
                }
                VarInfo[] vi_array =
                    (VarInfo[])var_infos.toArray(new VarInfo[0]);
                PptTopLevel ppt = new PptTopLevel(pptname, vi_array);
                // FIXME: Toh added this so the code compiles.  This
                // should instead use Alan's batching method.
                all_ppts.add(ppt);
                break;
            }

            case CT_NoData:
                JTrace.println(JTrace.V_DEBUG, "<NoData>");
                for (;;)
                    try {
                        Thread.sleep(100); // sleep and try again
                        break;
                    } catch(InterruptedException e) {}
                break;

            case CT_EOF:
                JTrace.println(JTrace.V_DEBUG, "<EOF>");
                break loop;

            default:
                Assert.assertTrue(false, "Bad control code!");
                break;
            } // endswitch
        }

        JTrace.println(JTrace.V_DEBUG,"JTrace: out of sample loop.");

        FileIO.process_unmatched_procedure_entries();

        //
        //  Finish up...
        //

        JTrace.println(JTrace.V_INFO,"JTrace: Inference thread stop.");

      } catch (Exception e) {
          StringWriter buf = new StringWriter();
          e.printStackTrace(new PrintWriter(buf));
          failure_message = buf.toString();
      }
    }

    // Read all data for a certain ppt, create and fill a sample with it
    // The length of the returned ValueTuple is ...?
    private ValueTuple read_single_sample(String ppt_name)
    {
        // from FileIO:

        PptTopLevel ppt = all_ppts.get(ppt_name);
        Assert.assertTrue(ppt != null);

        // not vis.length, as that includes constants, derived variables, etc.
        // Actually, we do want to leave space for _orig vars.
        // And for the time being (and possibly forever), for derived variables.
        VarInfo[] vis = ppt.var_infos;
        int num_tracevars = ppt.num_tracevars;
        int vals_array_size = ppt.var_infos.length - ppt.num_static_constant_vars;
        Assert.assertTrue(vals_array_size == num_tracevars + ppt.num_orig_vars);

        Object[] vals = new Object[vals_array_size];
        int[] mods = new int[vals_array_size];

        // XXX modbits -- skipped

        {
            JTrace.println(JTrace.V_DEBUG, "JTrace: getting " + num_tracevars + " values");
            // XXX oldvalue_reps has been thrown away

            // foreach variable associated with this ppt:
            for (int vi_index=0, val_index=0; val_index<num_tracevars; vi_index++) {
                Assert.assertTrue(vi_index < vis.length
                                  // , "Got to vi_index " + vi_index + " after " + val_index + " of " + num_tracevars + " values"
                                  );
                // XXX handling of static finals (compile-time constants)
                VarInfo vi = vis[vi_index];
                Assert.assertTrue((! vi.is_static_constant)
                              || (vi.value_index == -1)
                              // , "Bad value_index " + vi.value_index + " when static_constant_value = " + vi.static_constant_value + " for " + vi.repr() + " at " + ppt_name
                    );
                if (vi.is_static_constant)
                    continue;
                Assert.assertTrue(val_index == vi.value_index
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
                JTrace.print(JTrace.V_DEBUG, "JTrace: getControl : " + types[control] + " = " );

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
                    Assert.assertTrue(false, "freak-up in control stream!");
//                          abort();
                    break;
                }

                JTrace.println(JTrace.V_DEBUG, "" + value);

                // XXX read modbit of value from dtrace file: omitted. For now
                // assume value is always present. Need to address this.

                int mod = ValueTuple.MODIFIED;

                mods[val_index] = mod;
//      oldvalue_reps[val_index] = value_rep;
                if (ValueTuple.modIsMissingNonsensical(mod)) {
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
    public void                 joinX() { // join, but suppress failure
        for (;;)
            try {
                join(); // wait for thread to exit
                return;
            } catch(InterruptedException e) {}
    }

    private String getPptNameX(int key) {
        // Eventually, JTrace should construct the proper name, but
        // for now, I'm more comfortable just fixing it here.
        String result = getPptName(key);
        int after_semi = 1 + result.indexOf(';');
        String clazz = utilMDE.UtilMDE.classnameFromJvm(result.substring(0, after_semi));
        return clazz + result.substring(after_semi);
    }

    private native Object       getSample();

    // returns JTrace-interned reference to program point name.
    // Note: use of invalid keys is catastrophic!
    private native String       getPptName(int key);

    // return next load of samples of the specified kind
    private native double[]     getDoubles();
    private native long[]       getLongs();
    private native int[]        getIntegers();
    private native String[]     getStrings();
    private native byte[]       getControls();

    private double[]            _theDoubles;    private int _doubleCount;
    private long[]              _theLongs;      private int _longCount;
    private int[]               _theIntegers;   private int _integerCount;
    private String[]            _theStrings;    private int _stringCount;
    private byte[]              _theControls;   private int _controlCount;

    // dispense one at a time:
    private double      getDouble() {
        if (_theDoubles == null || _doubleCount == _theDoubles.length)
        {
            _theDoubles  = getDoubles();
            _doubleCount = 0;
        }
        return _theDoubles[_doubleCount++];
    }
    private long        getLong() {
        if (_theLongs == null || _longCount == _theLongs.length)
        {
            _theLongs  = getLongs();
            _longCount = 0;
        }
        return _theLongs[_longCount++];
    }
    private int         getInteger() {
        if (_theIntegers == null || _integerCount == _theIntegers.length)
        {
            _theIntegers  = getIntegers();
            _integerCount = 0;
        }
        return _theIntegers[_integerCount++];
    }
    private String      getString() {
        if (_theStrings == null || _stringCount == _theStrings.length)
        {
            _theStrings  = getStrings();
            _stringCount = 0;
        }
        return _theStrings[_stringCount++];
    }
    private byte        getControl() {
        if (_theControls == null || _controlCount == _theControls.length)
        {
            _theControls  = getControls();
            _controlCount = 0;
        }
        return _theControls[_controlCount++];
    }

    // enum ControlToken from ev-dispatcher.h -- KEEP CONSISTENT!
    private static final int CT_void            = 0;
    private static final int CT_boolean         = 1;
    private static final int CT_byte            = 2;
    private static final int CT_char            = 3;
    private static final int CT_short           = 4;
    private static final int CT_int             = 5;
    private static final int CT_long            = 6;
    private static final int CT_double          = 7;
    private static final int CT_float           = 8;
    private static final int CT_string          = 9;
    private static final int CT_Hashcode        = 10;
    private static final int CT_ArrayLength     = 11;
    private static final int CT_PptTraceStart   = 12;
    private static final int CT_PptTraceEnd     = 13;
    private static final int CT_PptDeclStart    = 14;
    private static final int CT_PptDeclEnd      = 15;
    private static final int CT_NoData          = 16;
    private static final int CT_EOF             = 17;

    private static final String[] types = new String[] {
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
        "PptTraceStart",
        "PptTraceEnd",
        "PptDeclStart",
        "PptDeclEnd",
        "NoData",
        "EOF"
    };
}

/*
 * Local Variables:
 * c-basic-offset:      4
 * c-indentation-style: "stroustrup"
 * End:
 */
