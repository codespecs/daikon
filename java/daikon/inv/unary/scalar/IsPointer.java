package daikon.inv.unary.scalar;

import utilMDE.Assert;
import daikon.PptSlice;
import daikon.ProglangType;
import daikon.VarInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.ValueSet;

/**
 * IsPointer is an invariant that heuristically determines whether 
 * an integer represents a pointer (a 32-bit memory address). 
 * Since both a 32-bit integer and an address have the same representation, 
 * sometimes a a pointer can be mistaken for an integer. When this happens,
 * several scalar invariants are computed for integer variables. Most of them
 * would not make any sense for pointers. Determining whether a 32-bit variable
 * is a pointer can thus spare the computation of many irrelevant invariants. 
 * 
 * Jeff suggested the following heuristic: if all samples observed for an 
 * integer variable are greater than 100000 or smaller than -100000, then
 * it is likely that the integer variable represents a pointer (also,
 * we allow 0 as a valid value for a pointer). This heuristic was confirmed
 * by observing the sample values for those variables that we knew for sure
 * that are pointers (e.g., their file_rep_type == HASHCODE and type == *data).
 *
 */
public class IsPointer extends SingleScalar {

    private static IsPointer proto;

    private static final long serialVersionUID = 20080221L;
    
    public static boolean dkconfig_enabled = true;

    // pointers values in ff_prepare.dtrace were starting from 65536,
    // in other dtrace files, the pointer values were even larger
    private long largestNonPointerValue = 100000;

    private long smallestNonPointerValue = -100000;

    protected IsPointer(PptSlice ppt) {
        super(ppt);
        
    }
    

    /** Returns the prototype invariant for NonZero **/
    public static Invariant get_proto() {
      if (proto == null)
        proto = new IsPointer(null);
      return (proto);
    }

    @Override
    protected Invariant instantiate_dyn(PptSlice slice) {
        return new IsPointer(slice); 
    }
    
    @Override
    public boolean enabled() {
        return dkconfig_enabled;
    } 
    
    @Override
    public boolean instantiate_ok(VarInfo[] vis) {
        if (!super.valid_types(vis))
            return false;

        ProglangType file_rep_type = vis[0].file_rep_type;
        
        return (file_rep_type == ProglangType.INT);
    }

    
    @Override
    public InvariantStatus add_modified(long value, int count) {
        return check_modified(value, count);
    }


    public InvariantStatus check_modified(long v, int count) {
        if (!isWithinPointerRange(v))
            return InvariantStatus.FALSIFIED;
        return InvariantStatus.NO_CHANGE;
      }
    
    private boolean isWithinPointerRange(long value) {
        if (value == 0)
            return true;
        return (value >= largestNonPointerValue) || (value <= smallestNonPointerValue); 
    }

    
    @Override
    public String format() {
        String name = var().jml_name();
        ValueSet.ValueSetScalar vs = (ValueSet.ValueSetScalar) ppt.var_infos[0].get_value_set();
        return name + " isPointer"; 
    }
    
    @Override
    public String repr() {
        return format();
    }
    
    @Override
    public String format_using(OutputFormat format) {
        return format();
    }
    
    protected double computeConfidence() {
        return 1 - computeProbability();
    }

    // computes the probability that this is the result
    // of chance
    protected double computeProbability() {
        Assert.assertTrue(!falsified);

        ValueSet.ValueSetScalar vs = (ValueSet.ValueSetScalar) ppt.var_infos[0]
                .get_value_set();

        if ((!isWithinPointerRange(vs.max()))
                || (!isWithinPointerRange(vs.min()))) {
            return Invariant.PROBABILITY_UNJUSTIFIED;
        }

        return Invariant.PROBABILITY_JUSTIFIED;
    }
}
