/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semantics;

import ajax.jbc.*;

/*
This class represents a combining semantics for the value-point relation.
Each created value has an identity assigned at creation. The identities on
two values can be unified if certain operations are performed on both of them.
<p>
This class assigns two independent properties to each operator:
<ul>
<li>Whether or not the operator combines all its inputs
<li>Whether or not the operator combines its first input with all its outputs
(the first input is the one that is computed first, i.e. it is farthest from
the top of stack)
</ul>
Each bytecode instruction is considered an operator.
*/
public class CombiningSemantics extends Semantics implements OpcodeConstants {
    public CombiningSemantics() {
    }
    
/* Does the output combine with the first input? */
    public boolean isCombiningInputWithOutput(byte opcode) {
        switch (opcode & 0xFF) {
            case OP_iadd:
            case OP_ladd:
            case OP_fadd:
            case OP_dadd:
            case OP_isub:
            case OP_lsub:
            case OP_fsub:
            case OP_dsub:
                return true;

            case OP_imul:
            case OP_lmul:
            case OP_fmul:
            case OP_dmul:
            case OP_idiv:
            case OP_ldiv:
            case OP_fdiv:
            case OP_ddiv:
            case OP_irem:
            case OP_lrem:
            case OP_frem:
            case OP_drem:
                return false;

            case OP_ineg:
            case OP_lneg:
            case OP_fneg:
            case OP_dneg:
            case OP_ishl:
            case OP_lshl:
            case OP_ishr:
            case OP_lshr:
            case OP_iushr:
            case OP_lushr:
            case OP_iand:
            case OP_land:
            case OP_ior:
            case OP_lor:
            case OP_ixor:
            case OP_lxor:
                return true;

            case OP_i2l:
            case OP_i2f:
            case OP_i2d:
            case OP_l2i:
            case OP_l2f:
            case OP_l2d:
            case OP_f2i:
            case OP_f2l:
            case OP_f2d:
            case OP_d2i:
            case OP_d2l:
            case OP_d2f:
            case OP_i2b:
            case OP_i2c:
            case OP_i2s:
                return true;

            case OP_lcmp:
            case OP_fcmpl:
            case OP_fcmpg:
            case OP_dcmpl:
            case OP_dcmpg:
                return false;

            case OP_getfield:
            case OP_arraylength:
            case OP_instanceof:
                return false;

            default:
                return false;
        }
    }

    public boolean isTrackingArrayIndices() {
	return true;
    }
   
/* Do the two input operands combine? */
    public boolean isCombiningInputs(byte opcode) {
        switch (opcode & 0xFF) {
            case OP_iadd:
            case OP_ladd:
            case OP_fadd:
            case OP_dadd:
            case OP_isub:
            case OP_lsub:
            case OP_fsub:
            case OP_dsub:
                return true;
                
            case OP_imul:
            case OP_lmul:
            case OP_fmul:
            case OP_dmul:
            case OP_idiv:
            case OP_ldiv:
            case OP_fdiv:
            case OP_ddiv:
            case OP_irem:
            case OP_lrem:
            case OP_frem:
            case OP_drem:
                return false;
                
            case OP_ishl:
            case OP_lshl:
            case OP_ishr:
            case OP_lshr:
            case OP_iushr:
            case OP_lushr:
            case OP_iand:
            case OP_land:
            case OP_ior:
            case OP_lor:
            case OP_ixor:
            case OP_lxor:
                return true;
                
            case OP_lcmp:
            case OP_fcmpl:
            case OP_fcmpg:
            case OP_dcmpl:
            case OP_dcmpg:
            case OP_if_icmpeq:
            case OP_if_icmpne:
            case OP_if_icmplt:
            case OP_if_icmpge:
            case OP_if_icmpgt:
            case OP_if_icmple:
                return true;
                
            case OP_if_acmpeq:
            case OP_if_acmpne:
                return true;
                
            default:
               return false;
        }
    }
}
