/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

class JBCInstructionInfo {
/**
A 'block' is a set of instructions containing a distinguished instruction,
the "head". All non-head instructions of a block have one predecessor; it must
also be in the block. Since non-head instructions in a block have only one
predecessor, control transfer to them can be treated as a monomorphic function
call. Getting rid of useless polymorphism this way saves a lot of time and
resources.
*/
    JBCBlockInfo block = null;
    Object stackVarsChanged;
    
/**
This array contains the indices of the data for the catch blocks that can
catch an exception thrown by this instruction. There is also a '-1' entry if the
exception can propagate out of the method. This field may be null if there are no
catch successors.
*/
    int[] catchSuccessors;
}
