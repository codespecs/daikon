/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.typechecker;

import ajax.jbc.*;
import ajax.Globals;
import java.util.*;
import ajax.util.*;

/**
The BytecodeTypechecker computes types for local variables and stack locations in a
Java method. It does not currently do any actual type checking.
<p>
The getLocalVarTypes and getStackElemTypes methods return a set of types for the
requested variable or stack element. The actual value of the variable or stack element
could belong to one or more of the types in the set. The returned objects are subclasses
of JBCType, except for PreciseClassTypes if they are enabled (see below). The JBCType.OBJECT
type means "the type of null". All other JBCObjectTypes, those with specific classes,
mean that the value could have that class or any class which is a subclass of
the specified class. jsr return addresses are given type ReturnAddrType. Note that
booleans, bytes, shorts and chars never appear in stack or local variable types because
they're automatically promoted and demoted to and from int in the virtual machine.
<p>
If the code location is unreachable or the variable or stack element is uninitialized
at the specified location, the set of possible types will be empty.
*/
public class BytecodeTypechecker implements OpcodeConstants {
    private JBCMethod method;
    
/**
Maps instruction offsets -> indexes -> optional subroutine stack -> set of types
*/
    private Object[][] stackTypeSets;
    private Object[][] varTypeSets;
    private CompactSet[] instructionExecutionContexts;
    private int maxLocals;
    private int maxStack;
    private CompactSet updatesNeeded = null;
    private boolean enablePreciseClasses = false;
    private boolean trackInstanceOf = false;
    private VarNumbering numbering;
    private boolean comparedIncomparableObjectTypes = false;
    
    private CatchBlockData[] catches;
    
    private CompactSet getLocalVarTypeSet(int offset, int index, CallStack context) {
        return getTypeSet(varTypeSets, offset, index, context, maxLocals);
    }
        
    private CompactSet getStackElemTypeSet(int offset, int index, CallStack context) {
        return getTypeSet(stackTypeSets, offset, index, context, maxStack);
    }
        
    private CompactSet getTypeSet(Object[][] sets, int offset, int index, CallStack context,
        int numIndices) {
        Object[] varTypes = sets[offset];
        
        if (varTypes == null) {
            varTypes = new Object[numIndices];
            sets[offset] = varTypes;
        }
        
        Object types = varTypes[index];
        
        if (types == null) {
            types = context.isRoot() ? (Object)(new CompactSet()) : (Object)(new Hashtable());
            varTypes[index] = types;
        }
        
        Hashtable table;
        
        if (types instanceof CompactSet) {
            if (context.isRoot()) {
                return (CompactSet)types;
            } else {
                table = new Hashtable();
                
                varTypes[index] = table;
                table.put(context.getRoot(), types);
            }
        } else {
            table = (Hashtable)types;
        }
        
        CompactSet set = (CompactSet)table.get(context);
        
        if (set == null) {
            set = new CompactSet();
            table.put(context, set);
        }
        
        return set;
    }
    
    private void initParameterTypes(CallStack rootContext, byte[] code) {
        JBCType[] parameterTypes = method.getMethodType().getParameterTypes();
        int index = 0;
        
        if (Globals.debug && !method.isStatic() && !parameterTypes[0].equals(method.getContainingClass().getClassType())) {
            Globals.nonlocalError("Bug in method typing code?");
            parameterTypes = method.getMethodType().getParameterTypes();
        }
        
        for (int i = 0; i < parameterTypes.length; i++) {
            defLocalVarType(0, index, rootContext, parameterTypes[i], code);
            index += parameterTypes[i].getWordSize();
        }
    }
    
/**
Create a typechecker for a method. The type information is computed
immediately.

@param method the method to check
*/
    public BytecodeTypechecker(JBCMethod method) {
        this.method = method;
    }
    
    public boolean isPreciseClassTypes() {
        return enablePreciseClasses;
    }
    
    public void setEnablePreciseClasses(boolean enablePreciseClasses) {
        this.enablePreciseClasses = enablePreciseClasses;
    }
    
    public void setTrackInstanceOf(boolean trackInstanceOf) {
        this.trackInstanceOf = trackInstanceOf;
    }
    
    public void execute() {
        CallStack rootContext = new CallStack();
        MethodData data = method.getData();
        byte[] code = data.getCode();
        int codeLen = code.length;
        
        catches = method.getData().getCatchBlocks();
        
        stackTypeSets = new Object[codeLen][];
        varTypeSets = new Object[codeLen][];
        instructionExecutionContexts = new CompactSet[codeLen];
        maxLocals = data.getMaxLocalWords();
        maxStack = data.getMaxStackWords();
        
        if (trackInstanceOf) {
            numbering = new VarNumbering(method);
        } else {
            numbering = null;
        }
        
        initParameterTypes(rootContext, code);
        addInstructionExecution(0, rootContext, code);
        
        while (workUpdateInstructionExecutions(code)) {
        }
    }
    
/**
Returns the method that's been typechecked.

@return the method
*/
    public JBCMethod getMethod() {
        return method;
    }

    public boolean didCompareIncomparableObjectTypes() {
        return comparedIncomparableObjectTypes;
    }
    
    private boolean isApplicableReturnAddressType(Object type, CallStack context) {
        if (type instanceof ReturnAddrType) {
            int retAddr = ((ReturnAddrType)type).getReturnAddress();
            CallStack s;
            
            for (s = context; !s.isRoot() && s.getReturnAddress() != retAddr; s = s.getParent()) {
            }
            
            return !s.isRoot();
        } else {
            return false;
        }
    }
    
    private int[] getSuccessors(int offset, CallStack context, byte[] code) {
        int[] successors = JBCCodeUtilities.getReachableSuccessors(code, offset);
        
        switch (code[offset] & 0xFF) {
            case OP_jsr:
            case OP_jsr_w:
 /* In our view of the world, a jsr has two successors: the subroutine entry point
    with a new context, and the subroutine entry point in the current context (in case
    the subroutine just jumps out without returning).
    
    Recursive subroutine calls are not allowed; if the return-to address is already being used
    by an activation on the current context stack, then this instruction has no successors.
 */
                CallStack s;
                
                for (s = context; !s.isRoot() && s.getReturnAddress() != successors[0]; s = s.getParent()) {
                }
                
                if (s.isRoot()) {
                    successors[0] = successors[1];
                } else {
                    successors = new int[0];
                }
                break;

            case OP_wide:
                if ((code[offset + 1] & 0xFF) != OpcodeConstants.OP_ret) {
                    break;
                } /* else fall through */
            case OP_ret: {
                CompactSet retTypes = getLocalVarTypeSet(offset, JBCCodeUtilities.getLocalVariableLoadIndex(code, offset), context);
                Vector successorList = new Vector();
                
                for (Enumeration e = retTypes.elements(); e.hasMoreElements();) {
                    Object t = e.nextElement();
                    
                    if (isApplicableReturnAddressType(t, context)) {
                        successorList.addElement(new Integer(((ReturnAddrType)t).getReturnAddress()));
                    }
                }
                
                successors = new int[successorList.size()];
                for (int i = 0; i < successors.length; i++) {
                    successors[i] = ((Integer)successorList.elementAt(i)).intValue();
                }
                break;
            }
        }
        
        return successors;
    }

    private CallStack[] getSuccessorContexts(int offset, CallStack context, int[] successors, byte[] code) {
        CallStack[] result = new CallStack[successors.length];
        
        for (int i = 0; i < result.length; i++) {
            result[i] = context;
        }
        
        switch (code[offset] & 0xFF) {
            case OP_jsr:
            case OP_jsr_w: {
 /* In our view of the world, a jsr has two successors: the subroutine entry point
    with a new context, and the subroutine entry point in the current context (in case
    the subroutine just jumps out without returning).
    
    Recursive subroutine calls are not allowed; if the return-to address is already being used
    by an activation on the current context stack, then this instruction has no successors.
*/
                if (result.length > 0) {
                    result[0] = new CallStack(context, JBCCodeUtilities.getReachableSuccessors(code, offset)[0]);
                }
                break;
            }
            
            case OP_wide:
                if ((code[offset + 1] & 0xFF) != OpcodeConstants.OP_ret) {
                    break;
                } /* else fall through */
            case OP_ret: {
                CompactSet retTypes = getLocalVarTypeSet(offset, JBCCodeUtilities.getLocalVariableLoadIndex(code, offset), context);
                Vector resultList = new Vector();
                
                for (Enumeration e = retTypes.elements(); e.hasMoreElements();) {
                    Object t = e.nextElement();
                    
                    if (isApplicableReturnAddressType(t, context)) {
                        int retAddr = ((ReturnAddrType)t).getReturnAddress();
                        CallStack s;
                        
                        for (s = context; s.getReturnAddress() != retAddr; s = s.getParent()) {
                        }
                        
                        resultList.addElement(s.getParent());
                    }
                }
                
                resultList.copyInto(result);
                break;
            }
        }
        
        return result;
    }
    
    private Object findClassIntersection(JBCClass c, Object t) {
        if (t instanceof PreciseClassType) {
            JBCClass c2 = ((PreciseClassType)t).getObjectType().getClassDef();
            
            if (c2.isSubclassOf(c)) {
                return t;
            } else {
                return null;
            }
        } else if (t instanceof JBCObjectType) {
            JBCClass c2 = ((JBCObjectType)t).getClassDef();
            
            if (c.isSubclassOf(c2)) {
                return c.getClassType();
            } else if (c2.isSubclassOf(c)) {
                return t;
            } else if (!c.isInterface() && !c2.isInterface()) {
                return null;
            } else {
                // give it the class of the constraint, since the
                // code is likely to use operations on that class
                return c.getClassType();
            }
        } else {
            Globals.userError("Taking instanceof a non-object type?");
            return null;
        }
    }

    private Object adjustTypeAlongEdge(NumberedVar v, int offset, int successorNum, Object type, byte[] code) {
        if (v == null) {
            return type;
        } else {
            int opcode = code[offset] & 0xFF;
            
            switch (opcode) {
                case OP_ifeq:
                case OP_ifne:
                case OP_iflt:
                case OP_ifge:
                case OP_ifgt:
                case OP_ifle: {
                    NumberedVar condition = numbering.getStackNumber(offset, 0);
                    
                    if (condition instanceof NumberedVarComputation
                        && successorNum == (JBCCodeUtilities.isBranchTaken(opcode, 1) ? 1 : 0)) {
                        NumberedVarComputation c = (NumberedVarComputation)condition;
                        int computationOffset = c.getOffset();
                        int computationOpcode = code[computationOffset] & 0xFF;
                        
                        if (computationOpcode == OP_instanceof) {
                            JBCClass cl = JBCCodeUtilities.resolveInstructionClass(method, code, computationOffset);
                            
                            if (cl != null) {
                                NumberedVar instanceOperand = numbering.getStackNumber(computationOffset, 0);
                                
                                if (instanceOperand != null
                                    && instanceOperand.equals(v)) {
                                    return findClassIntersection(cl, type);
                                }
                            }
                        }
                    }
                    break;
                }
            }
            
            return type;
        }
    }
    
    private void defLocalVarType(int offset, int index, CallStack context, Object type, byte[] code) {
        while (true) {
            CompactSet set = getLocalVarTypeSet(offset, index, context);
            
            if (set.get(type) == null) {
                set.addUnconditionally(type);
                
                if (JBCCodeUtilities.isLocalVariableLoaded(code, offset, index)) {
                    updateInstructionExecution(offset, context, code);
                }
                
                for (int i = 0; i < catches.length; i++) {
                    CatchBlockData catchData = catches[i];
                    
                    if (catchData.getStartPC() <= offset && offset < catchData.getEndPC()) {
                        defLocalVarType(catchData.getHandlerPC(), index, context, type, code);
                    }
                }
                
                if (!JBCCodeUtilities.isLocalVariableStored(code, offset, index)) {
                    int[] successors = getSuccessors(offset, context, code);
                    CallStack[] contexts = getSuccessorContexts(offset, context, successors, code);
                    NumberedVar v = numbering == null ? null : numbering.getVariableNumber(offset, index);

                    for (int i = 1; i < successors.length; i++) {
                        Object t = adjustTypeAlongEdge(v, offset, i, type, code);
                        
                        if (t != null) {
                            defLocalVarType(successors[i], index, contexts[i], t, code);
                        }
                    }
                    
                    if (successors.length >= 1) {
                        type = adjustTypeAlongEdge(v, offset, 0, type, code);
                        offset = successors[0];
                        context = contexts[0];
                        
                        if (type != null) {
                            continue;
                        }
                    }
                }
            }
            
            return;
        }
    }
    
    private void defStackElemType(int offset, int index, CallStack context, Object type, byte[] code) {
        while (true) {
            CompactSet set = getStackElemTypeSet(offset, index, context);
            
            if (set.get(type) == null) {
                set.addUnconditionally(type);
                
                int stackSizeDelta = JBCCodeUtilities.getStackSizeDelta(method, code, offset);
                int stackPushCount = JBCCodeUtilities.getStackPushCount(method, code, offset);
                int stackPopCount = stackPushCount - stackSizeDelta;
                
                if (index < stackPopCount) {
                    updateInstructionExecution(offset, context, code);
                } else {
                    NumberedVar v = numbering == null ? null : numbering.getStackNumber(offset, index);
                    int[] successors = getSuccessors(offset, context, code);
                    CallStack[] contexts = getSuccessorContexts(offset, context, successors, code);

                    for (int i = 1; i < successors.length; i++) {
                        Object t = adjustTypeAlongEdge(v, offset, i, type, code);
                        
                        if (t != null) {
                            defStackElemType(successors[i], index + stackSizeDelta, contexts[i], t, code);
                        }
                    }
                    
                    if (successors.length >= 1) {
                        type = adjustTypeAlongEdge(v, offset, 0, type, code);
                        offset = successors[0];
                        context = contexts[0];
                        index += stackSizeDelta;
                        
                        if (type != null) {
                            continue;
                        }
                    }
                }
            }
            
            return;
        }
    }
    
    private void transferStackElemTypeSets(CallStack context, int fromOffset, int fromStackPos,
        int toOffset, int toStackPos, byte[] code) {
        for (Enumeration e = getStackElemTypeSet(fromOffset, fromStackPos, context).elements();
            e.hasMoreElements();) {
            defStackElemType(toOffset, toStackPos, context, e.nextElement(), code);
        }
    }
    
/**
This method is called to perform the type effects of an instruction. "Fall through"
values on the stack and local variables are taken care of elsewhere.
*/
    private void updateInstructionExecution(int offset, CallStack context, byte[] code) {
        if (updatesNeeded == null) {
            updatesNeeded = new CompactSet();
        }
        
        updatesNeeded.add(new ContextOffsetPair(context, offset));
    }
    
    private boolean workUpdateInstructionExecutions(byte[] code) {
        if (updatesNeeded == null) {
            return false;
        } else {
            Enumeration e = updatesNeeded.elements();
            
            updatesNeeded = null;
            while (e.hasMoreElements()) {
                ContextOffsetPair p = (ContextOffsetPair)e.nextElement();
                int offset = p.getOffset();
                CallStack context = p.getContext();
                
                updateInstructionExecution(offset, context, code, getSuccessors(offset, context, code));
            }
            return true;
        }
    }

    private boolean isAssignableTo(Object fromType, Object toType) {
        if (toType instanceof JBCObjectType) {
	    if (toType.equals(JBCType.OBJECT)) {
                return fromType.equals(JBCType.OBJECT);
	    } else {
                JBCClass fromClass;

                if (fromType.equals(JBCType.OBJECT)) {
                    return true;
                } else if (fromType instanceof JBCObjectType) {
                    fromClass = ((JBCObjectType)fromType).getClassDef();
		} else if (fromType instanceof PreciseClassType) {
                    fromClass = ((PreciseClassType)fromType).getObjectType()
                        .getClassDef();
		} else {
                    return false;
		}
                
                return fromClass.isSubclassOf(
                    ((JBCObjectType)toType).getClassDef());
	    }
        } else if (toType instanceof PreciseClassType) {
	    if (fromType instanceof JBCObjectType) {
  	        return fromType.equals(JBCType.OBJECT);
	    } else {
                return fromType.equals(toType);
	    }
	} else {
            return fromType.equals(toType);
	}
    }

/**
This method is called to perform the type effects of an instruction. "Fall through"
values on the stack and local variables are taken care of elsewhere.
*/
    private void updateInstructionExecution(int offset, CallStack context, byte[] code, int[] successors) {
        int opcode = code[offset] & 0xFF;
        
        if (opcode == OP_wide) {
            opcode = code[offset + 1] & 0xFF;
        }
        
        switch (opcode) {
            case OP_aconst_null:
                defStackElemType(successors[0], 0, context, JBCType.OBJECT, code);
                break;

            case OP_baload:
            case OP_caload:
            case OP_saload:
            case OP_iaload:
            case OP_iload_0:
            case OP_iload_1:
            case OP_iload_2:
            case OP_iload_3:
            case OP_iload:
            case OP_bipush:
            case OP_sipush:
            case OP_iconst_m1:   
            case OP_iconst_0:
            case OP_iconst_1:
            case OP_iconst_2:
            case OP_iconst_3:
            case OP_iconst_4:
            case OP_iconst_5:
            case OP_iadd:
            case OP_isub:
            case OP_imul:
            case OP_idiv:
            case OP_irem:
            case OP_ineg:
            case OP_ishl:
            case OP_ishr:
            case OP_iushr:
            case OP_iand:
            case OP_ior:
            case OP_ixor:
            case OP_l2i:
            case OP_f2i:
            case OP_d2i:
            case OP_i2b:
            case OP_i2c:
            case OP_i2s:
            case OP_lcmp:
            case OP_fcmpl:
            case OP_fcmpg:
            case OP_dcmpl:
            case OP_dcmpg:
            case OP_arraylength:
            case OP_instanceof:
                defStackElemType(successors[0], 0, context, JBCType.INT, code);
                break;
                
            case OP_laload:
            case OP_lload_0:
            case OP_lload_1:
            case OP_lload_2:
            case OP_lload_3:
            case OP_lload:
            case OP_lconst_0:
            case OP_lconst_1:
            case OP_ladd:
            case OP_lsub:
            case OP_lmul:
            case OP_ldiv:
            case OP_lrem:
            case OP_lneg:
            case OP_lshl:
            case OP_lshr:
            case OP_lushr:
            case OP_land:
            case OP_lor:
            case OP_lxor:
            case OP_i2l:
            case OP_d2l:
            case OP_f2l:
                defStackElemType(successors[0], 0, context, JBCType.LONG, code);
                break;

            case OP_faload:
            case OP_fload_0:
            case OP_fload_1:
            case OP_fload_2:
            case OP_fload_3:
            case OP_fload:
            case OP_fconst_0:
            case OP_fconst_1:
            case OP_fconst_2:
            case OP_fadd:
            case OP_fsub:
            case OP_fmul:
            case OP_fdiv:
            case OP_frem:
            case OP_fneg:
            case OP_i2f:
            case OP_l2f:
            case OP_d2f:
                defStackElemType(successors[0], 0, context, JBCType.FLOAT, code);
                break;

            case OP_daload:
            case OP_dload_0:
            case OP_dload_1:
            case OP_dload_2:
            case OP_dload_3:
            case OP_dload:
            case OP_dconst_0:
            case OP_dconst_1:
            case OP_dadd:
            case OP_dsub:
            case OP_dmul:
            case OP_ddiv:
            case OP_drem:
            case OP_dneg:
            case OP_i2d:
            case OP_l2d:
            case OP_f2d:
                defStackElemType(successors[0], 0, context, JBCType.DOUBLE, code);
                break;

            case OP_ldc:
            case OP_ldc_w:
            case OP_ldc2_w:
                defStackElemType(successors[0], 0, context,
                    JBCCodeUtilities.getCodeConstantType(method, code, offset), code);
                break;

            case OP_aload:
            case OP_aload_0:
            case OP_aload_1:
            case OP_aload_2:
            case OP_aload_3: {
                for (Enumeration e = getLocalVarTypeSet(offset,
                        JBCCodeUtilities.getLocalVariableLoadIndex(code, offset), context).elements();
                     e.hasMoreElements();) {
                     Object t = e.nextElement();
                     
                     if (t instanceof JBCObjectType || t instanceof ReturnAddrType || t instanceof PreciseClassType) {
                         defStackElemType(successors[0], 0, context, t, code);
                     }
                }
                break;
            }
            
            case OP_aaload: {
                for (Enumeration e = getStackElemTypeSet(offset, 1, context).elements();
                     e.hasMoreElements();) {
                     Object t = e.nextElement();
                     
                     if (t instanceof JBCObjectType || t instanceof PreciseClassType) {
                         JBCType elemType;
                         
                         if (t instanceof JBCObjectType) {
                             elemType = ((JBCObjectType)t).getArrayElementType();
                         } else {
                             elemType = ((PreciseClassType)t).getObjectType().getArrayElementType();
                         }
                         
                         if (elemType instanceof JBCObjectType || t instanceof PreciseClassType) {
                             defStackElemType(successors[0], 0, context, elemType, code);
                         }
                     }
                }
                break;
            }
            
            case OP_istore_0:
            case OP_istore_1:
            case OP_istore_2:
            case OP_istore_3:
            case OP_istore:
            case OP_iinc:
                defLocalVarType(successors[0],
                    JBCCodeUtilities.getLocalVariableStoreIndex(code, offset),
                    context, JBCType.INT, code);
                break;

            case OP_lstore_0:
            case OP_lstore_1:
            case OP_lstore_2:
            case OP_lstore_3:
            case OP_lstore:
                defLocalVarType(successors[0],
                    JBCCodeUtilities.getLocalVariableStoreIndex(code, offset),
                    context, JBCType.LONG, code);
                break;

            case OP_fstore_0:
            case OP_fstore_1:
            case OP_fstore_2:
            case OP_fstore_3:
            case OP_fstore:
                defLocalVarType(successors[0],
                    JBCCodeUtilities.getLocalVariableStoreIndex(code, offset),
                    context, JBCType.FLOAT, code);
                break;

            case OP_dstore_0:
            case OP_dstore_1:
            case OP_dstore_2:
            case OP_dstore_3:
            case OP_dstore:
                defLocalVarType(successors[0],
                    JBCCodeUtilities.getLocalVariableStoreIndex(code, offset),
                    context, JBCType.DOUBLE, code);
                break;

            case OP_astore:
            case OP_astore_0:
            case OP_astore_1:
            case OP_astore_2:
            case OP_astore_3: {
                int varIndex = JBCCodeUtilities.getLocalVariableStoreIndex(code, offset);
                
                for (Enumeration e = getStackElemTypeSet(offset, 0, context).elements();
                     e.hasMoreElements();) {
                     Object t = e.nextElement();
                     
                     if (t instanceof JBCObjectType || t instanceof ReturnAddrType || t instanceof PreciseClassType) {
                         defLocalVarType(successors[0], varIndex, context, t, code);
                     }
                }
                break;
            }
            
            case OP_dup:
                transferStackElemTypeSets(context, offset, 0, successors[0], 0, code);
                transferStackElemTypeSets(context, offset, 0, successors[0], 1, code);
                break;
            
            case OP_dup_x1:
                transferStackElemTypeSets(context, offset, 0, successors[0], 0, code);
                transferStackElemTypeSets(context, offset, 1, successors[0], 1, code);
                transferStackElemTypeSets(context, offset, 0, successors[0], 2, code);
                break;
                
            case OP_dup_x2:
                transferStackElemTypeSets(context, offset, 0, successors[0], 0, code);
                transferStackElemTypeSets(context, offset, 1, successors[0], 1, code);
                transferStackElemTypeSets(context, offset, 2, successors[0], 2, code);
                transferStackElemTypeSets(context, offset, 0, successors[0], 3, code);
                break;
                
            case OP_dup2:
                transferStackElemTypeSets(context, offset, 0, successors[0], 0, code);
                transferStackElemTypeSets(context, offset, 1, successors[0], 1, code);
                transferStackElemTypeSets(context, offset, 0, successors[0], 2, code);
                transferStackElemTypeSets(context, offset, 1, successors[0], 3, code);
                break;
            
            case OP_dup2_x1:
                transferStackElemTypeSets(context, offset, 0, successors[0], 0, code);
                transferStackElemTypeSets(context, offset, 1, successors[0], 1, code);
                transferStackElemTypeSets(context, offset, 2, successors[0], 2, code);
                transferStackElemTypeSets(context, offset, 0, successors[0], 3, code);
                transferStackElemTypeSets(context, offset, 1, successors[0], 4, code);
                break;
            
            case OP_dup2_x2:
                transferStackElemTypeSets(context, offset, 0, successors[0], 0, code);
                transferStackElemTypeSets(context, offset, 1, successors[0], 1, code);
                transferStackElemTypeSets(context, offset, 2, successors[0], 2, code);
                transferStackElemTypeSets(context, offset, 3, successors[0], 3, code);
                transferStackElemTypeSets(context, offset, 0, successors[0], 4, code);
                transferStackElemTypeSets(context, offset, 1, successors[0], 5, code);
                break;
            
            case OP_swap:
                transferStackElemTypeSets(context, offset, 0, successors[0], 1, code);
                transferStackElemTypeSets(context, offset, 1, successors[0], 0, code);
                break;
                
            case OP_jsr:
            case OP_jsr_w:
                if (successors.length > 0) {
                    CallStack[] contexts = getSuccessorContexts(offset, context, successors, code);
                    
                    defStackElemType(successors[0], 0, contexts[0],
                        new ReturnAddrType(contexts[0].getReturnAddress()), code);
                }
                break;

            case OP_ret: {
                CallStack[] contexts = getSuccessorContexts(offset, context, successors, code);
                
                for (int i = 0; i < maxStack; i++) {
                    for (Enumeration e = getStackElemTypeSet(offset, i, context).elements();
                        e.hasMoreElements();) {
                        Object type = e.nextElement();
                        
                        for (int j = 0; j < successors.length; j++) {
                            defStackElemType(successors[j], i, contexts[j], type, code);
                        }
                    }
                }
                for (int i = 0; i < maxLocals; i++) {
                    for (Enumeration e = getLocalVarTypeSet(offset, i, context).elements();
                        e.hasMoreElements();) {
                        Object type = e.nextElement();
                        
                        for (int j = 0; j < successors.length; j++) {
                            defLocalVarType(successors[j], i, contexts[j], type, code);
                        }
                    }
                }
                break;
            }
            
            case OP_getstatic:
            case OP_getfield: {
                JBCField f = JBCCodeUtilities.resolveInstructionField(method, code, offset);
            
                if (f != null) {
                    defStackElemType(successors[0], 0, context, f.getFieldType(), code);
                }
                break;
            }
                
            case OP_if_acmpeq:
  	    case OP_if_acmpne: {
	      // we don't really need to do anything here to adjust
              // the typestate. But we're going
              // to make sure that the comparison is between comparable
              // object types just so that RTA (or other clients) can know
              // whether the type system is being respected here
                for (Enumeration e = getStackElemTypeSet(offset, 0, context).elements(); e.hasMoreElements();) {
                    Object t = e.nextElement();
		    for (Enumeration e2 = getStackElemTypeSet(offset, 1, context).elements(); e2.hasMoreElements();) {
		        Object t2 = e2.nextElement();
                        if (!isAssignableTo(t, t2) && !isAssignableTo(t2, t)) {
                            Globals.writeLog(this,
            "Found comparison of incomparable object types " + t + ", " + t2);
                            comparedIncomparableObjectTypes = true;
			}
		    }
		}
                break;
	    }
            
            case OP_invokevirtual:
            case OP_invokespecial:
            case OP_invokestatic:
            case OP_invokeinterface: {
                JBCMethod m = JBCCodeUtilities.resolveInstructionMethod(method, code, offset);
                
                if (m != null) {
                    JBCType returnType = m.getMethodType().getReturnType();

                    if (!returnType.equals(JBCType.VOID)) {
                        defStackElemType(successors[0], 0, context, returnType, code);
                    }
                }
                break;
            }
            
            case OP_new:
            case OP_newarray:
            case OP_anewarray:
            case OP_multianewarray: {
                JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, offset);
                
                if (c != null) {
                    if (enablePreciseClasses) {
                        defStackElemType(successors[0], 0, context,
                            PreciseClassType.get(c.getClassType()), code);
                    } else {
                        defStackElemType(successors[0], 0, context,
                            c.getClassType(), code);
                    }
                }
                break;
            }
                
            case OP_checkcast: {
                JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, offset);
                
                if (c != null) {
                    defStackElemType(successors[0], 0, context,
                        c.getClassType(), code);
                }
                break;
            }
        }
    }
    
    private void addCatchExecutions(int offset, CallStack context, byte[] code) {
        for (int i = 0; i < catches.length; i++) {
            CatchBlockData catchData = catches[i];
            
            if (catchData.getStartPC() <= offset && offset < catchData.getEndPC()) {
                String catchClassName = catchData.getCatchType();
                JBCClassLoader loader = method.getContainingClass().getClassLoader();
                JBCObjectType catchClassType =
                    JBCObjectType.get(catchClassName == null ? "java.lang.Throwable" : catchClassName, loader);
                int handlerPC = catchData.getHandlerPC();
                
                addInstructionExecution(handlerPC, context, code);
                defStackElemType(handlerPC, 0, context, catchClassType, code);
            }
        }
    }
    
    private void addInstructionExecution(int offset, CallStack context, byte[] code) {
        while (true) {
            if (instructionExecutionContexts[offset] == null) {
                instructionExecutionContexts[offset] = new CompactSet();
            }
            if (instructionExecutionContexts[offset].get(context) == null) {
                instructionExecutionContexts[offset].addUnconditionally(context);
                
                int[] successors = getSuccessors(offset, context, code);
                CallStack[] contexts = getSuccessorContexts(offset, context, successors, code);
                
                updateInstructionExecution(offset, context, code, successors);
                
                for (int i = 1; i < successors.length; i++) {
                    addInstructionExecution(successors[i], contexts[i], code);
                }
                
                addCatchExecutions(offset, context, code);
                
                if (successors.length > 0) {
                    offset = successors[0];
                    context = contexts[0];
                    continue;
                }
            }
            return;
        }
    }
    
    private CompactSet getAllTypesSet(Object[][] sets, int offset, int index) {
        Object[] varTypes = sets[offset];
        
        if (varTypes == null) {
            return null;
        } else {
            Object types = varTypes[index];
            
            if (types == null) {
                return null;
            } else if (types instanceof CompactSet) {
                return (CompactSet)types;
            } else {
                CompactSet result = new CompactSet();
                
                for (Enumeration e = ((Hashtable)types).elements(); e.hasMoreElements();) {
                    for (Enumeration e2 = ((CompactSet)e.nextElement()).elements(); e2.hasMoreElements();) {
                        result.add(e2.nextElement());
                    }
                }
                
                return result;
            }
        }
    }
    
/**
Get the set of types that may appear in a given local variable.

@param offset the offset of the bytecode instruction to check
@param index the index of the local variable
*/
    public Enumeration getLocalVarTypes(int offset, int index) {
        CompactSet result = getAllTypesSet(varTypeSets, offset, index);
        
        if (result != null) {
            return result.elements();
        } else {
            return EmptyEnumerator.get();
        }
    }

/**
Get the set of types that may appear in a given stack element.

@param offset the offset of the bytecode instruction to check
@param index the index of the stack element (0 means top of stack)
*/
    public Enumeration getStackElemTypes(int offset, int index) {
        CompactSet result = getAllTypesSet(stackTypeSets, offset, index);
        
        if (result != null) {
            return result.elements();
        } else {
            return EmptyEnumerator.get();
        }
    }
}
