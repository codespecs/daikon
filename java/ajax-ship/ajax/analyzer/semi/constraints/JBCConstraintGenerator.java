/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.*;
import ajax.analyzer.semi.*;
import ajax.analyzer.semantics.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.util.*;
import ajax.Globals;

/**
This is the heart of the system. It generates constraints from the
bytecode of a JBC method.

<h3>General Notes</h3>

Double-word values (longs and doubles) only get a variable and an abstract
value for the first word (the word at the top of the stack, 'value2', or at the given
local variable index). The second word is unused (that's the second word on
the stack, 'value1', or at the given local variable plus one).
*/
class JBCConstraintGenerator implements OpcodeConstants, DataConstants {
    private ConstraintManager manager;
    private JBCMethod method;
    private InvocationContext context;
    private int uniqueOffset = Integer.MIN_VALUE;
    
    private static final ObjectStackValue NULL_VALUE = new ObjectStackValue(null);
    private static final IntStackValue INT_M1_VALUE = new IntStackValue(-1);
    private static final IntStackValue INT_0_VALUE = new IntStackValue(0);
    private static final IntStackValue INT_1_VALUE = new IntStackValue(1);
    private static final IntStackValue INT_2_VALUE = new IntStackValue(2);
    private static final IntStackValue INT_3_VALUE = new IntStackValue(3);
    private static final IntStackValue INT_4_VALUE = new IntStackValue(4);
    private static final IntStackValue INT_5_VALUE = new IntStackValue(5);
    private static final LongStackValue LONG_0_VALUE = new LongStackValue(0);
    private static final LongStackValue LONG_1_VALUE = new LongStackValue(1);
    private static final FloatStackValue FLOAT_0_VALUE = new FloatStackValue(0.0f);
    private static final FloatStackValue FLOAT_1_VALUE = new FloatStackValue(1.0f);
    private static final FloatStackValue FLOAT_2_VALUE = new FloatStackValue(2.0f);
    private static final DoubleStackValue DOUBLE_0_VALUE = new DoubleStackValue(0.0);
    private static final DoubleStackValue DOUBLE_1_VALUE = new DoubleStackValue(1.0);
    
    JBCConstraintGenerator(ConstraintManager manager, JBCMethod method,
        InvocationContext context) {
        this.manager = manager;
        this.method = method;
        this.context = context;
    }
    
    private static void visitInstruction(JBCInstructionInfo[] instructions, byte[] code,
        int[] predecessorCount, int offset) {
        JBCInstructionInfo info = instructions[offset];
            
        if (info == null) {
            info = new JBCInstructionInfo();
            instructions[offset] = info;
            
            int[] successors = JBCCodeUtilities.getReachableSuccessors(code, offset);
            
            if (successors.length != 1) {
                for (int i = 0; i < successors.length; i++) {
                    int successor = successors[i];
                    
                    predecessorCount[successor]++;
                    visitInstruction(instructions, code, predecessorCount, successor);
                }
            } else {
                int[] singletonArray = successors;
                
                while (true) {
                    offset = successors[0];
                    predecessorCount[offset]++;
                    info = instructions[offset];
            
                    if (info == null) {
                        info = new JBCInstructionInfo();
                        instructions[offset] = info;
                        successors = JBCCodeUtilities.getReachableSuccessors(code, offset, singletonArray);
                        
                        if (successors.length != 1) {
                            for (int i = 0; i < successors.length; i++) {
                                int successor = successors[i];
                                
                                predecessorCount[successor]++;
                                visitInstruction(instructions, code, predecessorCount, successor);
                            }
                        } else {
                            continue;
                        }
                    }
                    return;
                }
            }
        }
    }
    
    private static void makeBlock(JBCInstructionInfo[] instructions, int offset,
        int predecessorCount) {
        JBCInstructionInfo info = instructions[offset];
        
        if (info.block == null) {
            JBCBlockInfo block = new JBCBlockInfo();
            
            info.block = block;
            
            block.predecessorsNotProcessed = predecessorCount;
            block.offset = offset;
        }
    }
    
    private JBCInstructionInfo[] makeInstructions(byte[] code) {
        JBCInstructionInfo[] instructions = new JBCInstructionInfo[code.length];
        int[] predecessorCount = new int[code.length];
        World w = manager.getSolver();
        
        visitInstruction(instructions, code, predecessorCount, 0);
        
        CatchBlockData[] data = method.getData().getCatchBlocks();
        
        for (int i = 0; i < data.length; i++) {
            int offset = data[i].getHandlerPC();
            
            visitInstruction(instructions, code, predecessorCount, offset);
        }
        
        Vector retInstructions = new Vector();
        
        for (int i = 0; i < instructions.length; i++) {
            if (instructions[i] != null) {
                if ((code[i] & 0xFF) == OP_ret
                    || ((code[i] & 0xFF) == OP_wide && (code[i + 1] & 0xFF) == OP_ret)) {
                    retInstructions.addElement(new Integer(i));
                    makeBlock(instructions, i, predecessorCount[i]);
                }
            }
        }
        
        int[] retInstructionArray = new int[retInstructions.size()];
        
        for (int i = 0; i < retInstructionArray.length; i++) {
            retInstructionArray[i] = ((Integer)retInstructions.elementAt(i)).intValue();
        }
        
        makeBlock(instructions, 0, predecessorCount[0]);
        
        for (int i = 0; i < data.length; i++) {
            int handlerPC = data[i].getHandlerPC();
           
            makeBlock(instructions, handlerPC, Integer.MAX_VALUE);
        }
        
        for (int i = 1; i < predecessorCount.length; i++) {
            if (instructions[i] != null) {
                if (predecessorCount[i] > 1) {
                    makeBlock(instructions, i, predecessorCount[i]);
                } 
                
                int opcode = code[i] & 0xFF;
                
                if (opcode == OP_jsr || opcode == OP_jsr_w) {
                    int returnTo = i + (opcode == OP_jsr ? 3 : 5);
                    JBCReturnInfo returnInfo = new JBCReturnInfo();
                    
                    makeBlock(instructions, returnTo, Integer.MAX_VALUE);
                    instructions[returnTo].block.returnInfo = returnInfo;
                    
                    returnInfo.retToThisInstructionOffsets = retInstructionArray;
                    returnInfo.jsrTupleVar = new Variable(w);
                    returnInfo.instance = JBCInstructionTransferInstance.get(method, i, returnTo, 0);

                    if (Globals.debug) manager.addToken(returnInfo.jsrTupleVar, new JSRTupleDebugToken(i, method));
                }
            }
        }
        
        return instructions;
    }
    
    private void addCatchBlockSuccessors(JBCInstructionInfo[] instructions, byte[] code) {
        boolean[] exceptionsDefinitelyCaught = new boolean[code.length];
        Vector[] catchSuccessors = new Vector[code.length];
        CatchBlockData[] data = method.getData().getCatchBlocks();
        JBCClassLoader loader = method.getContainingClass().getClassLoader();
        JBCClass throwableClass = manager.getSpecialClass("java.lang.Throwable");
        
        for (int i = 0; i < data.length; i++) {
            CatchBlockData block = data[i];
            String catchClassName = block.getCatchType();
            int endPC = block.getEndPC();
            JBCClass catchClass = catchClassName == null ? null : loader.getClass(catchClassName);
            Integer catchIndex = new Integer(i);
            
            for (int j = block.getStartPC(); j < endPC; j++) {
                if (instructions[j] != null
                    && !exceptionsDefinitelyCaught[j]
                    && JBCCodeUtilities.isExceptionSource(code, j, catchClass)) {
                    Vector successors = catchSuccessors[j];
                    
                    if (successors == null) {
                        successors = new Vector();
                        catchSuccessors[j] = successors;
                    }
                    successors.addElement(catchIndex);
                    
                    if (catchClass == null || catchClass.equals(throwableClass)) {
                        exceptionsDefinitelyCaught[j] = true;
                    }
                }
            }
        }
        
        for (int i = 0; i < catchSuccessors.length; i++) {
            Vector successors = catchSuccessors[i];
            boolean propagates = instructions[i] != null && !exceptionsDefinitelyCaught[i];
            
            if (successors != null || propagates) {
                int successors_size = successors != null ? successors.size() : 0;
                int[] successorArray = new int[successors_size + (propagates ? 1 : 0)];
                
                if (successors != null) {
                    for (int j = 0; j < successors_size; j++) {
                        successorArray[j] = ((Integer)successors.elementAt(j)).intValue();
                    }
                }
                
                if (propagates) {
                    successorArray[successors_size] = -1;
                }
                
                instructions[i].catchSuccessors = successorArray;
            }
        }
    }

    private static AbstractStackValue makeConstantStackValue(Object val) {
        if (val instanceof Integer) {
            return new IntStackValue(((Integer)val).intValue());
        } else if (val instanceof Double) {
            return new DoubleStackValue(((Double)val).doubleValue());
        } else if (val instanceof Float) {
            return new FloatStackValue(((Float)val).floatValue());
        } else if (val instanceof Long) {
            return new LongStackValue(((Long)val).longValue());
        } else if (val instanceof String) {
            return new ObjectStackValue(((String)val).intern());
        } else {
            Globals.localError("Invalid constant type");
            return null;
        }
    }
    
    private static int doOPlcmp(long a, long b) {
        return a < b ? -1 : (a > b ? 1 : 0);
    }
    
    private static int doOPdcmpg(double a, double b) {
        return a < b ? -1 : (a == b ? 0 : 1);
    }
    
    private static int doOPdcmpl(double a, double b) {
        return a > b ? 1 : (a == b ? 0 : -1);
    }
    
    private static AbstractStackValue doBinaryMathOp(int opcode, AbstractStackValue a,
        AbstractStackValue b) {
        switch (opcode) {
            case OP_iadd:
                return new IntStackValue(a.getIntValue() + b.getIntValue());
            case OP_ladd:
                return new LongStackValue(a.getLongValue() + b.getLongValue());
            case OP_fadd:
                return new FloatStackValue(a.getFloatValue() + b.getFloatValue());
            case OP_dadd:
                return new DoubleStackValue(a.getDoubleValue() + b.getDoubleValue());
            case OP_isub:
                return new IntStackValue(a.getIntValue() - b.getIntValue());
            case OP_lsub:
                return new LongStackValue(a.getLongValue() - b.getLongValue());
            case OP_fsub:
                return new FloatStackValue(a.getFloatValue() - b.getFloatValue());
            case OP_dsub:
                return new DoubleStackValue(a.getDoubleValue() - b.getDoubleValue());
            case OP_imul:
                return new IntStackValue(a.getIntValue() * b.getIntValue());
            case OP_lmul:
                return new LongStackValue(a.getLongValue() * b.getLongValue());
            case OP_fmul:
                return new FloatStackValue(a.getFloatValue() * b.getFloatValue());
            case OP_dmul:
                return new DoubleStackValue(a.getDoubleValue() * b.getDoubleValue());
            case OP_idiv:
                try {
                    return new IntStackValue(a.getIntValue() / b.getIntValue());
                } catch (ArithmeticException ex) {
                    return null;
                }
            case OP_ldiv:
                try {
                    return new LongStackValue(a.getLongValue() / b.getLongValue());
                } catch (ArithmeticException ex) {
                    return null;
                }
            case OP_fdiv:
                return new FloatStackValue(a.getFloatValue() / b.getFloatValue());
            case OP_ddiv:
                return new DoubleStackValue(a.getDoubleValue() / b.getDoubleValue());
            case OP_irem:
                try {
                    return new IntStackValue(a.getIntValue() % b.getIntValue());
                } catch (ArithmeticException ex) {
                    return null;
                }
            case OP_lrem:
                try {
                    return new LongStackValue(a.getLongValue() % b.getLongValue());
                } catch (ArithmeticException ex) {
                    return null;
                }
            case OP_frem:
                return new FloatStackValue(a.getFloatValue() % b.getFloatValue());
            case OP_drem:
                return new DoubleStackValue(a.getDoubleValue() % b.getDoubleValue());
            case OP_ishl:
                return new IntStackValue(a.getIntValue() << b.getIntValue());
            case OP_lshl:
                return new LongStackValue(a.getLongValue() << b.getIntValue());
            case OP_ishr:
                return new IntStackValue(a.getIntValue() >> b.getIntValue());
            case OP_lshr:
                return new LongStackValue(a.getLongValue() >> b.getIntValue());
            case OP_iushr:
                return new IntStackValue(a.getIntValue() >>> b.getIntValue());
            case OP_lushr:
                return new LongStackValue(a.getLongValue() >>> b.getIntValue());
            case OP_iand:
                return new IntStackValue(a.getIntValue() & b.getIntValue());
            case OP_land:
                return new LongStackValue(a.getLongValue() & b.getLongValue());
            case OP_ior:
                return new IntStackValue(a.getIntValue() | b.getIntValue());
            case OP_lor:
                return new LongStackValue(a.getLongValue() | b.getLongValue());
            case OP_ixor:
                return new IntStackValue(a.getIntValue() ^ b.getIntValue());
            case OP_lxor:
                return new LongStackValue(a.getLongValue() ^ b.getLongValue());
            case OP_lcmp:
                return new IntStackValue(doOPlcmp(a.getLongValue(), b.getLongValue()));
            case OP_fcmpl:
                return new IntStackValue(doOPdcmpl(a.getFloatValue(), b.getFloatValue()));
            case OP_fcmpg:
                return new IntStackValue(doOPdcmpg(a.getFloatValue(), b.getFloatValue()));
            case OP_dcmpl:
                return new IntStackValue(doOPdcmpl(a.getDoubleValue(), b.getDoubleValue()));
            case OP_dcmpg:
                return new IntStackValue(doOPdcmpg(a.getDoubleValue(), b.getDoubleValue()));
            default:
                Globals.localError("invalid math opcode");
                return null;
        }
    }
    
    private static AbstractStackValue doUnaryMathOp(int opcode, AbstractStackValue v) {
        switch (opcode) {
            case OP_ineg:
                return new IntStackValue(-v.getIntValue());
            case OP_lneg:
                return new LongStackValue(-v.getLongValue());
            case OP_fneg:
                return new FloatStackValue(-v.getFloatValue());
            case OP_dneg:
                return new DoubleStackValue(-v.getDoubleValue());
            case OP_i2l:
                return new LongStackValue(v.getIntValue());
            case OP_i2f:
                return new FloatStackValue((float)v.getIntValue());
            case OP_i2d:
                return new DoubleStackValue((double)v.getIntValue());
            case OP_l2i:
                return new IntStackValue((int)v.getLongValue());
            case OP_l2f:
                return new FloatStackValue((float)v.getLongValue());
            case OP_l2d:
                return new DoubleStackValue((double)v.getDoubleValue());
            case OP_f2i:
                return new IntStackValue((int)v.getFloatValue());
            case OP_f2l:
                return new LongStackValue((long)v.getFloatValue());
            case OP_f2d:
                return new DoubleStackValue(v.getFloatValue());
            case OP_d2i:
                return new IntStackValue((int)v.getDoubleValue());
            case OP_d2l:
                return new LongStackValue((long)v.getDoubleValue());
            case OP_d2f:
                return new FloatStackValue((float)v.getDoubleValue());
            case OP_i2b:
                return new IntStackValue((byte)v.getIntValue());
            case OP_i2c:
                return new IntStackValue((char)v.getIntValue());
            case OP_i2s:
                return new IntStackValue((short)v.getIntValue());
            default:
                Globals.localError("invalid math opcode");
                return null;
        }
    }
    
    private static boolean doUnaryBooleanOp(int opcode, AbstractStackValue v) {
        switch (opcode) {
            case OP_ifeq:
            case OP_ifne:
            case OP_iflt:
            case OP_ifge:
            case OP_ifgt:
            case OP_ifle:
                return JBCCodeUtilities.isBranchTaken(opcode, v.getIntValue());
            
            case OP_ifnull:
                return v.getObjectValue() == null;
            case OP_ifnonnull:
                return v.getObjectValue() != null;
            default:
                throw Globals.localError("invalid boolean opcode");
        }
    }
    
    private static boolean doBinaryBooleanOp(int opcode, AbstractStackValue a,
        AbstractStackValue b) {
        switch (opcode) {
            case OP_if_icmpeq:
                return a.getIntValue() == b.getIntValue();
            case OP_if_icmpne:
                return a.getIntValue() != b.getIntValue();
            case OP_if_icmplt:
                return a.getIntValue() < b.getIntValue();
            case OP_if_icmpge:
                return a.getIntValue() >= b.getIntValue();
            case OP_if_icmpgt:
                return a.getIntValue() > b.getIntValue();
            case OP_if_icmple:
                return a.getIntValue() <= b.getIntValue();
            case OP_if_acmpeq:
                return a.getObjectValue() == b.getObjectValue();
            case OP_if_acmpne:
                return a.getObjectValue() != b.getObjectValue();
            default:
                throw Globals.localError("invalid boolean opcode");
        }
    }
    
    private AbstractStackValue findConstFieldValue(JBCField f) {
        return new UnknownObjectStackValue();
/* You would think that 'final' fields with initial values provided
   could have their contents returned here, because we should know
   what the value is going to be. However, it seems that native
   code can actually change the value of a final variable ...
   see java.lang.System.in/out/err for an example. */
    }
    
    private Variable makeNewObject(int offset, JBCInstructionInfo info, JBCClass c) {
        Variable objVar = manager.getClassVar(c);
        JBCMethod finalizer = c.getInheritedMethod("finalize", "()V");
        World w = manager.getSolver();
        
        if (!manager.getSingletonUsageDetector().isClassInstantiatedOnce(c)) {
            objVar = objVar.getInstance(w, JBCNewObjectInstance.get(context, method, offset));
        }
        
        if (finalizer != null && !finalizer.getContainingClass().equals(manager.getJavaLangObject())) {
            Variable finalizerVar = manager.getMethodVar(JBCMethodInvocationContext.get(context, method, offset), finalizer)
                .getInstance(w, JBCMethodInvocationInstance.get(context, method, offset));

            if (manager.makeGlobalComponents) {
                finalizerVar.getComponent(w, Variable.DMODE, JBCMethodGlobalsArgComponent.get()).
                    makeEqual(w, info.block.globalsVar);
            }
            finalizerVar.getComponent(w, Variable.DMODE, JBCMethodArgComponent.get(0))
                .makeEqual(w, objVar);
        }
        
        return objVar;
    }
    
    private Variable getBlockLocalVar(JBCBlockInfo block, int index) {
        Variable v = block.entryLocalVars[index];
        
        if (v == null) {
            v = new Variable(manager.getSolver());
            block.entryLocalVars[index] = v;

            if (Globals.debug) manager.addToken(v, new LocalVariableDebugToken(index, block.offset, method));
        }
        
        return v;
    }

    private static int getBaseOpcode(byte[] code, int offset) {
        int opcode = code[offset] & 0xFF;
        
        if (opcode == OP_wide) {
            opcode = code[offset + 1] & 0xFF;
        }
        
        return opcode;
    }

    private String getArrayElementName(byte[] code, int offset) {
        switch (code[offset] & 0xFF) {
            case OP_anewarray:
            case OP_multianewarray:
            case OP_aaload:
            case OP_aastore:        return "arrayelement";
            
            case OP_iaload:
            case OP_iastore:
            case OP_baload:
            case OP_caload:
            case OP_saload:
            case OP_bastore:
            case OP_castore:
            case OP_sastore:        return "intarrayelement";
            
            case OP_laload:
            case OP_lastore:        return "longarrayelement";
            
            case OP_faload:
            case OP_fastore:        return "floatarrayelement";
            
            case OP_daload:
            case OP_dastore:        return "doublearrayelement";
                
            case OP_newarray:
                switch (code[offset + 1] & 0xFF) {
                    case T_BOOLEAN:
                    case T_CHAR:
                    case T_BYTE:
                    case T_SHORT:
                    case T_INT:     return "intarrayelement";
                    
                    case T_FLOAT:   return "floatarrayelement";
                    
                    case T_DOUBLE:  return "doublearrayelement";
                    
                    case T_LONG:    return "longarrayelement";
                    
                    default:
                        throw Globals.localError("Invalid primitive array type");
                }
            
            default:
                throw Globals.localError("Cannot get array element name for nonarray instruction");
        }
    }
    
    private AbstractStackValue getPushedScalarConstant(byte[] code, int offset) {
        switch (code[offset] & 0xFF) {
            case OP_iconst_m1:   return INT_M1_VALUE;
            case OP_iconst_0:    return INT_0_VALUE;
            case OP_iconst_1:    return INT_1_VALUE;
            case OP_iconst_2:    return INT_2_VALUE;
            case OP_iconst_3:    return INT_3_VALUE;
            case OP_iconst_4:    return INT_4_VALUE;
            case OP_iconst_5:    return INT_5_VALUE;
            case OP_lconst_0:    return LONG_0_VALUE;
            case OP_lconst_1:    return LONG_1_VALUE;
            case OP_fconst_0:    return FLOAT_0_VALUE;
            case OP_fconst_1:    return FLOAT_1_VALUE;
            case OP_fconst_2:    return FLOAT_2_VALUE;
            case OP_dconst_0:    return DOUBLE_0_VALUE;
            case OP_dconst_1:    return DOUBLE_1_VALUE;
            case OP_bipush:      return new IntStackValue(code[offset + 1]);
            case OP_sipush:      return new IntStackValue(JBCCodeUtilities.getShortAt(code, offset + 1));
            default:
                throw Globals.localError("Not a scalar-pushing instruction: " + code[offset]);
        }
    }
    
/**
@return the exception thrown, if it's special (invoke* or athrow)
*/
    private Variable makeConstraints(JBCInstructionInfo[] instructions, byte[] code,
        int offset, AbstractStackValue[] entryStackValues, Variable[] entryStackVars,
        AbstractStackValue[] entryLocalValues, Variable[] entryDefinedLocalVars,
        AbstractStackValue[] stackValues, Variable[] stackVars,
        int pushCount, int delta, int stackSize, int[] successors) {
        World w = manager.getSolver();
        JBCInstructionInfo info = instructions[offset];
        int opcode = getBaseOpcode(code, offset);
        boolean combineInputs = false;
        boolean combineInputWithOutput = false;
        
        if (manager.getSemantics() instanceof CombiningSemantics) {
            CombiningSemantics semantics = (CombiningSemantics)manager.getSemantics();
            
            combineInputs = semantics.isCombiningInputs((byte)opcode);
            combineInputWithOutput = semantics.isCombiningInputWithOutput((byte)opcode);
        }
        
        switch (opcode) {
            case OP_nop:
                break;
                
            case OP_aconst_null:
                stackVars[0] = new Variable(w);
                stackValues[0] = NULL_VALUE;
                break;
                
            case OP_iconst_m1:
            case OP_iconst_0:
            case OP_iconst_1:
            case OP_iconst_2:
            case OP_iconst_3:
            case OP_iconst_4:
            case OP_iconst_5:
            case OP_lconst_0:
            case OP_lconst_1:
            case OP_fconst_0:
            case OP_fconst_1:
            case OP_fconst_2:
            case OP_dconst_0:
            case OP_dconst_1:
            case OP_bipush:
            case OP_sipush:
                stackVars[0] = new Variable(w);
                manager.makeInitializedValue(stackVars[0]);
                stackValues[0] = getPushedScalarConstant(code, offset);
                break;
            
            case OP_ldc:
            case OP_ldc_w:
            case OP_ldc2_w: {
                Object constant = JBCCodeUtilities.getLoadedConstant(method, code, offset);
                
                if (constant instanceof String) {
                    stackVars[0] = manager.makeStringConst(info.block.globalsVar);
                } else {
                    stackVars[0] = new Variable(w);
                }
                
                stackValues[0] = makeConstantStackValue(constant);
                manager.makeInitializedValue(stackVars[0]);
                break;
            }
            
            case OP_iload:
            case OP_lload:
            case OP_fload:
            case OP_dload:
            case OP_aload:
            case OP_iload_0:
            case OP_iload_1:
            case OP_iload_2:
            case OP_iload_3:
            case OP_lload_0:
            case OP_lload_1:
            case OP_lload_2:
            case OP_lload_3:
            case OP_fload_0:
            case OP_fload_1:
            case OP_fload_2:
            case OP_fload_3:
            case OP_dload_0:
            case OP_dload_1:
            case OP_dload_2:
            case OP_dload_3:
            case OP_aload_0:
            case OP_aload_1:
            case OP_aload_2:
            case OP_aload_3: {
                int index = JBCCodeUtilities.getLocalVariableLoadIndex(code, offset);

                stackValues[0] = entryLocalValues[index];
                
                Variable v = entryDefinedLocalVars[index];

                if (v == null) {
                    v = getBlockLocalVar(info.block, index);
                    entryDefinedLocalVars[index] = v;
                }
                
                stackVars[0] = v;
                break;
            }
                
            case OP_iaload:
            case OP_laload:
            case OP_faload:
            case OP_daload:
            case OP_aaload:
            case OP_baload:
            case OP_caload:
            case OP_saload: {
                Variable array = entryStackVars[1];
                
                if (array != null) {
                    if (combineInputs && entryStackVars[0] != null) {
                        array.makeEqual(w, entryStackVars[0]);
                    }

		    if (manager.isTrackingArrayIndices() && entryStackVars[0] != null) {
			manager.makeNonstaticUserField(Variable.DMODE,
                            array,
			    manager.getJavaLangObject().registerUserField("arraylength", false),
			    Variable.DMODE).makeEqual(w, entryStackVars[0]);
		    }
                
                    stackVars[0] = manager.makeNonstaticUserField(Variable.DMODE,
                        array,
                        manager.getJavaLangObject().registerUserField(
                            getArrayElementName(code, offset), false),
                        Variable.DMODE);
                    
                    if (combineInputWithOutput) {
                        array.makeEqual(w, stackVars[0]);
                    }
                }
                
                if (opcode == OP_aaload) {
                    stackValues[0] = new UnknownObjectStackValue();
                }
                break;
            }
            
            case OP_iastore:
            case OP_lastore:
            case OP_fastore:
            case OP_dastore:
            case OP_aastore:
            case OP_bastore:
            case OP_castore:
            case OP_sastore: {
                Variable array = entryStackVars[-delta - 1];
                Variable element = entryStackVars[0];
                
                if (array != null && element != null && entryStackVars[-delta - 2] != null) {
		    if (manager.isTrackingArrayIndices()) {
			manager.makeNonstaticUserField(Variable.CMODE,
                            array,
			    manager.getJavaLangObject().registerUserField("arraylength", false),
                            Variable.DMODE).makeEqual(w, entryStackVars[-delta - 2]);
			    }

                    if (combineInputs) {
                        array.makeEqual(w, entryStackVars[-delta - 2]);
                        array.makeEqual(w, element);
			}

		    manager.makeNonstaticUserField(Variable.CMODE,
                            array,
                            manager.getJavaLangObject().registerUserField(
                                getArrayElementName(code, offset), false),
                            Variable.DMODE)
			    .makeEqual(w, element);
                }
                break;
            }
            
            case OP_pop:
            case OP_pop2:
                break;
                
            case OP_dup:
                stackVars[0] = entryStackVars[0];
                stackValues[0] = entryStackValues[0];
                stackVars[1] = entryStackVars[0];
                stackValues[1] = entryStackValues[0];
                break;
                
            case OP_dup_x1:
                stackVars[0] = entryStackVars[0];
                stackValues[0] = entryStackValues[0];
                stackVars[1] = entryStackVars[1];
                stackValues[1] = entryStackValues[1];
                stackVars[2] = entryStackVars[0];
                stackValues[2] = entryStackValues[0];
                break;
                
            case OP_dup_x2:
                stackVars[0] = entryStackVars[0];
                stackValues[0] = entryStackValues[0];
                stackVars[1] = entryStackVars[1];
                stackValues[1] = entryStackValues[1];
                stackVars[2] = entryStackVars[2];
                stackValues[2] = entryStackValues[2];
                stackVars[3] = entryStackVars[0];
                stackValues[3] = entryStackValues[0];
                break;
            
            case OP_dup2:
                stackVars[0] = entryStackVars[0];
                stackValues[0] = entryStackValues[0];
                stackVars[1] = entryStackVars[1];
                stackValues[1] = entryStackValues[1];
                stackVars[2] = entryStackVars[0];
                stackValues[2] = entryStackValues[0];
                stackVars[3] = entryStackVars[1];
                stackValues[3] = entryStackValues[1];
                break;

            case OP_dup2_x1:
                stackVars[0] = entryStackVars[0];
                stackValues[0] = entryStackValues[0];
                stackVars[1] = entryStackVars[1];
                stackValues[1] = entryStackValues[1];
                stackVars[2] = entryStackVars[2];
                stackValues[2] = entryStackValues[2];
                stackVars[3] = entryStackVars[0];
                stackValues[3] = entryStackValues[0];
                stackVars[4] = entryStackVars[1];
                stackValues[4] = entryStackValues[1];
                break;

            case OP_dup2_x2:
                stackVars[0] = entryStackVars[0];
                stackValues[0] = entryStackValues[0];
                stackVars[1] = entryStackVars[1];
                stackValues[1] = entryStackValues[1];
                stackVars[2] = entryStackVars[2];
                stackValues[2] = entryStackValues[2];
                stackVars[3] = entryStackVars[3];
                stackValues[3] = entryStackValues[3];
                stackVars[4] = entryStackVars[0];
                stackValues[4] = entryStackValues[0];
                stackVars[5] = entryStackVars[1];
                stackValues[5] = entryStackValues[1];
                break;

            case OP_swap:
                stackVars[0] = entryStackVars[1];
                stackValues[0] = entryStackValues[1];
                stackVars[1] = entryStackVars[0];
                stackValues[1] = entryStackValues[0];
                if (combineInputs && stackVars[0] != null && stackVars[1] != null) {
                    stackVars[0].makeEqual(w, stackVars[1]);
                }
                break;
                
            case OP_iadd:
            case OP_ladd:
            case OP_fadd:
            case OP_dadd:
            case OP_isub:
            case OP_lsub:
            case OP_fsub:
            case OP_dsub:
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
            case OP_lxor: {
                AbstractStackValue a = entryStackValues[-delta];
                
                if (a != null && a.isValueKnown()) {
                    AbstractStackValue b = entryStackValues[0];
                    
                    if (b != null && b.isValueKnown()) {
                        try {
                            stackValues[0] = doBinaryMathOp(opcode, a, b);
                        } catch (InvalidAbstractValueTypeError err) {
                        }
                    }
                }
                
                if (entryStackVars[-delta] != null
                    && entryStackVars[0] != null) {
                    if (combineInputs) {
                        entryStackVars[-delta].makeEqual(w, entryStackVars[0]);
                    }

                    if (combineInputWithOutput) {
                        stackVars[0] = entryStackVars[-delta];
                    } else {
                        stackVars[0] = new Variable(w);
                    }
                    manager.makeInitializedValue(stackVars[0]);
                }
                break;
            }
            
            case OP_ineg:
            case OP_lneg:
            case OP_fneg:
            case OP_dneg: {
                AbstractStackValue a = entryStackValues[0];
                
                if (a != null && a.isValueKnown()) {
                    try {
                        stackValues[0] = doUnaryMathOp(opcode, a);
                    } catch (InvalidAbstractValueTypeError err) {
                    }
                }

                if (entryStackVars[0] != null) {
                    if (combineInputWithOutput) {
                        stackVars[0] = entryStackVars[0];
                    } else {
                        stackVars[0] = new Variable(w);
                    }
                    manager.makeInitializedValue(stackVars[0]);
                }
                break;
            }
            
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
            case OP_i2s: {
                AbstractStackValue a = entryStackValues[0];
                
                if (a != null && a.isValueKnown()) {
                    try {
                        stackValues[0] = doUnaryMathOp(opcode, a);
                    } catch (InvalidAbstractValueTypeError err) {
                    }
                }
                
                if (entryStackVars[0] != null) {
                    if (combineInputWithOutput) {
                        stackVars[0] = entryStackVars[0];
                    } else {
                        stackVars[0] = new Variable(w);
                    }
                    manager.makeInitializedValue(stackVars[0]);
                }
                break;
            }
            
            case OP_lcmp:
            case OP_fcmpl:
            case OP_fcmpg:
            case OP_dcmpl:
            case OP_dcmpg: {
                AbstractStackValue a = entryStackValues[(1 - delta)/2];
                
                if (a != null && a.isValueKnown()) {
                    AbstractStackValue b = entryStackValues[0];
                    
                    if (b != null && b.isValueKnown()) {
                        try {
                            stackValues[0] = doBinaryMathOp(opcode, a, b);
                        } catch (InvalidAbstractValueTypeError err) {
                        }
                    }
                }

                if (entryStackVars[(1 - delta)/2] != null
                    && entryStackVars[0] != null) {
                    if (combineInputs) {
                        entryStackVars[(1 - delta)/2].makeEqual(w, entryStackVars[0]);
                    }
                    
                    if (combineInputWithOutput) {
                        stackVars[0] = entryStackVars[(1 - delta)/2];
                    } else {
                        stackVars[0] = new Variable(w);
                    }
                    manager.makeInitializedValue(stackVars[0]);
                }
                
                break;
            }
            
            case OP_ifeq:
            case OP_ifne:
            case OP_iflt:
            case OP_ifge:
            case OP_ifgt:
            case OP_ifle:
            case OP_ifnull:
            case OP_ifnonnull: {
                AbstractStackValue a = entryStackValues[0];
                
                if (a != null && a.isValueKnown()) {
                    try {
                        if (doUnaryBooleanOp(opcode, a)) {
                            successors[0] = -1;
                        } else {
                            successors[1] = -1;
                        }
                    } catch (InvalidAbstractValueTypeError err) {
                    }
                }
                break;
            }
            
            case OP_if_icmpeq:
            case OP_if_icmpne:
            case OP_if_icmplt:
            case OP_if_icmpge:
            case OP_if_icmpgt:
            case OP_if_icmple:
            case OP_if_acmpeq:
            case OP_if_acmpne: {
                AbstractStackValue a = entryStackValues[1];
                
                if (a != null && a.isValueKnown()) {
                    AbstractStackValue b = entryStackValues[0];
                    
                    if (b != null && b.isValueKnown()) {
                        try {
                            if (doBinaryBooleanOp(opcode, a, b)) {
                                successors[0] = -1;
                            } else {
                                successors[1] = -1;
                            }
                        } catch (InvalidAbstractValueTypeError err) {
                        }
                    }
                }
                
                if (entryStackVars[1] != null
                    && entryStackVars[0] != null) {
                    if (combineInputs) {
                        entryStackVars[1].makeEqual(w, entryStackVars[0]);
                    }
                }
                break;
            }
            
            case OP_goto:
            case OP_goto_w:
            case OP_tableswitch:
            case OP_lookupswitch:
            case OP_return:
                break;
                
            case OP_jsr:
            case OP_jsr_w:
                /* the routing functions do most of the work here */
                stackVars[0] = instructions[successors[0]].block.returnInfo.jsrTupleVar;
                manager.makeInitializedValue(stackVars[0]);
                break;
            
            case OP_ret: {
                /* the routing functions do most of the work here */
                int index = JBCCodeUtilities.getLocalVariableLoadIndex(code, offset);
                Variable retAddr = getBlockLocalVar(info.block, index);
                ComponentLabel resultComponent = JBCMethodResultComponent.get();
                JBCBlockInfo block = info.block;
                
                retAddr.getComponent(w, Variable.DMODE, JBCInstructionLocalVarComponent.get(index))
                    .makeEqual(w, retAddr);
                retAddr.getComponent(w, Variable.DMODE, resultComponent)
                    .makeEqual(w, block.returnVar);
                    
                if (manager.makeExceptionComponents) {
                    retAddr.getComponent(w, Variable.DMODE, JBCMethodExceptionComponent.get())
                        .makeEqual(w, block.exceptionVar);
                }
                    
                if (manager.makeGlobalComponents) {
                    retAddr.getComponent(w, Variable.DMODE, JBCMethodGlobalsArgComponent.get())
                        .makeEqual(w, block.globalsVar);
                }

                for (int i = 0; i < stackSize; i++) {
                    Variable in = entryStackVars[i];
                    
                    if (in != null) {
                        retAddr.getComponent(w, Variable.DMODE, JBCInstructionStackElemComponent.get(i))
                            .makeEqual(w, in);
                    }
                }
                break;
            }
            
            case OP_ireturn:
            case OP_lreturn:
            case OP_freturn:
            case OP_dreturn:
            case OP_areturn: {
                Variable retVal = entryStackVars[0];
                
                if (retVal != null) {
                    info.block.returnVar.makeEqual(w, retVal);
                }
                break;
            }
            
            case OP_getstatic:  {
                JBCField f = JBCCodeUtilities.resolveInstructionField(method, code, offset);
                
                if (f != null) {
                    stackValues[0] = findConstFieldValue(f);
                    stackVars[0] = manager.makeStaticField(Variable.DMODE, info.block.globalsVar, f, Variable.DMODE);
                    
                    addClassInitializationIfNeeded(info.block, offset, f.getContainingClass());
                }
                break;
            }
            
            case OP_getfield: {
                JBCField f = JBCCodeUtilities.resolveInstructionField(method, code, offset);
                
                if (f != null) {
                    Variable objVar = entryStackVars[0];
                        
                    stackValues[0] = findConstFieldValue(f);
                    if (objVar != null) {
                        stackVars[0] = manager.makeNonstaticField(Variable.DMODE,
                            objVar, f, Variable.DMODE);
                            
                        if (combineInputWithOutput) {
                            stackVars[0].makeEqual(w, objVar);
                        }
                    }
                }
                break;
            }
            
            case OP_putstatic: {
                Variable valVar = entryStackVars[0];
                
                if (valVar != null) {
                    JBCField f = JBCCodeUtilities.resolveInstructionField(method, code, offset);
                    
                    if (f != null) {
                        manager.makeStaticField(Variable.CMODE, info.block.globalsVar, f, Variable.DMODE)
                            .makeEqual(w, valVar);

                        addClassInitializationIfNeeded(info.block, offset, f.getContainingClass());
                    }
                }
                break;
            }
            
            case OP_putfield: {
                JBCField f = JBCCodeUtilities.resolveInstructionField(method, code, offset);
                
                if (f != null) {
                    Variable objVar = entryStackVars[-delta - 1];
                    Variable valVar = entryStackVars[0];
                    
                    if (objVar != null && valVar != null) {
                        manager.makeNonstaticField(Variable.CMODE, objVar, f, Variable.DMODE)
                            .makeEqual(w, valVar);
                            
                        if (combineInputs) {
                            valVar.makeEqual(w, objVar);
                        }
                    }
                }
                break;
            }
            
            case OP_invokestatic:
            case OP_invokespecial:
            case OP_invokevirtual:
            case OP_invokeinterface: {
                JBCMethod m = JBCCodeUtilities.resolveInstructionMethod(method, code, offset);
                
                if (m != null) {
                    InvocationContext callContext =
                        JBCMethodInvocationContext.get(context, method, offset);
                    Variable v;

                    if (opcode == OP_invokespecial || opcode == OP_invokestatic
                        || manager.useStaticDispatch(callContext, m)) {
                        SEMISingletonUsageDetector singletonDetector = manager.getSingletonUsageDetector();
                        int callStatus = singletonDetector.getMethodCallStatus(m,
                            new JBCLocation(method, offset));
                            
                        v = manager.getMethodVar(callContext, m);
                            
                        if (callStatus != singletonDetector.CALL_SINGLE_CALLER) {
                            v = v.getInstance(w, JBCMethodInvocationInstance.get(context, method, offset));
                        }
                    } else {
                        Variable objVar = entryStackVars[-delta + pushCount - 1];
                        
                        if (objVar != null) {
                            v = new Variable(w);
                                    
                            manager.getCallResolver().resolveCall(callContext, method, offset,
                                m, objVar, v);
                        } else {
                            v = null;
                        }
                    }
                            
                    if (v == null) {
                        if (pushCount > 0
                            && m.getMethodType().getReturnType() instanceof JBCObjectType) {
                            stackValues[0] = new UnknownObjectStackValue();
                        }
                    } else {
                        JBCMethodType methodType = m.getMethodType();
                        Variable firstParam = null;
                        Variable resultVar = null;
                        
                        if (manager.makeGlobalComponents) {
                            v.getComponent(w, Variable.DMODE, JBCMethodGlobalsArgComponent.get()).
                                makeEqual(w, info.block.globalsVar);

                            if (Globals.debug) {
                                manager.addToken(info.block.globalsVar, new ActualGlobalsDebugToken(m, method));
                            }
                        }
                                    
                        if (pushCount > 0) {
                            JBCType returnType = methodType.getReturnType();
                            
                            resultVar = v.getComponent(w, Variable.DMODE,
                                JBCMethodResultComponent.get());
                                        
                            stackVars[0] = resultVar;
                            manager.setJBCType(resultVar, returnType);
                            if (returnType instanceof JBCObjectType) {
                                stackValues[0] = new UnknownObjectStackValue();
                            }

                            if (Globals.debug) {
                                manager.addToken(resultVar, new ActualReturnDebugToken(m, method));
                            }                
                        }
                                
                        JBCType[] paramTypes = methodType.getParameterTypes();
                        int wordCount = 0;
                                
                        for (int i = paramTypes.length - 1; i >= 0; i--) {
                            JBCType paramType = paramTypes[i];
                            Variable paramVar = entryStackVars[wordCount];
                                    
                            if (paramVar != null) {
                                paramVar.makeEqual(w, v.getComponent(w, Variable.DMODE,
                                    JBCMethodArgComponent.get(i)));
                                manager.setJBCType(paramVar, paramType);
                                
                                if (combineInputs && firstParam != null) {
                                    firstParam.makeEqual(w, paramVar);
                                }
                                firstParam = paramVar;

                                if (Globals.debug) {
                                    manager.addToken(paramVar, new ActualParameterDebugToken(i, m, method));
                                }
                            }

                            wordCount += paramType.getWordSize();
                        }
                        
                        if (combineInputWithOutput && firstParam != null && resultVar != null) {
                            firstParam.makeEqual(w, resultVar);
                        }

                        if (Globals.debug && wordCount != -delta + pushCount) {
                            Globals.localError("Didn't push all the parameters");
                        }
                        
                        if (manager.makeExceptionComponents) {
                            Variable exn = v.getComponent(w, Variable.DMODE, JBCMethodExceptionComponent.get());
                        
                            if (Globals.debug) {
                                manager.addToken(exn, new ActualExceptionDebugToken(m, method));
                            }
                            
                            return exn;
                        } else {
                            return manager.getExceptionVar();
                        }
                    }
                }
                break;
            }
            
            case OP_new: {
                JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, offset);
                
                if (c != null) {
                    stackVars[0] = makeNewObject(offset, info, c);
                    stackValues[0] = new ClassOnlyObjectStackValue(c);
                }
                break;
            }
            
            case OP_newarray: {
                Variable lengthVar = entryStackVars[0];

                if (lengthVar != null) {
                    JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, offset);
                    
                    if (c != null) {
                        Variable v = makeNewObject(offset, info, c);
                            
                        stackValues[0] = new ClassOnlyObjectStackValue(c);
                        stackVars[0] = v;
                        manager.makeNonstaticUserField(Variable.CMODE, v,
                                manager.getJavaLangObject().registerUserField("arraylength", false),
                                Variable.CMODE)
                            .makeEqual(w, lengthVar);
                        manager.setJBCType(lengthVar, JBCType.INT);
                    /* make it apparent that the element slots have been
                    initialized (to null) */
                        manager.makeInitializedValue(
                            manager.makeNonstaticUserField(Variable.CMODE, v,
                                manager.getJavaLangObject().registerUserField(
                                    getArrayElementName(code, offset), false),
                                Variable.CMODE));

                        if (combineInputWithOutput) {
                            stackVars[0].makeEqual(w, lengthVar);
                        }
                    }
                }
                break;
            }
            
            case OP_anewarray: {
                Variable lengthVar = entryStackVars[0];
                
                if (lengthVar != null) {
                    JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, offset);
                    
                    if (c != null) {
                        Variable v = makeNewObject(offset, info, c);
                        
                        stackValues[0] = new ClassOnlyObjectStackValue(c);
                        stackVars[0] = v;
                        manager.makeNonstaticUserField(Variable.CMODE, v,
                                manager.getJavaLangObject().registerUserField("arraylength", false),
                                Variable.CMODE)
                            .makeEqual(w, lengthVar);
                        manager.setJBCType(lengthVar, JBCType.INT);
                    /* make it apparent that the element slots have been
                    initialized (to null) */
                        manager.makeNonstaticUserField(Variable.CMODE, v,
                            manager.getJavaLangObject().registerUserField(
                                getArrayElementName(code, offset), false),
                            Variable.CMODE);
                            
                        if (combineInputWithOutput) {
                            stackVars[0].makeEqual(w, lengthVar);
                        }
                    }
                }
                break;
            }
            
            case OP_arraylength: {
                Variable arrayVar = entryStackVars[0];
                
                if (arrayVar != null) {
                    stackVars[0] = manager.makeNonstaticUserField(
                        Variable.DMODE, arrayVar,
                        manager.getJavaLangObject().registerUserField("arraylength", false),
                        Variable.DMODE);
                        
                    if (combineInputWithOutput) {
                        stackVars[0].makeEqual(w, arrayVar);
                    }
                }
                break;
            }
            
            case OP_athrow:
                return entryStackVars[0];
                
            case OP_checkcast: {
                JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, offset);
                
                if (c != null) {
                    Variable objVar = entryStackVars[0];

                    stackValues[0] = entryStackValues[0];
                    
                    if (objVar != null) {
                        stackVars[0] = manager.makeDowncast(objVar, c);
                    }
                }
                break;
            }
            
            case OP_instanceof: {
                JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, offset);
                
                if (c != null) {
                    if (entryStackValues[0] != null) {
                        stackValues[0] = new InstanceOfResultValue(manager, code, entryStackValues[0], c);
                    }

                    if (entryStackVars[0] != null) {
                        stackVars[0] = new Variable(w);
                        
                        if (combineInputWithOutput) {
                            stackVars[0].makeEqual(w, entryStackVars[0]);
                        }
                        manager.makeInitializedValue(stackVars[0]);
                    }
                }
                break;
            }
            
            case OP_monitorenter:
            case OP_monitorexit:
                break;
                
            case OP_multianewarray: {
                JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, offset);
                
                if (c != null) {
                    Variable v = makeNewObject(offset, info, c);
                    int dimensions = code[offset + 3] & 0xFF;
                    JBCClass javaLangObject = manager.getJavaLangObject();
                    UserField arrayElement = javaLangObject.registerUserField(
                        getArrayElementName(code, offset), false);
                    UserField arrayLength = javaLangObject.registerUserField("arraylength", false);
                    Variable savedLengthVar = null;
                    
                    stackValues[0] = new ClassOnlyObjectStackValue(c);
                    stackVars[0] = v;
                    
                    for (int i = 0; i < dimensions; i++) {
                        Variable lengthVar =
                            entryStackVars[dimensions - i - 1];

                        if (lengthVar != null) {
                            manager.makeNonstaticUserField(Variable.CMODE, v,
                                    arrayLength, Variable.CMODE)
                                .makeEqual(w, lengthVar);
                            manager.setJBCType(lengthVar, JBCType.INT);

                            if (i == 0 && combineInputWithOutput) {
                                lengthVar.makeEqual(w, v);
                            }                    
                            
                            if (savedLengthVar == null) {
                                savedLengthVar = lengthVar;
                            } else if (combineInputs) {
                                lengthVar.makeEqual(w, savedLengthVar);
                            }
                        }
                        
                        v = manager.makeNonstaticUserField(Variable.CMODE, v,
                            arrayElement, Variable.CMODE);
                    }
                }
                break;
            }
                
            default:
            /* we should never get to here if there's an invalid opcode ---
               it should have been detected earlier */
                Globals.localError("Invalid opcode: " + opcode);
        }
        
        return null;
    }
    
    private void addBlockPredecessor(JBCInstructionInfo[] instructions,
        byte[] code, Vector blocksToDoFirst, Vector blocksToDoLast,
        int offset, int successor, int successorIndex,
        Variable[] entryStackVars, AbstractStackValue[] entryStackValues, int stackSize,
        Variable[] entryDefinedLocalVars, AbstractStackValue[] entryLocalValues,
        boolean createAllStackVars) {
        JBCBlockInfo dest = instructions[successor].block;
        JBCBlockInfo source = instructions[offset].block;
        int opcode = code[offset] & 0xFF;
        World w = manager.getSolver();
        boolean isFirstPredecessor = dest.exceptionVar == null;
        boolean isRetTarget = successorIndex == 0 && (opcode == OP_jsr || opcode == OP_jsr_w);
        
        if (isRetTarget) {
            /* if this instruction is reached as the return address for a subroutine call,
               then fix up the stack size to be the stack size at the jsr MINUS ONE because
               the return address is not here */
            stackSize--;
        }
        
        if (isFirstPredecessor) {
            dest.returnVar = new Variable(w);
            
            if (manager.makeExceptionComponents) {
                dest.exceptionVar = new Variable(w);
            } else {
                dest.exceptionVar = manager.getExceptionVar();
            }
            
            if (manager.makeGlobalComponents) {
                dest.globalsVar = new Variable(w);
            } else {
                dest.globalsVar = manager.getGlobalsVar();
            }
            
            dest.entryStackVars = new Variable[stackSize];
            dest.entryLocalVars = new Variable[entryDefinedLocalVars.length];
            dest.entryStackValues = new AbstractStackValue[stackSize];
            dest.entryLocalValues = new AbstractStackValue[entryLocalValues.length];
            
            if (createAllStackVars) {
                Variable[] stackVars = dest.entryStackVars;
                
                for (int i = 0; i < stackVars.length; i++) {
                    stackVars[i] = new Variable(w);
                }
            }
            
            if (Globals.debug) {
                if (manager.makeGlobalComponents) {
                    manager.addToken(dest.globalsVar, new InternalGlobalsDebugToken(successor, method));
                    manager.setJBCType(dest.globalsVar, ConstraintManager.GLOBALS);
                }
                
                if (manager.makeExceptionComponents) {
                    manager.addToken(dest.exceptionVar, new InternalExceptionDebugToken(successor, method));
                    manager.setJBCType(dest.exceptionVar, JBCType.OBJECT);
                }
                
                manager.addToken(dest.returnVar, new InternalResultDebugToken(successor, method));
            }
            
            if (manager.makeExceptionsGlobal) {
                dest.exceptionVar.setGlobal(w);
            }
            dest.globalsVar.setGlobal(w);
            
            blocksToDoLast.addElement(dest);
        }
        
        if (isRetTarget) {
            JBCReturnInfo returnInfo = dest.returnInfo;
            InstanceLabel returnInstance = returnInfo.instance;
            Variable jsrTupleVar = returnInfo.jsrTupleVar;
            
            if (ConstraintManager.useControlFlowPolymorphism) {
                if (manager.makeExceptionComponents) {
                    jsrTupleVar.getComponent(w, Variable.CMODE, JBCMethodExceptionComponent.get())
                        .makeEqual(w, dest.exceptionVar.getInstance(w, returnInstance));
                }
                    
                if (manager.makeGlobalComponents) {
                    jsrTupleVar.getComponent(w, Variable.CMODE, JBCMethodGlobalsArgComponent.get())
                        .makeEqual(w, dest.globalsVar.getInstance(w, returnInstance));
                }
                
                jsrTupleVar.getComponent(w, Variable.CMODE, JBCMethodResultComponent.get())
                    .makeEqual(w, dest.returnVar.getInstance(w, returnInstance));
            } else {
                if (manager.makeExceptionComponents) {
                    jsrTupleVar.getComponent(w, Variable.CMODE, JBCMethodExceptionComponent.get())
                        .makeEqual(w, dest.exceptionVar);
                }
                    
                if (manager.makeGlobalComponents) {
                    jsrTupleVar.getComponent(w, Variable.CMODE, JBCMethodGlobalsArgComponent.get())
                        .makeEqual(w, dest.globalsVar);
                }
                
                jsrTupleVar.getComponent(w, Variable.CMODE, JBCMethodResultComponent.get())
                    .makeEqual(w, dest.returnVar);
            }
            
            for (int i = 0; i < dest.entryStackVars.length; i++) {
                Variable destV = dest.entryStackVars[i];
                
                if (destV == null) {
                    destV = new Variable(w);
                    dest.entryStackVars[i] = destV;
                    
                    if (Globals.debug) manager.addToken(destV, new InternalStackElemDebugToken(i, successor, method));
                }
                
                if (ConstraintManager.useControlFlowPolymorphism) {
                    jsrTupleVar.getComponent(w, Variable.CMODE, JBCInstructionStackElemComponent.get(i))
                        .makeEqual(w, destV.getInstance(w, returnInstance));
                } else {
                    jsrTupleVar.getComponent(w, Variable.CMODE, JBCInstructionStackElemComponent.get(i))
                        .makeEqual(w, destV);
                }
            }
        } else {
            PredecessorEdgeInfo predInfo = new PredecessorEdgeInfo();
            boolean isJSRTarget = successorIndex == 1 && (opcode == OP_jsr || opcode == OP_jsr_w);
            InstanceLabel edgeInstance = (ConstraintManager.useControlFlowPolymorphism || isJSRTarget)
                ? JBCInstructionTransferInstance.get(method, offset, successor, successorIndex)
                : null;
            
            predInfo.predOffset = offset;
            predInfo.edgeInstance = edgeInstance;
            predInfo.localsDefinedInBlock = (Variable[])entryDefinedLocalVars.clone();
            predInfo.next = dest.firstPredecessor;
            dest.firstPredecessor = predInfo;
        
            if (ConstraintManager.useControlFlowPolymorphism || isJSRTarget) {
                if (manager.makeExceptionComponents) {
                    dest.exceptionVar.getInstance(w, edgeInstance).makeEqual(w, source.exceptionVar);
                }
                
                if (manager.makeGlobalComponents) {
                    dest.globalsVar.getInstance(w, edgeInstance).makeEqual(w, source.globalsVar);
                }
                
                dest.returnVar.getInstance(w, edgeInstance).makeEqual(w, source.returnVar);
            } else {
                if (manager.makeExceptionComponents) {
                    dest.exceptionVar.makeEqual(w, source.exceptionVar);
                }
                
                if (manager.makeGlobalComponents) {
                    dest.globalsVar.makeEqual(w, source.globalsVar);
                }
                
                dest.returnVar.makeEqual(w, source.returnVar);
            }
            
            for (int i = 0; i < stackSize; i++) {
                Variable v = entryStackVars[i];

                if (v != null) {
                    Variable destV = dest.entryStackVars[i];
                    
                    if (destV == null) {
                        destV = new Variable(w);
                        dest.entryStackVars[i] = destV;

                        if (Globals.debug) manager.addToken(destV, new InternalStackElemDebugToken(i, successor, method));
                    }
                    
                    if (ConstraintManager.useControlFlowPolymorphism || isJSRTarget) {
                        destV.getInstance(w, edgeInstance).makeEqual(w, v);
                    } else {
                        destV.makeEqual(w, v);
                    }
                }
            }
            
            if (isFirstPredecessor) {
                System.arraycopy(entryStackValues, 0, dest.entryStackValues, 0, stackSize);
                System.arraycopy(entryLocalValues, 0, dest.entryLocalValues, 0, entryLocalValues.length);
            } else {
                for (int i = 0; i < stackSize; i++) {
                    AbstractStackValue val = entryStackValues[i];
                    
                    if (val == null || !val.isEqualValue(dest.entryStackValues[i])) {
                        dest.entryStackValues[i] = null;
                    }
                }

                for (int i = 0; i < entryLocalValues.length; i++) {
                    AbstractStackValue val = entryLocalValues[i];
                    
                    if (val == null || !val.isEqualValue(dest.entryLocalValues[i])) {
                        dest.entryLocalValues[i] = null;
                    }
                }
            }
            
            dest.predecessorsNotProcessed--;
            if (dest.predecessorsNotProcessed == 0) {
                blocksToDoFirst.addElement(dest);
            }
        }
    }
    
    private void makeInstructionConstraints(JBCInstructionInfo[] instructions,
        byte[] code, Vector blocksToDoFirst, Vector blocksToDoLast,
        int offset, JBCBlockInfo block,
        AbstractStackValue[] entryStackValues, Variable[] entryStackVars,
        AbstractStackValue[] entryLocalValues, Variable[] entryDefinedLocalVars,
        int stackSize, boolean[] catchesCalledInBlock) {
        int[] singletonSuccessor = null;
        World w = manager.getSolver();
        int maxStackSize = method.getData().getMaxStackWords();
        AbstractStackValue[] newStackValues = new AbstractStackValue[maxStackSize];
        Variable[] newStackVars = new Variable[maxStackSize];
            
        tailCall: while (true) {
            int pushCount = JBCCodeUtilities.getStackPushCount(method, code, offset);
            int delta = JBCCodeUtilities.getStackSizeDelta(method, code, offset);
            
            if (pushCount < 0) {
                pushCount = 0;
                delta = 0;
            }
            
            int popCount = pushCount - delta;
            int preservedStack = maxStackSize - Math.max(pushCount, popCount);
            int[] successors = JBCCodeUtilities.getReachableSuccessors(code, offset, singletonSuccessor);
            JBCInstructionInfo info = instructions[offset];
            Vector specialRoutings = null;
            
            for (int i = 0; i < popCount; i++) {
                AbstractStackValue val = entryStackValues[i];
                
                if (val != null) {
                    SpecialPredecessorRouting routing = val.notifyConsumed(i);
                    
                    if (routing != null) {
                        if (specialRoutings == null) {
                            specialRoutings = new Vector();
                        }
                        specialRoutings.addElement(routing);
                    }
                }
            }

            System.arraycopy(entryStackValues, popCount, newStackValues, pushCount, preservedStack);
            System.arraycopy(entryStackVars, popCount, newStackVars, pushCount, preservedStack);
            for (int i = 0; i < pushCount; i++) {
                newStackValues[i] = null;
                newStackVars[i] = null;
            }
            
            info.block = block;
            
            int localStoreIndex = JBCCodeUtilities.getLocalVariableStoreIndex(code, offset);
            Variable exception;
            
            if (localStoreIndex >= 0) {
                int opcode = code[offset] & 0xFF;
                
                exception = null;
                
                if (opcode == OP_iinc) {
                    AbstractStackValue val = entryLocalValues[localStoreIndex];
                    
                    if (val != null) {
                        try {
                            entryLocalValues[localStoreIndex] =
                                doBinaryMathOp(OP_iadd, val, new IntStackValue(code[offset + 2]));
                        } catch (InvalidAbstractValueTypeError ex) {
                        }
                    }
                } else if (opcode == OP_wide && (code[offset + 1] & 0xFF) == OP_iinc) {
                    AbstractStackValue val = entryLocalValues[localStoreIndex];
                    
                    if (val != null) {
                        try {
                            entryLocalValues[localStoreIndex] = doBinaryMathOp(OP_iadd, val,
                                new IntStackValue(JBCCodeUtilities.getUnsignedShortAt(code, offset + 4)));
                        } catch (InvalidAbstractValueTypeError ex) {
                        }
                    }
                } else {
                    entryLocalValues[localStoreIndex] = entryStackValues[0];
                    entryDefinedLocalVars[localStoreIndex] = entryStackVars[0];
                }
                
                for (int i = JBCCodeUtilities.getLocalVariableStoreCount(code, offset) - 1; i > 0; i--) {
                    entryLocalValues[localStoreIndex + i] = null;
                    entryDefinedLocalVars[localStoreIndex + i] = null;
                }
                
                for (int i = 0; i < catchesCalledInBlock.length; i++) {
                    catchesCalledInBlock[i] = false;
                }
            } else {
                exception = makeConstraints(instructions, code, offset,
                    entryStackValues, entryStackVars, entryLocalValues, entryDefinedLocalVars,
                    newStackValues, newStackVars, pushCount, delta, stackSize, successors);
            }

            if (pushCount == 1) {
                info.stackVarsChanged = newStackVars[0];
            } else if (pushCount > 1) {
                Variable[] vars = new Variable[pushCount];
                
                System.arraycopy(newStackVars, 0, vars, 0, pushCount);
                info.stackVarsChanged = vars;
            } else {
                info.stackVarsChanged = null;
            }
            
            int[] catchSuccessors = info.catchSuccessors;
            
            if (catchSuccessors != null) {
                CatchBlockData[] dataArray = method.getData().getCatchBlocks();
                
                for (int i = 0; i < catchSuccessors.length; i++) {
                    int catchIndex = catchSuccessors[i];
                    
                    if (catchIndex == -1) {
                        if (exception != null) {
                            block.exceptionVar.makeEqual(w, exception);
                        }
                    } else {
                        CatchBlockData catchBlock = dataArray[catchIndex];
                            
                        if (exception != null || !catchesCalledInBlock[catchIndex]) {
                            entryStackValues[0] = null;
                
                            if (exception != null) {
                                String className = catchBlock.getCatchType();
                                
                                if (className == null) {
                                    entryStackVars[0] = exception;
                                } else {
                                    entryStackVars[0] = manager.makeDowncast(exception,
                                        JBCCodeUtilities.resolveCodeClass(method, className));
                                }
                            } else {
                                entryStackVars[0] = null;
                            }                
                            
                            addBlockPredecessor(instructions, code,
                                blocksToDoFirst, blocksToDoLast, offset,
                                catchBlock.getHandlerPC(), -i - 1,
                                entryStackVars, entryStackValues, 1,
                                entryDefinedLocalVars, entryLocalValues, true);

                            catchesCalledInBlock[catchIndex] = true;
                        }
                    }
                }
                
                /* give the garbage collector something to feed on */
                info.catchSuccessors = null;
            }
            
            stackSize += delta;

            int lastSuccessorIndex = -1;
            
            for (int i = 0; i < successors.length; i++) {
                int successor = successors[i];
                
                if (successor >= 0) {
                    if (instructions[successor].block == null) {
                        lastSuccessorIndex = i;
                    } else if (Globals.debug && instructions[successor].block.offset != successor) {
                        Globals.localError("Landed in the middle of another block?");
                    } else {
                        if (specialRoutings == null) {
                            addBlockPredecessor(instructions, code,
                                blocksToDoFirst, blocksToDoLast, offset, successor, i,
                                newStackVars, newStackValues, stackSize,
                                entryDefinedLocalVars, entryLocalValues, false);
                        } else {
                            AbstractStackValue[] successorStackValues = (AbstractStackValue[])newStackValues.clone();
                            AbstractStackValue[] successorLocalValues = (AbstractStackValue[])entryLocalValues.clone();
                            Variable[] successorStackVars = (Variable[])newStackVars.clone();
                            Variable[] successorLocalVars = (Variable[])entryDefinedLocalVars.clone();
                            
                            for (Enumeration e = specialRoutings.elements(); e.hasMoreElements();) {
                                SpecialPredecessorRouting routing = (SpecialPredecessorRouting)e.nextElement();
                                
                                routing.rerouteVars(i, stackSize, successorStackValues, successorStackVars,
                                    successorLocalValues, successorLocalVars);
                            }
                            
                            addBlockPredecessor(instructions, code,
                                blocksToDoFirst, blocksToDoLast, offset, successor, i,
                                successorStackVars, successorStackValues, stackSize,
                                successorLocalVars, successorLocalValues, false);
                        }
                    }
                }
            }
            
            for (int i = 0; i < successors.length; i++) {
                int successor = successors[i];
                
                if (successor >= 0 && instructions[successor].block == null) {
                    if (i < lastSuccessorIndex) {
                        AbstractStackValue[] successorStackValues = (AbstractStackValue[])newStackValues.clone();
                        AbstractStackValue[] successorLocalValues = (AbstractStackValue[])entryLocalValues.clone();
                        Variable[] successorStackVars = (Variable[])newStackVars.clone();
                        Variable[] successorLocalVars = (Variable[])entryDefinedLocalVars.clone();
                            
                        if (specialRoutings != null) {
                            for (Enumeration e = specialRoutings.elements(); e.hasMoreElements();) {
                                SpecialPredecessorRouting routing = (SpecialPredecessorRouting)e.nextElement();
                                
                                routing.rerouteVars(i, stackSize, successorStackValues, successorStackVars,
                                    successorLocalValues, successorLocalVars);
                            }
                        }

                        makeInstructionConstraints(instructions, code,
                            blocksToDoFirst, blocksToDoLast,
                            successor, block,
                            successorStackValues, successorStackVars,
                            successorLocalValues, successorLocalVars,
                            stackSize, (boolean[])catchesCalledInBlock.clone());
                    } else {
                        AbstractStackValue[] tmp1 = newStackValues;
                        Variable[] tmp2 = newStackVars;
                        
                        newStackValues = entryStackValues;
                        newStackVars = entryStackVars;
                        entryStackValues = tmp1;
                        entryStackVars = tmp2;
                        
                        if (specialRoutings != null) {
                            for (Enumeration e = specialRoutings.elements(); e.hasMoreElements();) {
                                SpecialPredecessorRouting routing = (SpecialPredecessorRouting)e.nextElement();
                                
                                routing.rerouteVars(i, stackSize, newStackValues, newStackVars,
                                    entryLocalValues, entryDefinedLocalVars);
                            }
                        }
                        
                        offset = successor;
                        continue tailCall;
                    }
                }
            }
            
            return;
        }
    }

    private void setupFirstBlock(JBCBlockInfo block) {
        Variable v = manager.getMethodVar(context, method);
        World w = manager.getSolver();
        int localWords = method.getData().getMaxLocalWords();

        if (manager.makeGlobalComponents) {
            block.globalsVar = v.getComponent(w, Variable.CMODE, JBCMethodGlobalsArgComponent.get());
        } else {
            block.globalsVar = manager.getGlobalsVar();
        }
        
        if (manager.makeExceptionComponents) {
            block.exceptionVar = v.getComponent(w, Variable.CMODE, JBCMethodExceptionComponent.get());
        } else {
            block.exceptionVar = manager.getExceptionVar();
        }
        
        block.returnVar = v.getComponent(w, Variable.CMODE, JBCMethodResultComponent.get());
        block.entryStackVars = new Variable[0];
        block.entryStackValues = new AbstractStackValue[0];
        block.entryLocalVars = new Variable[localWords];
        block.entryLocalValues = new AbstractStackValue[localWords];

        if (Globals.debug) {
            if (manager.makeGlobalComponents) {
                manager.setJBCType(block.globalsVar, ConstraintManager.GLOBALS);
                manager.addToken(block.globalsVar, new FormalGlobalsDebugToken(method));
            }
            
            if (manager.makeExceptionComponents) {
                manager.setJBCType(block.exceptionVar, JBCType.OBJECT);
                manager.addToken(block.exceptionVar, new FormalExceptionDebugToken(method));
            }
        
            manager.addToken(block.returnVar, new FormalResultDebugToken(method));
        }
        
        if (manager.makeExceptionsGlobal) {
            block.exceptionVar.setGlobal(w);
        }
        block.globalsVar.setGlobal(w);
    }
    
    private void makeAllConstraints(JBCInstructionInfo[] instructions, byte[] code) {
        boolean[] blocksDone = new boolean[instructions.length];
        Vector blocksToDoLast = new Vector();
        Vector blocksToDoFirst = new Vector();
        int index = 0;
        int numCatchBlocks = method.getData().getCatchBlocks().length;
        int numStackWords = method.getData().getMaxStackWords();
        JBCBlockInfo firstBlock = instructions[0].block;
        
        setupFirstBlock(firstBlock);
        blocksToDoLast.addElement(firstBlock);
        
        do {
            JBCBlockInfo block = (JBCBlockInfo)blocksToDoLast.elementAt(index);
            
            if (!blocksDone[block.offset]) {
                int firstIndex = 0;
                AbstractStackValue[] stackValues = block.entryStackValues;
                AbstractStackValue[] localValues = block.entryLocalValues;
                
                for (int i = 0; i < stackValues.length; i++) {
                    stackValues[i] = new UnknownObjectStackValue();
                }
                
                for (int i = 0; i < localValues.length; i++) {
                    localValues[i] = new UnknownObjectStackValue();
                }
                
                blocksToDoFirst.addElement(block);
                
                do {
                    block = (JBCBlockInfo)blocksToDoFirst.elementAt(firstIndex);
                    
                    if (!blocksDone[block.offset]) {
                        blocksDone[block.offset] = true;
                        
                        AbstractStackValue[] fullStackValues = new AbstractStackValue[numStackWords];
                        Variable[] fullStackVars = new Variable[numStackWords];
                        
                        System.arraycopy(block.entryStackValues, 0, fullStackValues, 0, block.entryStackValues.length);
                        System.arraycopy(block.entryStackVars, 0, fullStackVars, 0, block.entryStackVars.length);
                        
                        makeInstructionConstraints(instructions, code,
                            blocksToDoFirst, blocksToDoLast,
                            block.offset, block,
                            fullStackValues, fullStackVars,
                            block.entryLocalValues, (Variable[])block.entryLocalVars.clone(),
                            block.entryStackVars.length, new boolean[numCatchBlocks]);
                            
                        if (firstIndex > 0) {
                            block.entryStackValues = null;
                            block.entryLocalValues = null;
                        }
                    }
                    
                    firstIndex++;
                } while (firstIndex < blocksToDoFirst.size());
                
                blocksToDoFirst.removeAllElements();
            }

            index++;
        } while (index < blocksToDoLast.size());
    }
    
    private void addFieldInitialization(JBCBlockInfo block, boolean statics) {
        ComponentLabel result = JBCMethodResultComponent.get();
        Variable thisVar = null;
        Variable globalsVar = block.globalsVar;
        World w = manager.getSolver();
        int offset = -10;
        
        for (Enumeration e = method.getContainingClass().getFields(); e.hasMoreElements();) {
            JBCField f = (JBCField)e.nextElement();
            
            if (f.isStatic() == statics) {
                Variable v;
                
                if (statics) {
                    v = manager.makeStaticField(Variable.CMODE, globalsVar, f, Variable.CMODE);
                } else {
                    if (thisVar == null) {
                        thisVar = block.entryLocalVars[0];
                        if (thisVar == null) {
                            thisVar = new Variable(w);
                            block.entryLocalVars[0] = thisVar;
                        }
                    }

                    v = manager.makeNonstaticField(Variable.CMODE, thisVar, f, Variable.CMODE);
                }
                
                if (f.getData().getInitialValue() instanceof String) {
                    manager.makeStringConst(block.globalsVar).makeEqual(w, v);
                    manager.makeInitializedValue(v);
                } else if (!(f.getFieldType() instanceof JBCObjectType)) {
                    manager.makeInitializedValue(v);
                }
            }
            
            offset--;
        }
    }

/**
Adds initialization of the class c if there's no guarantee that it's already initialized.
It's guaranteed to be initialized if it's a superclass of the class this method is in.
*/
    private void addClassInitializationIfNeeded(JBCBlockInfo block, int offset, JBCClass c) {
        JBCClass thisClass = method.getContainingClass();
        
        if (!thisClass.isSubclassOf(c)) {
            addClassInitialization(block, offset, c);
        }
    }
    
/**
@return true if the caller should initialize the superclass too.
*/
    static boolean doClassInitialization(World w, ConstraintManager manager, Variable globalsVar, Variable exceptionVar,
        InvocationContext callContext, InstanceLabel initializerInstance,
        InstanceLabel wrapperInstance, JBCClass c) {
        Variable initializer = manager.getClassInitializerMethod(callContext, c);
        
        if (initializer != null) {
            Variable init = initializerInstance != null
                ? initializer.getInstance(w, initializerInstance)
                : initializer;
            Variable exceptionWrapper = manager.getSpecialVar(ConstraintManager.VAR_WRAP_INIT_EXN)
                .getInstance(w, wrapperInstance);
            
            if (manager.makeGlobalComponents) {
                ComponentLabel globals = JBCMethodGlobalsArgComponent.get();
                
                init.getComponent(w, Variable.DMODE, globals).makeEqual(w, globalsVar);
                exceptionWrapper.getComponent(w, Variable.DMODE, globals)
                    .makeEqual(w, globalsVar);
            }
            
            Variable initExn;
            
            if (manager.makeExceptionComponents) {
                initExn = init.getComponent(w, Variable.DMODE, JBCMethodExceptionComponent.get());
            } else {
                initExn = manager.getExceptionVar();
            }
            
            exceptionWrapper.getComponent(w, Variable.DMODE, JBCMethodArgComponent.get(0))
                .makeEqual(w, initExn);
            exceptionWrapper.getComponent(w, Variable.DMODE, JBCMethodResultComponent.get())
                .makeEqual(w, exceptionVar);
                
            return false;
        } else { // handle the case where the class has no <clinit> method. We still need to cons together the static globals
            ComponentLabel result = JBCMethodResultComponent.get();
            
            for (Enumeration e = c.getFields(); e.hasMoreElements();) {
                JBCField f = (JBCField)e.nextElement();
                
                if (f.isStatic()) {
                    Variable v = manager.makeStaticField(Variable.CMODE, globalsVar, f, Variable.CMODE);
                    
                    if (f.getData().getInitialValue() instanceof String) {
                        manager.makeStringConst(globalsVar).makeEqual(w, v);
                        manager.makeInitializedValue(v);
                    } else if (!(f.getFieldType() instanceof JBCObjectType)) {
                        manager.makeInitializedValue(v);
                    }
                }
            }
            
            return true;
        }
    }
    
    private void addClassInitialization(JBCBlockInfo block, int offset, JBCClass c) {
        boolean doSuperClass;
        World w = manager.getSolver();
        
        if (manager.hoistClassInitializers) {
            if (manager.addTopLevelClassInitialization(c)) {
                Variable v = manager.getClassInitializerFunction();
                Variable globalsVar;
                Variable exceptionVar;
                
                if (manager.makeGlobalComponents) {
                    globalsVar = v.getComponent(w, JBCMethodGlobalsArgComponent.get());
                } else {
                    globalsVar = manager.getGlobalsVar();
                }
                
                if (manager.makeExceptionComponents) {
                    exceptionVar = v.getComponent(w, JBCMethodExceptionComponent.get());
                } else {
                    exceptionVar = manager.getExceptionVar();
                }
                
                doSuperClass = doClassInitialization(w, manager, globalsVar,
                    exceptionVar, manager.getRootInvocationContext(),
                    null, JBCExceptionWrapperInstance.get(method, offset), c);
            } else {
                doSuperClass = false;
            }
        } else {
            doSuperClass = doClassInitialization(w, manager, block.globalsVar, block.exceptionVar,
                JBCMethodInvocationContext.get(context, method, offset),
                JBCMethodInvocationInstance.get(context, method, offset),
                JBCExceptionWrapperInstance.get(method, offset), c);
        }
        
        if (doSuperClass) {
            JBCClass superClass = c.getSuperClass();
            
            if (superClass != null) {
                addClassInitializationIfNeeded(block, offset, superClass);
            }
        }
    }
    
    private void getExceptionInfo(int offset, byte[] code, Hashtable types) {
        String[] exceptionList = JBCCodeUtilities.getExceptionList(code, offset);
        
        for (int i = 0; i < exceptionList.length; i++) {
            String exn = exceptionList[i];
            
            if (!exn.equals("java.lang.Throwable")) {
                types.put(exn, exn);
            }
        }
        
        types.put("java.lang.VirtualMachineError", "java.lang.VirtualMachineError");
        types.put("java.lang.ThreadDeath", "java.lang.ThreadDeath");
    }
    
    private void mergeCommonExceptions(Variable exceptionVar,
        JBCClass catchClass, int offset, Variable globalsVar) {
        World w = manager.getSolver();
        ComponentLabel resultComponent = JBCMethodResultComponent.get();
        Variable v = manager.getSpecialVar(ConstraintManager.VAR_INS_EXN)
            .getInstance(w, JBCExceptionThrowInstance.get(method, offset));
        
        if (manager.makeGlobalComponents) {
            ComponentLabel globals = JBCMethodGlobalsArgComponent.get();
            
            v.getComponent(w, Variable.DMODE, globals).makeEqual(w, globalsVar);
        }
            
        Variable exception = v.getComponent(w, Variable.DMODE, resultComponent);
        
        // What if the creation of the exception throws an exception?
        // Unsurprisingly, the JVM Spec is silent on this point. We assume it can't
        // happen.
        
        if (catchClass != null) {
            exception = manager.makeDowncast(exception, catchClass);
        }
        
        exception.makeEqual(w, exceptionVar);
    }
    
    private void finishExceptions(JBCInstructionInfo[] instructions, byte[] code) {
        CatchBlockData[] catchBlocks = method.getData().getCatchBlocks();
        
        for (int i = 0; i < catchBlocks.length; i++) {
            CatchBlockData data = catchBlocks[i];
            int handlerPC = data.getHandlerPC();
            JBCBlockInfo block = instructions[handlerPC].block;
            
            if (block.exceptionVar != null) {
                Variable exceptionVar = block.entryStackVars[0];
                String catchClassName = data.getCatchType();
                JBCClass catchClass = catchClassName != null ? JBCCodeUtilities.resolveCodeClass(method, catchClassName) : null;
                
                if (catchClass == null
                    || manager.catchesInstructionExceptions(catchClass)) {
                    mergeCommonExceptions(exceptionVar, catchClass, block.offset, block.globalsVar);
                }
            }
        }
    }
    
    static void fullyRouteLocalVar(ConstraintManager manager, JBCInstructionInfo[] instructions, byte[] code,
        JBCBlockInfo block, int index, int doneUpToOffset) {
        Variable v = block.entryLocalVars[index];
        World w = manager.getSolver();
        JBCReturnInfo returnInfo = block.returnInfo;
        
        if (returnInfo != null) {
            ComponentLabel localComponent = JBCInstructionLocalVarComponent.get(index);
            int[] retToThis = returnInfo.retToThisInstructionOffsets;
            
            if (ConstraintManager.useControlFlowPolymorphism) {
                v.getInstance(w, returnInfo.instance)
                    .makeEqual(w, returnInfo.jsrTupleVar.getComponent(w, localComponent));
            } else {
                v.makeEqual(w, returnInfo.jsrTupleVar.getComponent(w, localComponent));
            }
            
            for (int i = 0; i < retToThis.length; i++) {
                int retOffset = retToThis[i];
                JBCBlockInfo retBlock = instructions[retOffset].block;
                Variable[] retBlockVars = retBlock.entryLocalVars;
                
                if (retBlockVars[index] == null) {
                    retBlockVars[index] =
                        retBlockVars[JBCCodeUtilities.getLocalVariableLoadIndex(code, retOffset)]
                            .getComponent(w, localComponent);
                            
                    if (retOffset < doneUpToOffset) {
                        fullyRouteLocalVar(manager, instructions, code, retBlock, index, doneUpToOffset);
                    }
                }
            }
        }
        
        for (PredecessorEdgeInfo pred = block.firstPredecessor; pred != null;
            pred = pred.next) {
            Variable def = pred.localsDefinedInBlock[index];
            Variable vInstance = pred.edgeInstance != null ? v.getInstance(w, pred.edgeInstance) : v;
            
            if (def != null) {
                vInstance.makeEqual(w, def);
            } else {
                JBCBlockInfo predBlock = instructions[pred.predOffset].block;
                Variable predVar = predBlock.entryLocalVars[index];
                
                if (predVar == null) {
                    predVar = new Variable(w);
                    predBlock.entryLocalVars[index] = predVar;
                    
                    if (pred.predOffset < doneUpToOffset) {
                        fullyRouteLocalVar(manager, instructions, code, predBlock, index, doneUpToOffset);
                    }
                }
                
                vInstance.makeEqual(w, predVar);
            }
        }
    }
    
    private void completeRouting(JBCInstructionInfo[] instructions, byte[] code) {
        for (int i = 0; i < instructions.length; i++) {
            JBCInstructionInfo info = instructions[i];
            
            if (info != null) {
                JBCBlockInfo block = info.block;
                
                if (block != null && block.offset == i) {
                    Variable[] entryLocalVars = block.entryLocalVars;
                    
                    if (entryLocalVars != null) {
                        for (int j = 0; j < entryLocalVars.length; j++) {
                            if (entryLocalVars[j] != null) {
                                fullyRouteLocalVar(manager, instructions, code, block, j, i);
                            }
                        }
                    }
                }
            }
        }
    }
    
    private void hookupMethodParameters(JBCBlockInfo block) {
        Variable v = manager.getMethodVar(context, method);
        World w = manager.getSolver();
        JBCMethodType methodType = method.getMethodType();
        JBCType[] paramTypes = methodType.getParameterTypes();
        int wordCount = 0;
        int thisParam = method.isStatic() ? -1 : 0;
        Variable[] entryVars = block.entryLocalVars;
        
        for (int i = 0; i < paramTypes.length && wordCount < entryVars.length; i++) {
            JBCType paramType = paramTypes[i];
            Variable paramVar = entryVars[wordCount];
            Variable actualVar = v.getComponent(w, Variable.CMODE,
                JBCMethodArgComponent.get(i));
            
            if (paramVar == null) {
                paramVar = new Variable(w);
                entryVars[wordCount] = paramVar;
            }
            
            if (Globals.debug) {
                manager.addToken(actualVar, new FormalParameterDebugToken(i, method));
            }
            
            if (i == thisParam) {
                actualVar = manager.makeDowncast(actualVar, method.getContainingClass());
            }
            
            paramVar.makeEqual(w, actualVar);
            manager.setJBCType(paramVar, paramType);

            wordCount += paramType.getWordSize();
        }
    }   
    
    void makeLive() {
        try {
            byte[] code = method.getData().getCode();
            
            if (code == null) {
                Globals.nonlocalError("Cannot generate constraints for an abstract method");
            }
        
            String name = method.getMethodName();
            
            JBCInstructionInfo[] instructions = makeInstructions(code);       
            
            addCatchBlockSuccessors(instructions, code);
            makeAllConstraints(instructions, code);
            
            JBCBlockInfo startBlock = instructions[0].block;
            
            if (name.equals("<clinit>")) {
                addFieldInitialization(startBlock, true);

                JBCClass superClass = method.getContainingClass().getSuperClass();
            
                if (superClass != null) {
                    addClassInitialization(startBlock, -1, superClass);
                }
            } else if (name.equals("<init>")) {
                addFieldInitialization(startBlock, false);
                addClassInitialization(startBlock, -1, method.getContainingClass());
            } else if (method.isStatic()) {
                addClassInitialization(startBlock, -1, method.getContainingClass());
            }

            finishExceptions(instructions, code);
            completeRouting(instructions, code);
            hookupMethodParameters(startBlock);
            
            manager.setBytecodeExpressionEvaluator(method,
                new BytecodeExpressionEvaluator(method, instructions, code));
        } catch (InvalidClassDataError ex) {
            Globals.userError("Error in class data while analyzing method "
                + method + " (" + ex.getMessage() + ")");
        } catch (InconsistentProgramError ex) {
            Globals.userError("Program is inconsistent: " + ex.getMessage());
            /* XXX set things up for retrying */
        }
    }
}
