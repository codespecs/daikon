/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.typechecker;

import ajax.jbc.*;
import ajax.Globals;
import java.util.*;
import ajax.util.*;

public class VarNumbering implements OpcodeConstants {
    private JBCMethod method;
    
    private NumberedVar[][] stackNumbers;
    private NumberedVar[][] varNumbers;
    private int maxLocals;
    private int maxStack;
    
    public VarNumbering(JBCMethod method) {
        this.method = method;
        
        MethodData data = method.getData();
        int codeLen = data.getCode().length;
        
        maxLocals = data.getMaxLocalWords();
        maxStack = data.getMaxStackWords();
        stackNumbers = new NumberedVar[codeLen][maxStack];
        varNumbers = new NumberedVar[codeLen][maxLocals];
        
        makeNumbers();
    }
    
    private static void addValue(NumberedVar[][] numbers, int offset, int index, NumberedVar v) {
        if (v != null) {
            NumberedVar current = numbers[offset][index];
            
            if (current == null) {
                numbers[offset][index] = v;
            } else if (!current.equals(NumberedVar.getUnknown())
                && !current.equals(v)) {
                numbers[offset][index] = NumberedVar.getUnknown();
            }
        }
    }
    
    private void pushExceptionalInstructionValues(byte[] code, int i, int succ) {
        for (int k = 0; k < maxLocals; k++) {
            addValue(varNumbers, succ, k, varNumbers[i][k]);
        }
        
        addValue(stackNumbers, succ, 0, new NumberedVarComputation(i, -1));
    }
    
    private void pushInstructionValues(byte[] code, int i, int succ) {
        int opcode = code[i] & 0xFF;
        int pushCount = JBCCodeUtilities.getStackPushCount(method, code, i);
        int popCount = JBCCodeUtilities.getStackPopCount(method, code, i);
                
        if (opcode == OP_wide) {
            opcode = code[i + 1] & 0xFF;
        }
        
        switch (opcode) {
            case OP_jsr:
            case OP_jsr_w:
                return;
                        
            case OP_aload_0:
            case OP_iload_0:
            case OP_fload_0:
            case OP_dload_0:
            case OP_lload_0:
            case OP_aload_1:
            case OP_iload_1:
            case OP_fload_1:
            case OP_dload_1:
            case OP_lload_1:
            case OP_aload_2:
            case OP_iload_2:
            case OP_fload_2:
            case OP_dload_2:
            case OP_lload_2:
            case OP_aload_3:
            case OP_iload_3:
            case OP_fload_3:
            case OP_dload_3:
            case OP_lload_3:
            case OP_iload:
            case OP_aload:
            case OP_lload:
            case OP_fload:
            case OP_dload: {
                int loadIndex = JBCCodeUtilities.getLocalVariableLoadIndex(code, i);
                                
                for (int k = 0; k < pushCount; k++) {
                    addValue(stackNumbers, succ, k, varNumbers[i][loadIndex + k]);
                }
                break;
            }
                    
            case OP_iinc:
                addValue(varNumbers, succ, JBCCodeUtilities.getLocalVariableStoreIndex(code, i),
                    new NumberedVarComputation(i, 0));
                break;

            case OP_astore_0:
            case OP_istore_0:
            case OP_fstore_0:
            case OP_dstore_0:
            case OP_lstore_0:
            case OP_astore_1:
            case OP_istore_1:
            case OP_fstore_1:
            case OP_dstore_1:
            case OP_lstore_1:
            case OP_astore_2:
            case OP_istore_2:
            case OP_fstore_2:
            case OP_dstore_2:
            case OP_lstore_2:
            case OP_astore_3:
            case OP_istore_3:
            case OP_fstore_3:
            case OP_dstore_3:
            case OP_lstore_3:
            case OP_istore:
            case OP_astore:
            case OP_lstore:
            case OP_fstore:
            case OP_dstore: {
                int storeIndex = JBCCodeUtilities.getLocalVariableStoreIndex(code, i);
                                
                for (int k = 0; k < popCount; k++) {
                    addValue(varNumbers, succ, storeIndex + k, stackNumbers[i][k]);
                }
                break;
            }
            
            case OP_dup:
                addValue(stackNumbers, succ, 0, stackNumbers[i][0]);
                addValue(stackNumbers, succ, 1, stackNumbers[i][0]);
                break;
            
            case OP_dup_x1:
                addValue(stackNumbers, succ, 0, stackNumbers[i][0]);
                addValue(stackNumbers, succ, 1, stackNumbers[i][1]);
                addValue(stackNumbers, succ, 2, stackNumbers[i][0]);
                break;
                
            case OP_dup_x2:
                addValue(stackNumbers, succ, 0, stackNumbers[i][0]);
                addValue(stackNumbers, succ, 1, stackNumbers[i][1]);
                addValue(stackNumbers, succ, 2, stackNumbers[i][2]);
                addValue(stackNumbers, succ, 3, stackNumbers[i][0]);
                break;
                
            case OP_dup2:
                addValue(stackNumbers, succ, 0, stackNumbers[i][0]);
                addValue(stackNumbers, succ, 1, stackNumbers[i][1]);
                addValue(stackNumbers, succ, 2, stackNumbers[i][0]);
                addValue(stackNumbers, succ, 3, stackNumbers[i][1]);
                break;
            
            case OP_dup2_x1:
                addValue(stackNumbers, succ, 0, stackNumbers[i][0]);
                addValue(stackNumbers, succ, 1, stackNumbers[i][1]);
                addValue(stackNumbers, succ, 2, stackNumbers[i][2]);
                addValue(stackNumbers, succ, 3, stackNumbers[i][0]);
                addValue(stackNumbers, succ, 4, stackNumbers[i][1]);
                break;
            
            case OP_dup2_x2:
                addValue(stackNumbers, succ, 0, stackNumbers[i][0]);
                addValue(stackNumbers, succ, 1, stackNumbers[i][1]);
                addValue(stackNumbers, succ, 2, stackNumbers[i][2]);
                addValue(stackNumbers, succ, 3, stackNumbers[i][3]);
                addValue(stackNumbers, succ, 4, stackNumbers[i][0]);
                addValue(stackNumbers, succ, 5, stackNumbers[i][1]);
                break;
            
            case OP_swap:
                addValue(stackNumbers, succ, 0, stackNumbers[i][1]);
                addValue(stackNumbers, succ, 1, stackNumbers[i][0]);
                break;
            
            case OP_checkcast:
                addValue(stackNumbers, succ, 0, stackNumbers[i][0]);
                break;

            default:
                for (int k = 0; k < pushCount; k++) {
                    addValue(stackNumbers, succ, k, new NumberedVarComputation(i, k));
                }
        }
        
        for (int k = popCount; k < maxStack; k++) {
            int newK = k - popCount + pushCount;
                                    
            if (0 <= newK && newK < maxStack) {
                addValue(stackNumbers, succ, newK, stackNumbers[i][k]);
            }
        }
                                
        for (int k = 0; k < maxLocals; k++) {
            if (!JBCCodeUtilities.isLocalVariableStored(code, i, k)) {
                addValue(varNumbers, succ, k, varNumbers[i][k]);
            }
        }
    }
    
    private int[] computeAllSuccessors(byte[] code, int offset, int[] singleton, int[] returnDests, int[] exceptionIndex) {
        MethodData data = method.getData();
        CatchBlockData[] catches = data.getCatchBlocks();
        int[] ss = JBCCodeUtilities.getReachableSuccessors(code, offset, singleton);
        int numCatches = 0;
        int opcode = code[offset] & 0xFF;

        switch (opcode) {
            case OP_jsr:
            case OP_jsr_w:
                singleton[0] = ss[0];
                ss = singleton;
                break;
            
            case OP_ret:
                ss = returnDests;
                break;
        }
        
        for (int i = 0; i < catches.length; i++) {
            if (catches[i].getStartPC() <= offset && offset < catches[i].getEndPC()) {
                numCatches++;
            }
        }
        
        exceptionIndex[0] = ss.length;
        
        if (numCatches == 0) {
            return ss;
        } else {
            int[] newSS = new int[ss.length + numCatches];
            int index = ss.length;
            
            System.arraycopy(ss, 0, newSS, 0, ss.length);
            for (int i = 0; i < catches.length; i++) {
                if (catches[i].getStartPC() <= offset && offset < catches[i].getEndPC()) {
                    newSS[index] = catches[i].getHandlerPC();
                    index++;
                }
            }
        
            return newSS;
        }
    }
    
    private void makeNumbers() {
        MethodData data = method.getData();
        byte[] code = data.getCode();
        boolean[] starts = JBCCodeUtilities.getInstructionStarts(data);
        int[] singleton = new int[1];
        int numParamWords = JBCType.getMethodParamsSize(method.getMethodTypeName());
        int[] returnDests = new int[0];
        int[] exceptionIndex = new int[1];
        
        if (!method.isStatic()) {
            numParamWords++;
        }
        
        for (int i = 0; i < numParamWords; i++) {
            varNumbers[0][i] = new NumberedVarParam(i);
        }
        
        for (int i = 0; i < starts.length; i++) {
            if (starts[i]) {
                int opcode = code[i] & 0xFF;
                
                if (opcode == OP_jsr || opcode == OP_jsr_w) {
                    int[] successors = JBCCodeUtilities.getReachableSuccessors(code, i);
                    int[] newDests = new int[returnDests.length + 1];
                    
                    System.arraycopy(returnDests, 0, newDests, 0, returnDests.length);
                    newDests[returnDests.length] = successors[0];
                    returnDests = newDests;
                }
            }
        }
        
        for (int i = 0; i < starts.length; i++) {
            if (starts[i]) {
                int[] successors = computeAllSuccessors(code, i, singleton, returnDests, exceptionIndex);
                
                for (int j = 0; j < successors.length; j++) {
                    if (successors[j] <= i) {
                        makeAllValuesUnknown(successors[j]);
                    }
                }
            }
        }
        
        for (int i = 0; i < starts.length; i++) {
            if (starts[i]) {
                int[] successors = computeAllSuccessors(code, i, singleton, returnDests, exceptionIndex);
                
                for (int j = 0; j < successors.length; j++) {
                    int succ = successors[j];
                    
                    if (succ > i) {
                        if (j < exceptionIndex[0]) {
                            pushInstructionValues(code, i, succ);
                        } else {
                            pushExceptionalInstructionValues(code, i, succ);
                        }
                    }
                }
            }
        }
    }
    
    private void makeAllValuesUnknown(int offset) {
        makeAllUnknown(stackNumbers[offset]);
        makeAllUnknown(varNumbers[offset]);
    }
    
    private void makeAllUnknown(NumberedVar[] vs) {
        NumberedVar v = NumberedVar.getUnknown();
        
        for (int i = 0; i < vs.length; i++) {
            vs[i] = v;
        }
    }
    
    public JBCMethod getMethod() {
        return method;
    }
    
    private static NumberedVar makeKnown(NumberedVar v) {
        if (NumberedVar.getUnknown() == v) {
            return null;
        } else {
            return v;
        }
    }
    
    public NumberedVar getStackNumber(int offset, int index) {
        return makeKnown(stackNumbers[offset][index]);
    }
    
    public NumberedVar getVariableNumber(int offset, int index) {
        return makeKnown(varNumbers[offset][index]);
    }
}
