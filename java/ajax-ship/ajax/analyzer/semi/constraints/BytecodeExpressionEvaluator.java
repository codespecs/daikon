/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.*;
import ajax.solver.*;
import ajax.Globals;
import java.util.Vector;
import ajax.analyzer.*;

/**
This class provides the mechanism that maps a primitive bytecode expression
(a location specified as an instruction offset, plus either a stack element
index or a local variable index) to a Variable. Each one of these objects
corresponds to a particular method.

It is a bit tricky, because we want it to be small (there will be one of
these for each method analyzed). Also, it's possible for the client to ask
for a local variable in a context where it's dead --- thus we need to add
routing constraints to pass the value of the variable to the right context.
(This can't happen for stack variables since they are always fully routed
by the JBCConstraintGenerator).

The constraint manager uses the "compress" and "uncompress" methods to manage
storage. The informational methods will only be called when the object is in
the "uncompressed" state. For now, we adopt a "big" representation. Currently
the implementation simply throws away all data when "compressed", and throws an
exception if the data is subsequently "uncompressed".
*/
class BytecodeExpressionEvaluator implements OpcodeConstants {
    private JBCInstructionInfo[] instructions;
    private JBCMethod method;
    private int[] singletonPredecessors;
    private byte[] code;
    
    BytecodeExpressionEvaluator(JBCMethod method, JBCInstructionInfo[] instructions, byte[] code) {
        this.method = method;
        this.instructions = instructions;
        this.code = code;
        
        singletonPredecessors = computeSingletonPredecessors(instructions, code);
    }
    
    JBCMethod getMethod() {
        return method;
    }
    
    private static int[] computeSingletonPredecessors(JBCInstructionInfo[] info, byte[] code) {
        int[] result = new int[info.length];
        int[] singleton = new int[1];
        
        for (int i = 0; i < result.length; i++) {
            if (info[i] == null) {
                result[i] = -1;
            } else if (info[i].block == null) {
                info[i] = null;
                result[i] = -1;
            } else {
                int[] successors = JBCCodeUtilities.getReachableSuccessors(code, i, singleton);
                
                if (info[i].block.offset == i) {
                    result[i] = -1;
                }
                
                for (int j = 0; j < successors.length; j++) {
                    int successor = successors[j];
                    
                    if (result[successor] == 0) {
                        result[successor] = i;
                    }
                }
            }
        }
        
        return result;
    }
    
    Variable getVar(ConstraintManager manager, int offset, JBCExpression expression) {
        if (expression instanceof JBCStackElemExpression) {
            return getStackElementVar(manager, offset,
                ((JBCStackElemExpression)expression).getStackElemIndex());
        } else if (expression instanceof JBCLocalVarExpression) {
            return getLocalVariableVar(manager, offset,
                ((JBCLocalVarExpression)expression).getLocalVarIndex());
        } else if (expression instanceof JBCStaticFieldExpression) {
            Variable globalsVar = getGlobalsVar(manager, offset);
            
            if (globalsVar == null) {
                return null;
            } else {
                return manager.getStaticFieldVar(globalsVar,
                    ((JBCStaticFieldExpression)expression).getField());
            }
        } else if (expression instanceof JBCStaticUserFieldExpression) {
            Variable globalsVar = getGlobalsVar(manager, offset);
            
            if (globalsVar == null) {
                return null;
            } else {
                return manager.getStaticUserFieldVar(getGlobalsVar(manager, offset),
                    ((JBCStaticUserFieldExpression)expression).getField());
            }
        } else {
            throw Globals.nonlocalError("Invalid expression type for a bytecode method");
        }
    }
    
/**
Gets the Variable for a stack element's value on entry to a given
bytecode instruction. Returns "null" if the variable has no value at
that instruction.

@param manager the constraint environment
@param offset the offset of the bytecode instruction
@param index the index of the stack element
@return the corresponding Variable, or null if there isn't one
*/
    private Variable getStackElementVar(ConstraintManager manager, int offset, int element) {
        if (Globals.debug && instructions == null) {
            Globals.localError("Cannot operate on compressed evaluator");
        }
        
        if (Globals.debug && element < 0) {
            Globals.nonlocalError("Negative stack elements cannot exist");
            return null;
        }
        
        if (Globals.debug && !(offset >= 0 && offset < singletonPredecessors.length)) {
            Globals.nonlocalError("Out-of-bounds instruction offset detected");
            return null;
        }
        
        int predecessor;
        
        while ((predecessor = singletonPredecessors[offset]) != -1) {
            offset = predecessor;
            
            Object changedElements = instructions[offset].stackVarsChanged;
            
            if (changedElements != null) {
                if (changedElements instanceof Variable[]) {
                    Variable[] elemArray = (Variable[])changedElements;
                    
                    if (element < elemArray.length) {
                        if (elemArray[element] == null) {
                            return null;
                        } else {
                            Variable v = elemArray[element].getHead();
                        
                            elemArray[element] = v;
                            return v;
                        }
                    }
                } else {
                    if (element < 1) {
                        Variable v = ((Variable)changedElements).getHead();
                        
                        changedElements = v;
                        return v;
                    }
                }
            }
            
            element -= JBCCodeUtilities.getStackSizeDelta(method, code, offset);
        }
        
        if (instructions[offset] == null) {
            return null;
        }
        
        Variable[] elemArray = instructions[offset].block.entryStackVars;
        
        if (elemArray != null && element < elemArray.length) {
            Variable v = elemArray[element];
            
            elemArray[element] = v;
            return v;
        } else {
            return null;
        }
    }
    
/**
Gets the Variable for a local variable's value on entry to a given
bytecode instruction. Returns "null" if the variable has no value at
that instruction.

@param manager the constraint environment
@param offset the offset of the bytecode instruction
@param index the index of the local variable
@return the corresponding Variable, or null if there isn't one
*/
    private Variable getLocalVariableVar(ConstraintManager manager, int offset, int index) {
        if (Globals.debug && instructions == null) {
            Globals.localError("Cannot operate on compressed evaluator");
        }
        
        if (Globals.debug && !(offset >= 0 && offset < singletonPredecessors.length)) {
            Globals.nonlocalError("Out-of-bounds instruction offset detected");
            return null;
        }
        
        int predecessor;
        
        while ((predecessor = singletonPredecessors[offset]) != -1) {
            offset = predecessor;
            
            if (JBCCodeUtilities.getLocalVariableStoreIndex(code, offset) == index) {
                return getStackElementVar(manager, offset, 0);
            }
        }
        
        if (instructions[offset] == null) {
            return null;
        }
        
        JBCBlockInfo block = instructions[offset].block;
        Variable[] elemArray = block.entryLocalVars;
        
        if (index < 0) {
            Globals.nonlocalError("Local variable index cannot be negative");
            return null;
        } else if (index >= elemArray.length) {
            return null;
        } else {
            Variable result = elemArray[index];
            
            if (result == null) {
                result = new Variable(manager.getSolver());
                elemArray[index] = result;
                JBCConstraintGenerator.fullyRouteLocalVar(manager, instructions, code,
                    block, index, code.length);
            }
            
            return result;
        }
    }
    
    private Variable getGlobalsVar(ConstraintManager manager, int offset) {
        if (Globals.debug && instructions == null) {
            Globals.localError("Cannot operate on compressed evaluator");
        }
        
        if (Globals.debug && !(offset >= 0 && offset < singletonPredecessors.length)) {
            Globals.nonlocalError("Out-of-bounds instruction offset detected");
            return null;
        }
        
        if (instructions[offset] == null) {
            return null;
        }
        
        return instructions[offset].block.globalsVar;
    }
    
    void compress() {
        singletonPredecessors = null;
        code = null;
    }
    
    void uncompress() {
        code = method.getData().getCode();
        singletonPredecessors = computeSingletonPredecessors(instructions, code);
    }
}
