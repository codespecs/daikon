/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import ajax.Globals;

public class JBCCodeUtilities implements OpcodeConstants, DataConstants {
    /* These inscrutable tables are derived from the data file 'opcodelist' by the makeClass.pl script. */
    
    private static byte[] opcodeLengths = {
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 2, 3, 3, 2, 2,
        2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3,
        3, 5, 1, 3, 2, 3, 1, 1, 3, 3, 1, 1, 1, 4, 3, 3, 4, 4
    };
    
    private static byte[] opcodeStackSizeDeltas = {
        0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 2,
        1, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,
        -1, 0, -1, 0, -1, -1, -1, -1, -1, -2, -1, -2, -1, -1, -1, -1, -1, -2,
        -2, -2, -2, -1, -1, -1, -1, -2, -2, -2, -2, -1, -1, -1, -1, -3, -4,
        -3, -4, -3, -3, -3, -3, -1, -2, 1, 1, 1, 2, 2, 2, 0, -1, -2, -1, -2,
        -1, -2, -1, -2, -1, -2, -1, -2, -1, -2, -1, -2, -1, -2, -1, -2, 0, 0,
        0, 0, -1, -1, -1, -1, -1, -1, -1, -2, -1, -2, -1, -2, 0, 1, 0, 1, -1,
        -1, 0, 0, 1, 1, -1, 0, -1, 0, 0, 0, -3, -1, -1, -3, -3, -1, -1, -1,
        -1, -1, -1, -2, -2, -2, -2, -2, -2, -2, -2, 0, 1, 0, -1, -1, -99,
        -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99, -99,
        1, 0, 0, 0, -99, 0, 0, -1, -1, -99, -99, -1, -1, 0, 1
    };
        
    private static byte[] opcodeStackPushes = {
        0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 2,
        1, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,
        1, 2, 1, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 4,
        4, 5, 6, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
        2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1,
        2, 1, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, -99, -99, -99, -99, -99, -99, -99,
        0, -99, 0, -99, -99, -99, -99, -99, 1, 1, 1, 1, -99, 1, 1, 0, 0,
        -99, 1, 0, 0, 0, 1
    };
    
    private static byte[] opcodeExceptionListNum = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 1, 1, 1, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3,
        0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 4, 4, 5, 5, 6, 6, 7, 6, 0, 4, 8, 8, 9, 10, 11, 4, 9, 12, 0,
        13, 0, 0, 0, 0
    };
    
    private static String[][] opcodeExceptionLists = {
        {},
        { "java.lang.NullPointerException", "java.lang.ArrayIndexOutOfBoundsException" },
        { "java.lang.NullPointerException", "java.lang.ArrayIndexOutOfBoundsException", "java.lang.ArrayStoreException" },
        { "java.lang.ArithmeticException" },
        { "java.lang.LinkageError" },
        { "java.lang.NullPointerException", "java.lang.LinkageError" },
        { "java.lang.Throwable", "java.lang.NullPointerException", "java.lang.LinkageError" },
        { "java.lang.Throwable", "java.lang.LinkageError" },
        { "java.lang.NegativeArraySizeException" },
        { "java.lang.NullPointerException" },
        { "java.lang.Throwable", "java.lang.NullPointerException" },
        { "java.lang.LinkageError", "java.lang.ClassCastException" },
        { "java.lang.NullPointerException", "java.lang.IllegalMonitorStateException" },
        { "java.lang.LinkageError", "java.lang.NegativeArraySizeException" },
    };

    public static int getBytecodeOffset(JBCMethod m, int lineNumber) {
        LineNumberData[] lineData = m.getData().getLineNumbers();
        int bestLine = -1;
        int bestIndex = -1;

        if (lineData != null) {
            for (int i = 0; i < lineData.length; i++) {
                int startLine = lineData[i].getLineNumber();

                if (startLine <= lineNumber && startLine >= bestLine) {
                    bestLine = startLine;
                    bestIndex = i;
                }
            }
        }

        if (bestIndex >= 0) {
            return lineData[bestIndex].getStartingPC();
        } else {
            return -1;
        }
    }

    public static int getSourceLineNumber(JBCMethod m, int offset) {
        LineNumberData[] lineData = m.getData().getLineNumbers();
        int bestPC = -1;
        int bestIndex = -1;

        if (lineData != null) {
            for (int i = 0; i < lineData.length; i++) {
                int startPC = lineData[i].getStartingPC();

                if (startPC <= offset && startPC >= bestPC) {
                    bestPC = startPC;
                    bestIndex = i;
                }
            }
        }

        if (bestIndex >= 0) {
            return lineData[bestIndex].getLineNumber();
        } else {
            return -1;
        }
    }

    public static int getIntAt(byte[] code, int i) {
        return (code[i] << 24) | ((code[i + 1] & 0xFF) << 16)
            | ((code[i + 2] & 0xFF) << 8) | (code[i + 3] & 0xFF);
    }
    
    public static int getShortAt(byte[] code, int i) {
        return (code[i] << 8) | (code[i + 1] & 0xFF);
    }
    
    public static int getUnsignedShortAt(byte[] code, int i) {
        return ((code[i] & 0xFF) << 8) | (code[i + 1] & 0xFF);
    }
    
    public static CodeRefData getCodeRefAt(MethodData data, byte[] code, int offset) {
        return data.getCodeRef(getUnsignedShortAt(code, offset));
    }
    
    public static String getCodeClassAt(MethodData data, byte[] code, int offset) {
        return data.getCodeClass(getUnsignedShortAt(code, offset));
    }
    
    public static JBCClass resolveCodeClass(JBCMethod method, String className) {
        if (className == null) {
            throw new InvalidClassDataError("Bad class name in " + method);
        } else {
            JBCClass c = method.getContainingClass().getClassLoader().getClass(className);
            
            if (c == null) {
                Globals.writeLog(method, "WARNING: Class " + className + " not found in " + method);
            }
            
            return c;
        }
    }
    
    public static JBCType getCodeConstantType(JBCMethod method, byte[] code, int offset) {
        Object obj = getLoadedConstant(method, code, offset);
        
        if (obj instanceof String) {
            return JBCObjectType.get("java.lang.String",
                method.getContainingClass().getWorld().getSystemClassLoader());
        } else if (obj instanceof Integer) {
            return JBCType.INT;
        } else if (obj instanceof Long) {
            return JBCType.LONG;
        } else if (obj instanceof Float) {
            return JBCType.FLOAT;
        } else {
            if (Globals.debug && !(obj instanceof Double)) {
                Globals.nonlocalError("Invalid code constant type: " + obj);
            }
            
            return JBCType.DOUBLE;
        }
    }
    
    public static JBCClass resolveInstructionClass(JBCMethod method, byte[] code, int offset) {
        switch (code[offset] & 0xFF) {
            case OP_checkcast:
            case OP_instanceof:
            case OP_multianewarray:
            case OP_new:
                return resolveCodeClass(method, getCodeClassAt(method.getData(), code, offset + 1));
            case OP_newarray:
                return resolveCodeClass(method, JBCType.getBasicArrayClassName(code[offset + 1] & 0xFF));
            case OP_anewarray:
                return resolveCodeClass(method, JBCType.getObjectArrayClassName(
                        getCodeClassAt(method.getData(), code, offset + 1)));
            default:
                throw Globals.nonlocalError("Invalid instruction type for resolveInstructionClass in " + method + " at " + offset);
        }
    }
    
    public static JBCMethod resolveInstructionMethod(JBCMethod method, byte[] code, int offset) {
        MethodData data = method.getData();
        CodeRefData ref = getCodeRefAt(data, code, offset + 1);
        JBCClass containingClass = method.getContainingClass();
        JBCClass c = containingClass.getClassLoader().getClass(ref.getClassName());
           
        if (c == null) {
            Globals.writeLog(method, "WARNING: Class " + ref.getClassName() + " not found in " + method);
            return null;
        } else {
            JBCMethod m = new JBCMethod(c, ref.getSlotName(), ref.getSlotType());
            int opcode = code[offset] & 0xFF;
            JBCMethod originalMethod = m;
            
            while (!m.exists()) {
                c = c.getSuperClass();
                
                if (c == null) {
                    throw new InvalidClassDataError("No such method in call from " + method + ": " + originalMethod);
                } else {
                    m = new JBCMethod(c, ref.getSlotName(), ref.getSlotType());
                }
            }
            
            if (m.isStatic() != (opcode == OP_invokestatic)) {
                if (opcode == OP_invokestatic) {
                    throw new InvalidClassDataError("Expected static method in call from " + method + ": " + m);
                } else {
                    throw new InvalidClassDataError("Expected nonstatic method in call from " + method + ": " + m);
                }
            }
            
            if (!m.isAccessibleTo(containingClass)) {
                throw new InvalidClassDataError("Call to inaccessible "
                    + m + " from " + method);
            }
                    
            if (opcode == OP_invokespecial && isSuperMethod(method, m)) {
                m = method.getContainingClass().getSuperInheritedMethod(m);
            }

            return m;
        }
    }
    
    public static boolean useStaticDispatch(JBCMethod m) {
        int flags = m.getData().getAccessFlags();
        
        return (flags & (ACC_STATIC | ACC_PRIVATE | ACC_FINAL)) != 0
            || m.getMethodName().equals("<init>")
            || m.getContainingClass().isFinal();
    }
    
    private static boolean isSuperMethod(JBCMethod caller, JBCMethod m) {
        if (!m.getMethodName().equals("<init>") && !m.isPrivate()) {
            JBCClass methodClass = caller.getContainingClass();
        
            return methodClass.isSubclassOf(m.getContainingClass())
                && (methodClass.getData().getAccessFlags() & ACC_SUPER) != 0;
        } else {
            return false;
        }
    }
    
    public static JBCField resolveInstructionField(JBCMethod method, byte[] code, int offset) {
        MethodData data = method.getData();
        CodeRefData ref = JBCCodeUtilities.getCodeRefAt(data, code, offset + 1);
        JBCClass c = method.getContainingClass().getClassLoader()
            .getClass(ref.getClassName());
           
        if (c == null) {
            Globals.writeLog(method, "WARNING: Class " + ref.getClassName() + " not found in " + method);
            return null;
        } else {
            JBCField f = new JBCField(c, ref.getSlotName());
            
            try {
                if (!f.getFieldTypeName().equals(ref.getSlotType())) {
                    throw new InvalidClassDataError("Invalid type for field in " + method + ": " + f);
                }
            } catch (NonexistentFieldError ex) {
                Globals.writeLog(method, "WARNING: Field " + ref.getSlotName() + " not found in " + method);
                return null;
            }
            
            boolean needStatic;
            switch (code[offset] & 0xFF) {
                case OP_getstatic:
                case OP_putstatic:  needStatic = true; break;
                default:            needStatic = false;
            }
           
            if (f.isStatic() != needStatic) {
                if (needStatic) {
                    throw new InvalidClassDataError("Expected static field in " + method + ": " + f);
                } else {
                    throw new InvalidClassDataError("Expected nonstatic field " + method + ": " + f);
                }
            }
            
            if (!f.isAccessibleTo(method.getContainingClass())) {
                throw new InvalidClassDataError("Reference to inaccessible "
                    + f + " from " + method);
            }
                    
            return f;
        }
    }
    
/**
This function computes the change in the size of the stack when an instruction
executes normally. (Since athrow never executes normally but always throws
an exception, its stack delta is undefined.) Positive means that the stack grows.

Note that we need all the method information because some linkages may have to
be resolved (e.g. to get the field sizes for getstatic and getfield).

@param method the method whose code will be examined
@param index the offset within the code array of the start of the instruction
@return the change in stack size, in words.
*/
    public static int getStackSizeDelta(JBCMethod method, byte[] code, int index) {
        int opcode = code[index] & 0xFF;
        
        if (opcode == OP_wide) {
            opcode = code[index + 1] & 0xFF;
        }
        
        switch (opcode) {
            case OP_getstatic:
                return JBCType.getWordSize(getCodeRefAt(method.getData(), code, index + 1).getSlotType());
            case OP_putstatic:
                return -JBCType.getWordSize(getCodeRefAt(method.getData(), code, index + 1).getSlotType());
            case OP_getfield:
                return -1 + JBCType.getWordSize(getCodeRefAt(method.getData(), code, index + 1).getSlotType());
            case OP_putfield:
                return -1 - JBCType.getWordSize(getCodeRefAt(method.getData(), code, index + 1).getSlotType());
            case OP_invokevirtual:
            case OP_invokeinterface:
            case OP_invokespecial: {
                String type = getCodeRefAt(method.getData(), code, index + 1).getSlotType();
                
                return -1 - JBCType.getMethodParamsSize(type)
                    + JBCType.getMethodResultSize(type);
            }
            case OP_invokestatic: {
                String type = getCodeRefAt(method.getData(), code, index + 1).getSlotType();
                
                return -JBCType.getMethodParamsSize(type)
                    + JBCType.getMethodResultSize(type);
            }
            case OP_multianewarray:
                return 1 - (code[index + 3] & 0xFF);
            default:
                return opcodeStackSizeDeltas[opcode];
        }
    }

    public static int getStackPopCount(JBCMethod method, byte[] code, int offset) {
        return getStackPushCount(method, code, offset) - getStackSizeDelta(method, code, offset);
    }
    
/**
This function computes the number of words pushed onto the stack by an instruction.
This is the number of elements shown on the right-hand side of the
arrow in the JVM Spec's "Stack" description of each instruction. It basically
means the number of stack locations that are written if the instruction executes
normally. (I treat 'athrow' as never executing normally, so its push count is zero.)

Note that we need all the method information because some linkages may have to
be resolved (e.g. to get the field sizes for getstatic and getfield).

@param method the method whose code will be examined
@param index the offset within the code array of the start of the instruction
@return the number of words pushed onto the stack
*/
    public static int getStackPushCount(JBCMethod method, byte[] code, int index) {
        int opcode = code[index] & 0xFF;
        
        if (opcode == OP_wide) {
            opcode = code[index + 1] & 0xFF;
        }
        
        switch (opcode) {
            case OP_getstatic:
            case OP_getfield:
                return JBCType.getWordSize(getCodeRefAt(method.getData(), code, index + 1).getSlotType());
            case OP_invokevirtual:
            case OP_invokeinterface:
            case OP_invokespecial:
            case OP_invokestatic:
                return JBCType.getMethodResultSize(getCodeRefAt(method.getData(), code, index + 1).getSlotType());
            default:
                return opcodeStackPushes[opcode];
        }
    }

/**
@param code the code array for the method
@param index the offset within the code array of the start of the instruction
@return the length of the instruction, in bytes, or zero if the instruction is invalid
*/
    public static int computeOpLength(byte[] code, int index) {
        int opcode = code[index] & 0xFF;
        
        switch (opcode) {
            case OP_lookupswitch: {
                int newIndex = (index + 4) & ~0x3;
                return (newIndex - index) + 8 + getIntAt(code, newIndex + 4)*8;
            }
            case OP_tableswitch: {
                int newIndex = (index + 4) & ~0x03;
                return (newIndex - index) + 12 +
                    (getIntAt(code, newIndex + 8) - getIntAt(code, newIndex + 4) + 1)*4;
            }
            case OP_wide:
                if ((code[index + 1] & 0xFF) == OP_iinc) {
                    return 6;
                } else {
                    return 4;
                }
            default:
                if (0 <= opcode && opcode < opcodeLengths.length) {
                    return opcodeLengths[opcode];
                } else {
                    return 0;
                }
        }
    }

    private static int[] make1IntArray(int a, int[] storage) {
        if (storage == null) {
            storage = new int[1];
        } else if (Globals.debug && storage.length != 1) {
            Globals.nonlocalError("Invalid storage array length: " + storage.length);
        }
        
        storage[0] = a;
        return storage;
    }
    
    private static void fillInReachableInstructionStarts(byte[] code, int offset,
        int[] singleton, boolean[] result) {
        while (true) {
            if (!result[offset]) {
                int[] successors = getReachableSuccessors(code, offset, singleton);
            
                result[offset] = true;
            
                if (successors.length == 1) {
                    offset = successors[0];
                    continue;
                } else {
                    for (int i = 0; i < successors.length; i++) {
                        fillInReachableInstructionStarts(code, successors[i], singleton, result);
                    }
                }
            }
            
            return;
        }
    }
    
    public static boolean[] getInstructionStarts(MethodData data) {
        int[] singleton = new int[1];
        byte[] code = data.getCode();
        CatchBlockData[] catches = data.getCatchBlocks();
        boolean[] result = new boolean[code.length];
        
        fillInReachableInstructionStarts(code, 0, singleton, result);
        
        for (int i = 0; i < catches.length; i++) {
            fillInReachableInstructionStarts(code, catches[i].getHandlerPC(), singleton, result);
        }
        
        return result;
    }
    
/**
This method computes the offsets of the "reachable" successors of an instruction.
The reachable successors are the instructions that this instruction may cause to
be executed if it terminates normally. For example, "athrow" has no reachable
successors because it never terminates normally. "jsr" has two reachable
successors because it can cause its return address (the next instruction) to
be invoked, as well as the first instruction of the called subroutine. "ret"
and "wide ret" have no reachable successors.

@param code the code array for the method
@param index the offset within the code array of the start of the instruction
*/
    public static int[] getReachableSuccessors(byte[] code, int index) {
        return getReachableSuccessors(code, index, null);
    }
    
/**
This method computes the offsets of the "reachable" successors of an instruction.
The reachable successors are the instructions that this instruction may cause to
be executed if it terminates normally. For example, "athrow" has no reachable
successors because it never terminates normally. "jsr" has two reachable
successors because it can cause its return address (the next instruction) to
be invoked, as well as the first instruction of the called subroutine. "ret"
and "wide ret" have no reachable successors.

If one of the successors is the instruction after the given instruction, then
we always put it first in the returned array.

@param code the code array for the method
@param index the offset within the code array of the start of the instruction
@param storage if non-null, must be an array of length 1; if the result would have
length 1, then this array is filled in and returned instead of allocating a new one
*/
    public static int[] getReachableSuccessors(byte[] code, int index, int[] storage) {
        switch (code[index] & 0xFF) {
            case OP_jsr:
            case OP_ifeq:
            case OP_ifne:
            case OP_iflt:
            case OP_ifge:
            case OP_ifgt:
            case OP_ifle:
            case OP_if_icmpeq:
            case OP_if_icmpne:
            case OP_if_icmplt:
            case OP_if_icmpge:
            case OP_if_icmpgt:
            case OP_if_icmple:
            case OP_if_acmpeq:
            case OP_if_acmpne:
            case OP_ifnull:
            case OP_ifnonnull: {
                int[] result = { index + 3, index + JBCCodeUtilities.getShortAt(code, index + 1) };
                
                return result;
            }
            case OP_goto:
                return make1IntArray(index + JBCCodeUtilities.getShortAt(code, index + 1), storage);
            case OP_goto_w:
                return make1IntArray(index + JBCCodeUtilities.getIntAt(code, index + 1), storage);
            case OP_jsr_w: {
                int[] result = { index + 5, index + JBCCodeUtilities.getIntAt(code, index + 1) };
                
                return result;
            }
            case OP_ret:
            case OP_ireturn:
            case OP_lreturn:
            case OP_dreturn:
            case OP_freturn:
            case OP_areturn:
            case OP_return:
            case OP_athrow:
                return new int[0];
            case OP_tableswitch: {
                int newIndex = (index + 4) & ~0x3;
                int numOffsets = JBCCodeUtilities.getIntAt(code, newIndex + 8)
                    - JBCCodeUtilities.getIntAt(code, newIndex + 4) + 1;
                int[] result = new int[numOffsets + 1];
                int nextIndex = -1;
                int nextInstruction = newIndex + 12 + numOffsets*4;
                    
                for (int i = 0; i < numOffsets; i++) {
                    int successor = index + JBCCodeUtilities.getIntAt(code, newIndex + 12 + i*4);
                    
                    result[i] = successor;
                    if (successor == nextInstruction) {
                        nextIndex = i;
                    }
                }
                
                int successor = index + JBCCodeUtilities.getIntAt(code, newIndex);
                
                result[numOffsets] = successor;
                if (successor == nextInstruction) {
                    nextIndex = numOffsets;
                }
                
                if (nextIndex >= 0) {
                    int tmp = result[0];
                    
                    result[0] = result[nextIndex];
                    result[nextIndex] = tmp;
                }
                
                return result;
            }
            case OP_lookupswitch: {
                int newIndex = (index + 4) & ~0x3;
                int numOffsets = JBCCodeUtilities.getIntAt(code, newIndex + 4);
                int[] result = new int[numOffsets + 1];
                int nextIndex = -1;
                int nextInstruction = newIndex + 12 + numOffsets*8;
                
                for (int i = 0; i < numOffsets; i++) {
                    int successor = index + JBCCodeUtilities.getIntAt(code, newIndex + 12 + i*8);
                    
                    result[i] = successor;
                    if (successor == nextInstruction) {
                        nextIndex = i;
                    }
                }
                
                int successor = index + JBCCodeUtilities.getIntAt(code, newIndex);
                
                result[numOffsets] = successor;
                if (successor == nextInstruction) {
                    nextIndex = numOffsets;
                }
                
                if (nextIndex >= 0) {
                    int tmp = result[0];
                    
                    result[0] = result[nextIndex];
                    result[nextIndex] = tmp;
                }
                
                return result;
            }
            case OP_wide:
                if ((code[index + 1] & 0xFF) == OP_ret) {
                    return new int[0];
                } else {
                    return make1IntArray(index + JBCCodeUtilities.computeOpLength(code, index), storage);
                } 
            default: {
                int opLength = JBCCodeUtilities.computeOpLength(code, index);
                
                if (opLength <= 0) {
                    throw new InvalidClassDataError("Invalid opcode: " + (code[index] & 0xFF));
                } else {
                    return make1IntArray(index + opLength, storage);
                }
            }
        }
    }
    
/**
This function determines whether an instruction can throw an exception of a given class,
or a exception that's a subclass of the given class. The result is conservative
(it never returns false when the answer should be true), but it is not precise.
For example, athrow and the invoke* instructions are treated as being able to throw
any exception. Also, an instruction that can cause a subclass of LinkageError
to be thrown is treated as being able to throw LinkageError and any of its subclasses.
(This wouldn't be hard to make precise, though.)

This function is useful for determining whether an instruction might transfer control
to a catch block.

@param code the code array for the method
@param index the offset of the instruction
@param c we check for this class and its subclasses; if null, then we check for all
exception classes (and the result is 'true', since any instruction can throw a
VirtualMachineError)
*/
    public static boolean isExceptionSource(byte[] code, int index, JBCClass c) {
        if (c == null) {
            return true;
        } else {
            JBCWorld world = c.getClassLoader().getWorld();
            
            if (c.hasCommonSubclassWith(world.getSpecialClass("java.lang.VirtualMachineError"))
                || c.hasCommonSubclassWith(world.getSpecialClass("java.lang.ThreadDeath"))) {
                return true;
            } else {
                int opcode = code[index] & 0xFF;
                
                if (opcode == OP_wide) {
                    opcode = code[index + 1] & 0xFF;
                }
                
                String[] exceptionList = opcodeExceptionLists[opcodeExceptionListNum[opcode]];
                
                for (int i = 0; i < exceptionList.length; i++) {
                    if (c.hasCommonSubclassWith(world.getSpecialClass(exceptionList[i]))) {
                        return true;
                    }
                }
                
                return false;
            }
        }
    }
    
/**
Compute the list of exceptions that may be thrown by an instruction. This
list is only approximate, as described in 'isExceptionSource'. We return a list
of the class names of the exceptions; any subclass of any of the returned
classes may also be thrown.

@param code the code array of the method
@param index the offset of the instruction
@return an array of the class names
*/
    public static String[] getExceptionList(byte[] code, int index) {
        int opcode = code[index] & 0xFF;
        
        if (opcode == OP_wide) {
            opcode = code[index + 1] & 0xFF;
        }
        
        return opcodeExceptionLists[opcodeExceptionListNum[opcode]];
    }
 
/**
@param code the code array for the method
@param index the offset within the code array of the start of the instruction
@return the index of the local variable stored into by the instruction, or
Integer.MIN_VALUE if this instruction does not store into any local variable
*/
    public static int getLocalVariableStoreIndex(byte[] code, int offset) {
        int opcode = code[offset] & 0xFF;
        
        switch (opcode) {
            case OP_astore_0:
            case OP_istore_0:
            case OP_fstore_0:
            case OP_dstore_0:
            case OP_lstore_0:
                return 0;
            case OP_astore_1:
            case OP_istore_1:
            case OP_fstore_1:
            case OP_dstore_1:
            case OP_lstore_1:
                return 1;
            case OP_astore_2:
            case OP_istore_2:
            case OP_fstore_2:
            case OP_dstore_2:
            case OP_lstore_2:
                return 2;
            case OP_astore_3:
            case OP_istore_3:
            case OP_fstore_3:
            case OP_dstore_3:
            case OP_lstore_3:
                return 3;
            case OP_istore:
            case OP_astore:
            case OP_lstore:
            case OP_fstore:
            case OP_dstore:
            case OP_iinc:
                return code[offset + 1] & 0xFF;
            case OP_wide:
                switch (code[offset + 1] & 0xFF) {
                    case OP_istore:
                    case OP_fstore:
                    case OP_astore:
                    case OP_dstore:
                    case OP_lstore:
                    case OP_iinc:
                        return getUnsignedShortAt(code, offset + 2);
                    default:
                        return Integer.MIN_VALUE;
                }
            default:
                return Integer.MIN_VALUE;
        }
    }
    
    public static boolean isLocalVariableLoaded(byte[] code, int offset, int index) {
        int loadIndex = getLocalVariableLoadIndex(code, offset);

        if (loadIndex >= 0) {
            int loadCount = getLocalVariableLoadCount(code, offset);
            
            return loadIndex <= index && index < loadIndex + loadCount;
        } else {
            return false;
        }
    }

    public static boolean isLocalVariableStored(byte[] code, int offset, int index) {
        int storeIndex = getLocalVariableStoreIndex(code, offset);

        if (storeIndex >= 0) {
            int storeCount = getLocalVariableStoreCount(code, offset);
            
            return storeIndex <= index && index < storeIndex + storeCount;
        } else {
            return false;
        }
    }
    
    public static Object getLoadedConstant(JBCMethod method, byte[] code, int offset) {
        switch (code[offset] & 0xFF) {
            case OP_ldc:
                return method.getData().getCodeConstant(code[offset + 1] & 0xFF);
            case OP_ldc2_w:
            case OP_ldc_w:
                return method.getData().getCodeConstant(
                        JBCCodeUtilities.getUnsignedShortAt(code, offset + 1));
            default:
                return null;
        }
    }

/**
@param code the code array for the method
@param index the offset within the code array of the start of the instruction
@return the number of local variable slots stored into by this instruction
(or zero if this instruction does not store into any local variable)
*/
    public static int getLocalVariableStoreCount(byte[] code, int offset) {
        int opcode = code[offset] & 0xFF;
        
        if (opcode == OP_wide) {
            opcode = code[offset + 1] & 0xFF;
        }
        
        switch (opcode) {
            case OP_astore_0:
            case OP_istore_0:
            case OP_fstore_0:
            case OP_astore_1:
            case OP_istore_1:
            case OP_fstore_1:
            case OP_astore_2:
            case OP_istore_2:
            case OP_fstore_2:
            case OP_astore_3:
            case OP_istore_3:
            case OP_fstore_3:
            case OP_astore:
            case OP_istore:
            case OP_fstore:
            case OP_iinc:
                return 1;
            case OP_dstore_0:
            case OP_lstore_0:
            case OP_dstore_1:
            case OP_lstore_1:
            case OP_dstore_2:
            case OP_lstore_2:
            case OP_dstore_3:
            case OP_lstore_3:
            case OP_dstore:
            case OP_lstore:
                return 2;
            default:
                return 0;
        }
    }
    
/**
@param code the code array for the method
@param index the offset within the code array of the start of the instruction
@return the index of the local variable loaded by the instruction, or
Integer.MIN_VALUE if this instruction does not load any local variable
*/
    public static int getLocalVariableLoadIndex(byte[] code, int offset) {
        int opcode = code[offset] & 0xFF;
        
        switch (opcode) {
            case OP_aload_0:
            case OP_iload_0:
            case OP_fload_0:
            case OP_dload_0:
            case OP_lload_0:
                return 0;
            case OP_aload_1:
            case OP_iload_1:
            case OP_fload_1:
            case OP_dload_1:
            case OP_lload_1:
                return 1;
            case OP_aload_2:
            case OP_iload_2:
            case OP_fload_2:
            case OP_dload_2:
            case OP_lload_2:
                return 2;
            case OP_aload_3:
            case OP_iload_3:
            case OP_fload_3:
            case OP_dload_3:
            case OP_lload_3:
                return 3;
            case OP_iload:
            case OP_aload:
            case OP_lload:
            case OP_fload:
            case OP_dload:
            case OP_iinc:
            case OP_ret:
                return code[offset + 1] & 0xFF;
            case OP_wide:
                switch (code[offset + 1] & 0xFF) {
                    case OP_iload:
                    case OP_fload:
                    case OP_aload:
                    case OP_dload:
                    case OP_lload:
                    case OP_iinc:
                    case OP_ret:
                        return getUnsignedShortAt(code, offset + 2);
                    default:
                        return Integer.MIN_VALUE;
                }
            default:
                return Integer.MIN_VALUE;
        }
    }
    
/**
@param code the code array for the method
@param index the offset within the code array of the start of the instruction
@return the number of local variable slots loaded by this instruction
(or zero if this instruction does not load any local variable)
*/
   public static int getLocalVariableLoadCount(byte[] code, int offset) {
        int opcode = code[offset] & 0xFF;
        
        if (opcode == OP_wide) {
            opcode = code[offset + 1] & 0xFF;
        }
        
        switch (opcode) {
            case OP_aload_0:
            case OP_iload_0:
            case OP_fload_0:
            case OP_aload_1:
            case OP_iload_1:
            case OP_fload_1:
            case OP_aload_2:
            case OP_iload_2:
            case OP_fload_2:
            case OP_aload_3:
            case OP_iload_3:
            case OP_fload_3:
            case OP_aload:
            case OP_iload:
            case OP_fload:
            case OP_iinc:
                return 1;
            case OP_dload_0:
            case OP_lload_0:
            case OP_dload_1:
            case OP_lload_1:
            case OP_dload_2:
            case OP_lload_2:
            case OP_dload_3:
            case OP_lload_3:
            case OP_dload:
            case OP_lload:
                return 2;
            default:
                return 0;
        }
    }
    
    public static boolean isBranchTaken(int opcode, int operand) {
        switch (opcode) {
            case OP_ifne: return operand != 0;
            case OP_iflt: return operand < 0;
            case OP_ifgt: return operand > 0;
            case OP_ifeq: return operand == 0;
            case OP_ifge: return operand >= 0;
            case OP_ifle: return operand <= 0;
            default:
                throw Globals.localError("Unknown opcode for isBranchTaken: " + opcode);
        }
    }
    
    public static String[] getLocalVariableNames(JBCMethod method, int offset) {
        MethodData data = method.getData();
        LocalVariableData[] locals = data.getLocalVariables();
        int numLocals = data.getMaxLocalWords();
        String[] varNames = new String[numLocals];
        
        for (int i = 0; i < numLocals; i++) {
            varNames[i] = "local-" + i;
        }
        
        // locals will be null if there is no debug info
        if (locals != null) {
            for (int i = 0; i < locals.length; i++) {
                LocalVariableData varData = locals[i];
                int start = varData.getScopeStartPC();
                
                if (start <= offset && offset < start + varData.getScopeLength()) {
                    varNames[varData.getVarIndex()] = varData.getVarName();
                }
            }
        }
        
        return varNames;
    }
}
