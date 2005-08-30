package daikon.dcomp;

import java.util.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;
import org.apache.bcel.generic.FieldOrMethod;
import org.apache.bcel.util.*;

//map from handle -> Stack
//for each handle...
//  look at get targeters...
//  use any targeter as the "parent" stack
//  copy stack, modify it, add to map
public final class TypeStack
{
    private ConstantPoolGen pool;
    private final Map<InstructionHandle, Stack<Type>> stackMap = new HashMap<InstructionHandle, Stack<Type>>();
    private final Map<InstructionHandle, InstructionHandle> parentMap = new HashMap<InstructionHandle, InstructionHandle>();
    private Stack<Type> stack = null;
    
    private final Type[] argTypes;
    private final Type retType;

    public TypeStack(ClassGen gen, InstructionList l,
            final CodeExceptionGen[] exceptionTable,
            final Type[] argT, final Type retT)
    {
        retType = retT;
        argTypes = argT;
        pool = gen.getConstantPool();
        createMap(l, exceptionTable);
    }

    public TypeStack(ConstantPool p, InstructionList l,
            final CodeExceptionGen[] exceptionTable,
            final Type[] argT, final Type retT)
    {
        retType = retT;
        argTypes = argT;
        pool = new ConstantPoolGen(p);
        createMap(l, exceptionTable);
    }
    
    private Stack<Type> startMethStack()
    {
        Stack<Type> type = new Stack<Type> ();
        
        for(Type t: argTypes)
        {
            type.push(t);
        }
        
        return type;
    }

    private void createMap(final InstructionList l,
            final CodeExceptionGen[] exceptionTable)
    {
        if (!initParents(l.getStart(), l.getInstructionHandles(),
                exceptionTable))
            throw new IllegalStateException("No valid parent map possible for this method");
        
        //dumpMap(parentMap);
        
        for (InstructionHandle hand : l.getInstructionHandles())
        {
            initStack(hand);
        }
    }

    private boolean initParents(final InstructionHandle hand,
            final InstructionHandle[] allInst,
            final CodeExceptionGen[] exceptionTable)
    {
        if (hand == null)
            return true;
        
        InstructionHandle prev = hand.getPrev();
        Set<InstructionHandle> targeters = new HashSet<InstructionHandle>();

        // if first instruction, or previous instruction was not a "goto" or a throw
        if (prev == null || 
                !(prev.getInstruction() instanceof GotoInstruction 
                 ))// || prev.getInstruction() instanceof ATHROW))
        {
            // hand's parent is prev
            targeters.add(prev);
        }

        // look for branching instructings which target hand
        for (InstructionHandle nextHand : allInst)
        {
            if (nextHand.getInstruction() instanceof BranchInstruction)
            {
                BranchInstruction i = (BranchInstruction) nextHand
                        .getInstruction();
                if (i.getTarget().equals(hand))
                {
                    targeters.add(nextHand);
                }
            }
        }
        
        // look for an exception target
        for (CodeExceptionGen ex : exceptionTable)
        {
            if (ex.getHandlerPC().equals(hand))
            {
                return initParents(hand.getNext(), allInst, exceptionTable);
            }
        }

        for (InstructionHandle h : targeters)
        {
            //System.out.printf("%s targets%s(***%d***)%n", h, hand, targeters.size());

            parentMap.put(hand, h);
            if (!inChain(hand))
            {
                // if everything else works out, we're good
                if (initParents(hand.getNext(), allInst, exceptionTable))
                {
                    //System.out.println("TRUE FOR " + hand);
                    return true;
                }
                // otherwise, remove this entry and try next targeter...
                else
                {
                    parentMap.remove(hand);
                }
            }
            // this caused a loop, remove entry and try again
            else
            {
                parentMap.remove(hand);
            }
        }
        //System.out.println("FALSE FOR " + hand);

        // we couldn't find anything that worked
        return false;
    }

    private boolean inChain(final InstructionHandle h)
    {
        return inChainHelper(parentMap.get(h), h);
    }

    private boolean inChainHelper(final InstructionHandle h1,
            final InstructionHandle h2)
    {
        if (h2 == null)
            return false;
        
        if (h1 == null)
        {
            return false;
        }
        else if (h1.equals(h2))
        {
            return true;
        }
        else
            return inChainHelper(parentMap.get(h1), h2);
    }

    private void initStack(InstructionHandle hand)
    {
        Instruction inst = hand.getInstruction();
        assert inst != null;
        
        InstructionHandle parent = parentMap.get(hand);
        Stack<Type> parentStack;
        
        if (parent != null)
        {
            parentStack = stackMap.get(parent);
            if (parentStack == null)
            {
                //System.out.printf("handle: %s, parent: %s%n", hand, parent);
                
                initStack(parent);
                parentStack = stackMap.get(parent);
                assert parentStack != null : "Could not initialize parent stack!!!";
            }
        }
        else
        {
            if (hand.getPosition() == 0)
            {
                //init stack with just the arguments
                parentStack = startMethStack();
            }
            else
            {
                // must be exception
                parentStack = new Stack<Type>();
                parentStack.push(Type.OBJECT);
            }
        }
        
        // update the current stack
        stack = copyOfStack(parentStack);
        // update the map!
        stackMap.put(hand, stack);
        
        //System.out.printf("Processing: %s, parent: %s%n", hand, parent);
        //System.out.println("parent stack is " + parentStack);
        //System.out.println("Stack (before) is " + stack);
        
        if (inst instanceof ACONST_NULL)
        {
            stack.push(Type.NULL);
        }
        else if (inst instanceof ArithmeticInstruction)
        {
            handleMath((ArithmeticInstruction) inst);
        }
        else if (inst instanceof ArrayInstruction)
        {
            handleArray((ArrayInstruction) inst);
        }
        else if (inst instanceof ARRAYLENGTH)
        {
            popNumPut(1, Type.INT);
        }
        else if (inst instanceof ATHROW)
        {
            stack.clear();
            
            if(retType != Type.VOID)
                stack.push(retType);
        }
        else if (inst instanceof BIPUSH)
        {
            stack.push(Type.INT);
        }
        else if (inst instanceof BranchInstruction)
        {
            handleBranch((BranchInstruction) inst);
        }
        else if (inst instanceof BREAKPOINT)
        {
            notSupported(inst);
        }
        else if (inst instanceof ConversionInstruction)
        {
            handleConv((ConversionInstruction) inst);
        }
        else if (inst instanceof CPInstruction)
        {
            handleCP((CPInstruction) inst);
        }
        else if (inst instanceof DCMPG)
        {
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof DCMPL)
        {
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof DCONST)
        {
            stack.push(Type.DOUBLE);
        }
        else if (inst instanceof FCMPG)
        {
            popNumPut(2, Type.DOUBLE);
        }
        else if (inst instanceof FCMPL)
        {
            popNumPut(2, Type.DOUBLE);
        }
        else if (inst instanceof FCONST)
        {
            stack.push(Type.FLOAT);
        }
        else if (inst instanceof ICONST)
        {
            stack.push(Type.INT);
        }
        else if (inst instanceof IMPDEP1)
        {
            notSupported(inst);
        }
        else if (inst instanceof IMPDEP2)
        {
            notSupported(inst);
        }
        else if (inst instanceof LCMP)
        {
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof LCONST)
        {
            stack.push(Type.LONG);
        }
        else if (inst instanceof LocalVariableInstruction)
        {
            handleLocal((LocalVariableInstruction) inst);
        }
        else if (inst instanceof MONITORENTER)
        {
            stack.pop(); // obj ref
        }
        else if (inst instanceof MONITOREXIT)
        {
            stack.pop(); // obj ref
        }
        else if (inst instanceof NEWARRAY)
        {
            popNumPut(1, Type.OBJECT);
        }
        else if (inst instanceof RET)
        {
            // NO CHANGE
        }
        else if (inst instanceof ReturnInstruction)
        {
            handleReturn((ReturnInstruction) inst);
        }
        else if (inst instanceof SIPUSH)
        {
            stack.push(Type.INT);
        }
        else if (inst instanceof StackInstruction)
        {
            handleStack((StackInstruction) inst);
        }
        else
        {
            throw new RuntimeException("Unknown instruction type: " + inst);
        }
        
        //System.out.println("Stack (after) is " + stack);
    }

    private static <T> Stack<T> copyOfStack(Stack<T> parent)
    {
        Stack<T> copy = new Stack<T>();
        for (T el : parent)
            copy.push(el);
        return copy;
    }
    
    private <K, V> void dumpMap(Map<K, V> map)
    {
        for(K k: map.keySet())
        {
            System.out.printf("key: %s, value: %s%n", k, map.get(k));
        }
    }

    private void handleMath(ArithmeticInstruction inst)
    {
        if (inst instanceof DADD)
        {
            assertStack(Type.DOUBLE, Type.DOUBLE);
            popNumPut(2, Type.DOUBLE);
        }
        else if (inst instanceof DDIV)
        {
            assertStack(Type.DOUBLE, Type.DOUBLE);
            popNumPut(2, Type.DOUBLE);
        }
        else if (inst instanceof DMUL)
        {
            assertStack(Type.DOUBLE, Type.DOUBLE);
            popNumPut(2, Type.DOUBLE);
        }
        else if (inst instanceof DNEG)
        {
            assertStack(Type.DOUBLE);
            popNumPut(1, Type.DOUBLE);
        }
        else if (inst instanceof DREM)
        {
            assertStack(Type.DOUBLE, Type.DOUBLE);
            popNumPut(2, Type.DOUBLE);
        }
        else if (inst instanceof DSUB)
        {
            assertStack(Type.DOUBLE, Type.DOUBLE);
            popNumPut(2, Type.DOUBLE);
        }
        else if (inst instanceof FADD)
        {
            assertStack(Type.FLOAT, Type.FLOAT);
            popNumPut(2, Type.FLOAT);
        }
        else if (inst instanceof FDIV)
        {
            assertStack(Type.FLOAT, Type.FLOAT);
            popNumPut(2, Type.FLOAT);
        }
        else if (inst instanceof FMUL)
        {
            assertStack(Type.FLOAT, Type.FLOAT);
            popNumPut(2, Type.FLOAT);
        }
        else if (inst instanceof FNEG)
        {
            assertStack(Type.FLOAT);
            popNumPut(1, Type.FLOAT);
        }
        else if (inst instanceof FREM)
        {
            assertStack(Type.FLOAT, Type.FLOAT);
            popNumPut(2, Type.FLOAT);
        }
        else if (inst instanceof FSUB)
        {
            assertStack(Type.FLOAT, Type.FLOAT);
            popNumPut(2, Type.FLOAT);
        }
        else if (inst instanceof IADD)
        {
            assertStack(Type.INT, Type.INT);
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof IAND)
        {
            assertStack(Type.INT, Type.INT);
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof IDIV)
        {
            assertStack(Type.INT, Type.INT);
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof IMUL)
        {
            assertStack(Type.INT, Type.INT);
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof INEG)
        {
            assertStack(Type.INT);
            popNumPut(1, Type.INT);
        }
        else if (inst instanceof IOR)
        {
            assertStack(Type.INT, Type.INT);
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof IREM)
        {
            assertStack(Type.INT, Type.INT);
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof ISHL)
        {
            assertStack(Type.INT, Type.INT);
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof ISHR)
        {
            assertStack(Type.INT, Type.INT);
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof ISUB)
        {
            assertStack(Type.INT, Type.INT);
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof IUSHR)
        {
            assertStack(Type.INT, Type.INT);
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof IXOR)
        {
            assertStack(Type.INT, Type.INT);
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof LADD)
        {
            assertStack(Type.LONG, Type.LONG);
            popNumPut(2, Type.LONG);
        }
        else if (inst instanceof LAND)
        {
            assertStack(Type.LONG, Type.LONG);
            popNumPut(2, Type.LONG);
        }
        else if (inst instanceof LDIV)
        {
            assertStack(Type.LONG, Type.LONG);
            popNumPut(2, Type.LONG);
        }
        else if (inst instanceof LMUL)
        {
            assertStack(Type.LONG, Type.LONG);
            popNumPut(2, Type.LONG);
        }
        else if (inst instanceof LNEG)
        {
            assertStack(Type.LONG);
            popNumPut(1, Type.LONG);
        }
        else if (inst instanceof LOR)
        {
            assertStack(Type.LONG, Type.LONG);
            popNumPut(2, Type.LONG);
        }
        else if (inst instanceof LREM)
        {
            assertStack(Type.LONG, Type.LONG);
            popNumPut(2, Type.LONG);
        }
        else if (inst instanceof LSHL)
        {
            assertStack(Type.LONG, Type.LONG);
            popNumPut(2, Type.LONG);
        }
        else if (inst instanceof LSHR)
        {
            assertStack(Type.LONG, Type.LONG);
            popNumPut(2, Type.LONG);
        }
        else if (inst instanceof LSUB)
        {
            assertStack(Type.LONG, Type.LONG);
            popNumPut(2, Type.LONG);
        }
        else if (inst instanceof LUSHR)
        {
            assertStack(Type.LONG, Type.LONG);
            popNumPut(2, Type.LONG);
        }
        else if (inst instanceof LXOR)
        {
            assertStack(Type.LONG, Type.LONG);
            popNumPut(2, Type.LONG);
        }
        else
        {
            throw new RuntimeException("Unknown arithmetic instruction type: "
                    + inst);
        }
    }

    private void handleArray(ArrayInstruction inst)
    {
        if (inst instanceof AALOAD)
        {
            popNumPut(2, Type.OBJECT);
        }
        else if (inst instanceof AASTORE)
        {
            popNum(3);
        }
        else if (inst instanceof BALOAD)
        {
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof BASTORE)
        {
            popNum(3);
        }
        else if (inst instanceof CALOAD)
        {
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof CASTORE)
        {
            popNum(3);
        }
        else if (inst instanceof DALOAD)
        {
            popNumPut(2, Type.DOUBLE);
        }
        else if (inst instanceof DASTORE)
        {
            popNum(3);
        }
        else if (inst instanceof FALOAD)
        {
            popNumPut(2, Type.FLOAT);
        }
        else if (inst instanceof FASTORE)
        {
            popNum(3);
        }
        else if (inst instanceof IALOAD)
        {
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof IASTORE)
        {
            popNum(3);
        }
        else if (inst instanceof LALOAD)
        {
            popNumPut(2, Type.LONG);
        }
        else if (inst instanceof LASTORE)
        {
            popNum(3);
        }
        else if (inst instanceof SALOAD)
        {
            popNumPut(2, Type.INT);
        }
        else if (inst instanceof SASTORE)
        {
            popNum(3);
        }
        else
        {
            throw new RuntimeException("Unknown array instruction type: "
                    + inst);
        }
    }

    private void handleBranch(BranchInstruction inst)
    {
        if (inst instanceof GotoInstruction)
        {
            if (inst instanceof GOTO)
            {
                // NO CHANGE
            }
            else if (inst instanceof GOTO_W)
            {
                // NO CHANGE
            }
            else
            {
                throw new RuntimeException("Unknown goto instruction type: "
                        + inst);
            }
        }
        else if (inst instanceof IfInstruction)
        {
            if (inst instanceof IF_ACMPEQ)
            {
                popNum(2);
            }
            else if (inst instanceof IF_ACMPNE)
            {
                popNum(2);
            }
            else if (inst instanceof IF_ICMPEQ)
            {
                popNum(2);
            }
            else if (inst instanceof IF_ICMPGE)
            {
                popNum(2);
            }
            else if (inst instanceof IF_ICMPGT)
            {
                popNum(2);
            }
            else if (inst instanceof IF_ICMPLE)
            {
                popNum(2);
            }
            else if (inst instanceof IF_ICMPLT)
            {
                popNum(2);
            }
            else if (inst instanceof IF_ICMPNE)
            {
                popNum(2);
            }
            else if (inst instanceof IFEQ)
            {
                stack.pop();
            }
            else if (inst instanceof IFGE)
            {
                stack.pop();
            }
            else if (inst instanceof IFGT)
            {
                stack.pop();
            }
            else if (inst instanceof IFLE)
            {
                stack.pop();
            }
            else if (inst instanceof IFLT)
            {
                stack.pop();
            }
            else if (inst instanceof IFNE)
            {
                stack.pop();
            }
            else if (inst instanceof IFNONNULL)
            {
                stack.pop();
            }
            else if (inst instanceof IFNULL)
            {
                stack.pop();
            }
            else
            {
                throw new RuntimeException("Unknown if instruction type: "
                        + inst);
            }
        }
        else if (inst instanceof JsrInstruction)
        {
            // TODO Jsr
            notSupported(inst);
        }
        else if (inst instanceof Select)
        {
            if (inst instanceof LOOKUPSWITCH)
            {
                stack.pop();
            }
            else if (inst instanceof TABLESWITCH)
            {
                stack.pop();
            }
            else
            {
                throw new RuntimeException("Unknown select instruction type: "
                        + inst);
            }
        }
        else
        {
            throw new RuntimeException("Unknown branching instruction type: "
                    + inst);
        }
    }

    private void handleConv(ConversionInstruction inst)
    {
        if (inst instanceof D2F)
        {
            popNumPut(1, Type.FLOAT);
        }
        else if (inst instanceof D2I)
        {
            popNumPut(1, Type.INT);
        }
        else if (inst instanceof D2L)
        {
            popNumPut(1, Type.LONG);
        }
        else if (inst instanceof F2D)
        {
            popNumPut(1, Type.DOUBLE);
        }
        else if (inst instanceof F2I)
        {
            popNumPut(1, Type.INT);
        }
        else if (inst instanceof F2L)
        {
            popNumPut(1, Type.LONG);
        }
        else if (inst instanceof I2B)
        {
            popNumPut(1, Type.INT);
        }
        else if (inst instanceof I2C)
        {
            popNumPut(1, Type.INT);
        }
        else if (inst instanceof I2D)
        {
            popNumPut(1, Type.DOUBLE);
        }
        else if (inst instanceof I2F)
        {
            popNumPut(1, Type.FLOAT);
        }
        else if (inst instanceof I2L)
        {
            popNumPut(1, Type.LONG);
        }
        else if (inst instanceof I2S)
        {
            popNumPut(1, Type.INT);
        }
        else if (inst instanceof L2D)
        {
            popNumPut(1, Type.DOUBLE);
        }
        else if (inst instanceof L2F)
        {
            popNumPut(1, Type.FLOAT);
        }
        else if (inst instanceof L2I)
        {
            popNumPut(1, Type.INT);
        }
        else
        {
            throw new RuntimeException("Unknown conversion instruction type: "
                    + inst);
        }
    }

    private void handleCP(CPInstruction inst)
    {
        if (inst instanceof ANEWARRAY)
        {
            popNumPut(1, Type.OBJECT);
        }
        else if (inst instanceof CHECKCAST)
        {
            // NO CHANGE
        }
        else if (inst instanceof FieldOrMethod)
        {
            if (inst instanceof GETFIELD)
            {
                FieldInstruction f = (FieldInstruction) inst;
                popNumPut(1, f.getFieldType(pool));
            }
            else if (inst instanceof GETSTATIC)
            {
                FieldInstruction f = (FieldInstruction) inst;
                stack.push(f.getFieldType(pool));
            }
            else if (inst instanceof PUTFIELD)
            {
                popNum(2);
            }
            else if (inst instanceof PUTSTATIC)
            {
                stack.pop();
            }
            else if (inst instanceof INVOKEINTERFACE)
            {
                InvokeInstruction inv = (InvokeInstruction) inst;
                popNum(inv.getArgumentTypes(pool).length + 1);
                Type ret = inv.getReturnType(pool);
                if (ret != Type.VOID)
                    stack.push(ret);
            }
            else if (inst instanceof INVOKESPECIAL)
            {
                InvokeInstruction inv = (InvokeInstruction) inst;
                popNum(inv.getArgumentTypes(pool).length + 1);
                Type ret = inv.getReturnType(pool);
                if (ret != Type.VOID)
                    stack.push(ret);
            }
            else if (inst instanceof INVOKESTATIC)
            {
                InvokeInstruction inv = (InvokeInstruction) inst;
                popNum(inv.getArgumentTypes(pool).length);
                Type ret = inv.getReturnType(pool);
                if (ret != Type.VOID)
                    stack.push(ret);
            }
            else if (inst instanceof INVOKEVIRTUAL)
            {
                InvokeInstruction inv = (InvokeInstruction) inst;
                popNum(inv.getArgumentTypes(pool).length + 1);
                Type ret = inv.getReturnType(pool);
                if (ret != Type.VOID)
                    stack.push(ret);
            }
            else
            {
                throw new RuntimeException(
                        "Unknown field or method instruction type: " + inst);
            }
        }
        else if (inst instanceof INSTANCEOF)
        {
            popNumPut(1, Type.INT);
        }
        else if (inst instanceof LDC)
        {
            LDC ldc = (LDC) inst;
            stack.push(ldc.getType(pool));
        }
        else if (inst instanceof LDC2_W)
        {
            LDC2_W ldc = (LDC2_W) inst;
            stack.push(ldc.getType(pool));
        }
        else if (inst instanceof MULTIANEWARRAY)
        {
            MULTIANEWARRAY multi = (MULTIANEWARRAY) inst;
            // pop counts, push arraryRef...
            popNumPut(multi.getDimensions(), Type.OBJECT);
        }
        else if (inst instanceof NEW)
        {
            stack.push(Type.OBJECT);
        }
        else
        {
            throw new RuntimeException("Unknown CP instruction type: " + inst);
        }
    }

    private void handleLocal(LocalVariableInstruction inst)
    {
        if (inst instanceof IINC)
        {
            // NO CHANGE
        }
        else if (inst instanceof LoadInstruction)
        {
            if (inst instanceof ALOAD)
            {
                stack.push(Type.OBJECT);
            }
            else if (inst instanceof DLOAD)
            {
                stack.push(Type.DOUBLE);
            }
            else if (inst instanceof FLOAD)
            {
                stack.push(Type.FLOAT);
            }
            else if (inst instanceof ILOAD)
            {
                stack.push(Type.INT);
            }
            else if (inst instanceof LLOAD)
            {
                stack.push(Type.LONG);
            }
            else
            {
                throw new RuntimeException("Unknown load instruction type: "
                        + inst);
            }
        }
        else if (inst instanceof StoreInstruction)
        {
            stack.pop();
        }
        else
        {
            throw new RuntimeException("Unknown local instruction type: "
                    + inst);
        }
    }

    private void handleReturn(ReturnInstruction inst)
    {
        stack.clear();
        if (inst.getType() != Type.VOID)
        {
            stack.push(inst.getType());
        }
    }

    private void handleStack(StackInstruction inst)
    {
        if (inst instanceof DUP)
        {
            stack.push(stack.peek()); // duplicate!
        }
        else if (inst instanceof DUP_X1)
        {
            Type t1 = stack.pop();
            Type t2 = stack.pop();
            stack.push(t1);
            stack.push(t2);
            stack.push(t1);
        }
        else if (inst instanceof DUP_X2)
        {
            // TODO DUP_X2
            /*
             * Type t1 = stack.pop(); Type t2 = stack.pop(); Type t3 =
             * stack.pop(); stack.push(t1); stack.push(t3); stack.push(t2);
             * stack.push(t1);
             */
            assert false;
        }
        else if (inst instanceof DUP2)
        {
            // TODO DUP2
            assert false;
        }
        else if (inst instanceof DUP2_X1)
        {
            // TODO DUP2_X11
            assert false;
        }
        else if (inst instanceof DUP2_X2)
        {
            // TODO DUP2_X2
            assert false;
        }
        else if (inst instanceof POP)
        {
            stack.pop();
        }
        else if (inst instanceof POP2)
        {
            // TODO POP2
            assert false;
        }
        else if (inst instanceof SWAP)
        {
            Type t1 = stack.pop();
            Type t2 = stack.pop();
            stack.push(t1);
            stack.push(t2);
        }
        else
        {
            throw new RuntimeException("Unknown stack instruction type: "
                    + inst);
        }
    }

    private void popNumPut(int n, Type type)
    {
        popNum(n);
        stack.push(type);
    }

    private void popNum(int n)
    {
        for (int i = 0; i < n; i++)
        {
            stack.pop();
        }
    }

    public Type peek()
    {
        return stack.peek();
    }

    public static void main(String args[]) throws ClassNotFoundException
    {
        testClass(Class.forName("java.lang.Integer"));
        testClass(BranchInstruction.class);
        testClass(TypeStack.class);
        testClass(Type.class);
    }

    public static void testClass(Class testClass) throws ClassNotFoundException
    {
        System.out.printf("testing %s%n...", testClass);
        
        ClassLoaderRepository load;
        if(testClass.getClassLoader() == null)
            load = new ClassLoaderRepository(java.lang.ClassLoader.getSystemClassLoader());
        else
            load = new ClassLoaderRepository(testClass.getClassLoader());
            
        if(load == null)
            throw new RuntimeException("Null class loader for class " + testClass);
            
        JavaClass clazz = load.loadClass(testClass);
        
        for (Method meth : clazz.getMethods())
        {
            MethodGen mg = new MethodGen(meth, TypeStack.class.getName(),
                    new ConstantPoolGen(clazz.getConstantPool()));
            System.out.println("\n$$$$$$$$$$$$$$$$$$$$$$$$\nTesting method "
                    + mg + " ...");
            TypeStack stack = new TypeStack(clazz.getConstantPool(), mg.getInstructionList(), mg.getExceptionHandlers(), mg.getArgumentTypes(), mg.getReturnType());
            
            for (InstructionHandle inst : mg.getInstructionList().getInstructionHandles())
            {
                try
                {
                    Stack<Type> s = stack.getAfterInst(inst);
                    System.out.printf("After inst %s, have type %s with size %d%n", inst.toString(), s.peek().toString(), s.size());
                }
                catch (EmptyStackException e)
                {
                    System.out.printf("After inst %s, stack is empty%n", inst.toString());
                }
            }
            
            int numRet = meth.getReturnType() == Type.VOID ? 0 : 1;
            assert stack.size() == numRet : 
                "Stack must be emtpy or size 1 after method is complete!!!\n" +
                "It is actually size " + stack.size();
            
            if(meth.getReturnType() != Type.VOID)
                assert sameType(meth.getReturnType(), stack.peek()) :
                    "Invalid return type " + stack.peek() + "\n" +
                    "Expected " + meth.getReturnType();
        }
    }

    private Stack<Type> getAfterInst(InstructionHandle inst)
    {
        assert inst != null;
        Stack<Type> s = stackMap.get(inst);
        if (s == null)
            throw new IllegalArgumentException("Could not find " + inst
                    + " in the map");
        return copyOfStack(s);
    }
    
    private void assertStack(Type... types)
    {
        for(int i = 0; i < types.length; i++)
        {
            if(!sameType(stack.get(stack.size() - i - 1), types[i]))
                assert false : "Wanted " + types[i] + " but got " + stack.get(stack.size() - i - 1);
        }
    }

    private static boolean sameType(Type t1, Type t2)
    {
        return both(Type.BOOLEAN, t1, t2) || both(Type.BYTE, t1, t2) || both(Type.CHAR, t1, t2)
            || both(Type.DOUBLE, t1, t2) || both(Type.FLOAT, t1, t2)
            || both(Type.INT, t1, t2) || both(Type.LONG, t1, t2)
            || both(Type.SHORT, t1, t2)
            || (t1 instanceof ReferenceType && t2 instanceof ReferenceType);
    }
    
    private static boolean both(Type t, Type t1, Type t2)
    {
        //System.out.printf("%s --- %s --- %s%n", t, t1, t2);
        
        Class c = t.getClass();
        return (c.isInstance(t1)) && (c.isInstance(t2));
    }

    public boolean isEmpty()
    {
        return stack.isEmpty();
    }

    public int size()
    {
        return stack.size();
    }

    public void not_used()
    {
        assert false : "We don't use this superclass";
    }

    public Type pop_check(Type type)
    {
        Type top = stack.pop();
        assert type.equals(top) : "expected " + type + " found " + top;
        return (top);
    }

    private static void notSupported(Instruction inst)
    {
        throw new RuntimeException("Unsupported instruction: " + inst);
    }
    
    private void doMath(int i, float f, long l)
    {
        i = i + 3 + 4;
        f = f - 3f - 4f + 5f*7f;
        l = l + 6l*9l - 3l;
    }
}
