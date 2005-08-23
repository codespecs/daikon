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
//  copy stack, modify it, 

public final class TypeStack
{
    private Stack<Type> stack = new Stack<Type>();
    private ConstantPoolGen pool;

    public TypeStack(ClassGen gen)
    {
        pool = gen.getConstantPool();
    }

    public TypeStack(ConstantPool p)
    {
        pool = new ConstantPoolGen(p);
    }

    public void nextInstruction(Instruction inst)
    {
        assert inst != null;

        //System.out.println("Processing instruction: " + inst);

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
            //NO CHANGE
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
            //NO CHANGE
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
    }
    
    private void handleMath(ArithmeticInstruction inst)
    {
        if(inst instanceof DADD)
        {
            popNumPut(2, Type.DOUBLE);
        }
        else if(inst instanceof DDIV)
        {
            popNumPut(2, Type.DOUBLE);
        }
        else if(inst instanceof DMUL)
        {
            popNumPut(2, Type.DOUBLE);
        }
        else if(inst instanceof DNEG)
        {
            popNumPut(1, Type.DOUBLE);
        }
        else if(inst instanceof DREM)
        {
            popNumPut(2, Type.DOUBLE);
        }
        else if(inst instanceof DSUB)
        {
            popNumPut(2, Type.DOUBLE);
        }
        else if(inst instanceof FADD)
        {
            popNumPut(2, Type.FLOAT);
        }
        else if(inst instanceof FDIV)
        {
            popNumPut(2, Type.FLOAT);
        }
        else if(inst instanceof FMUL)
        {
            popNumPut(2, Type.FLOAT);
        }
        else if(inst instanceof FNEG)
        {
            popNumPut(1, Type.FLOAT);
        }
        else if(inst instanceof FREM)
        {
            popNumPut(2, Type.FLOAT);
        }
        else if(inst instanceof FSUB)
        {
            popNumPut(2, Type.FLOAT);
        }
        else if(inst instanceof IADD)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof IAND)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof IDIV)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof IMUL)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof INEG)
        {
            popNumPut(1, Type.INT);
        }
        else if(inst instanceof IOR)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof IREM)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof ISHL)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof ISHR)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof ISUB)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof IUSHR)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof IXOR)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof LADD)
        {
            popNumPut(2, Type.LONG);
        }
        else if(inst instanceof LAND)
        {
        }
        else if(inst instanceof LDIV)
        {
            popNumPut(2, Type.LONG);
        }
        else if(inst instanceof LMUL)
        {
            popNumPut(2, Type.LONG);
        }
        else if(inst instanceof LNEG)
        {
            popNumPut(1, Type.LONG);
        }
        else if(inst instanceof LOR)
        {
            popNumPut(2, Type.LONG);
        }
        else if(inst instanceof LREM)
        {
            popNumPut(2, Type.LONG);
        }
        else if(inst instanceof LSHL)
        {
            popNumPut(2, Type.LONG);
        }
        else if(inst instanceof LSHR)
        {
            popNumPut(2, Type.LONG);
        }
        else if(inst instanceof LSUB)
        {
            popNumPut(2, Type.LONG);
        }
        else if(inst instanceof LUSHR)
        {
            popNumPut(2, Type.LONG);
        }
        else if(inst instanceof LXOR)
        {
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
        if(inst instanceof AALOAD)
        {
            popNumPut(2, Type.OBJECT);
        }
        else if(inst instanceof AASTORE)
        {
            popNum(3);
        }
        else if(inst instanceof BALOAD)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof BASTORE)
        {
            popNum(3);
        }
        else if(inst instanceof CALOAD)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof CASTORE)
        {
            popNum(3);
        }
        else if(inst instanceof DALOAD)
        {
            popNumPut(2, Type.DOUBLE);
        }
        else if(inst instanceof DASTORE)
        {
            popNum(3);
        }
        else if(inst instanceof FALOAD)
        {
            popNumPut(2, Type.FLOAT);
        }
        else if(inst instanceof FASTORE)
        {
            popNum(3);
        }
        else if(inst instanceof IALOAD)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof IASTORE)
        {
            popNum(3);
        }
        else if(inst instanceof LALOAD)
        {
            popNumPut(2, Type.LONG);
        }
        else if(inst instanceof LASTORE)
        {
            popNum(3);
        }
        else if(inst instanceof SALOAD)
        {
            popNumPut(2, Type.INT);
        }
        else if(inst instanceof SASTORE)
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
        if(inst instanceof GotoInstruction)
        {
            if(inst instanceof GOTO)
            {
                //NO CHANGE
            }
            else if(inst instanceof GOTO_W)
            {
                //NO CHANGE
            }
            else
            {
                throw new RuntimeException("Unknown goto instruction type: "
                        + inst);
            }
        }
        else if (inst instanceof IfInstruction)
        {                
            if(inst instanceof IF_ACMPEQ)
            {
                popNum(2);
            }
            else if(inst instanceof IF_ACMPNE)
            {
                popNum(2);
            }
            else if(inst instanceof IF_ICMPEQ)
            {
                popNum(2);
            }
            else if(inst instanceof IF_ICMPGE)
            {
                popNum(2);
            }
            else if(inst instanceof IF_ICMPGT)
            {
                popNum(2);
            }
            else if(inst instanceof IF_ICMPLE)
            {
                popNum(2);
            }
            else if(inst instanceof IF_ICMPLT)
            {
                popNum(2);
            }
            else if(inst instanceof IF_ICMPNE)
            {
                popNum(2);
            }
            else if(inst instanceof IFEQ)
            {
                stack.pop();
            }
            else if(inst instanceof IFGE)
            {
                stack.pop();
            }
            else if(inst instanceof IFGT)
            {
                stack.pop();
            }
            else if(inst instanceof IFLE)
            {
                stack.pop();
            }
            else if(inst instanceof IFLT)
            {
                stack.pop();
            }
            else if(inst instanceof IFNE)
            {
                stack.pop();
            }
            else if(inst instanceof IFNONNULL)
            {
                stack.pop();
            }
            else if(inst instanceof IFNULL)
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
            //TODO Jsr
            notSupported(inst);
        }
        else if (inst instanceof Select)
        {
            if(inst instanceof LOOKUPSWITCH)
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

    private void  handleConv(ConversionInstruction inst)
    {
        if(inst instanceof D2F)
        {
            popNumPut(1, Type.FLOAT);
        }
        else if(inst instanceof D2I)
        {
            popNumPut(1, Type.INT);
        }
        else if(inst instanceof D2L)
        {
            popNumPut(1, Type.LONG);
        }
        else if(inst instanceof F2D)
        {
            popNumPut(1, Type.DOUBLE);
        }
        else if(inst instanceof F2I)
        {
            popNumPut(1, Type.INT);
        }
        else if(inst instanceof F2L)
        {
            popNumPut(1, Type.LONG);
        }
        else if(inst instanceof I2B)
        {
            popNumPut(1, Type.INT);
        }
        else if(inst instanceof I2C)
        {
            popNumPut(1, Type.INT);
        }
        else if(inst instanceof I2D)
        {
            popNumPut(1, Type.DOUBLE);
        }
        else if(inst instanceof I2F)
        {
            popNumPut(1, Type.FLOAT);
        }
        else if(inst instanceof I2L)
        {
            popNumPut(1, Type.LONG);
        }
        else if(inst instanceof I2S)
        {
            popNumPut(1, Type.INT);
        }
        else if(inst instanceof L2D)
        {
            popNumPut(1, Type.DOUBLE);
        }
        else if(inst instanceof L2F)
        {
            popNumPut(1, Type.FLOAT);
        }
        else if(inst instanceof L2I)
        {
            popNumPut(1, Type.INT);
        }
        else 
        {
            throw new RuntimeException("Unknown conversion instruction type: "
                    + inst);
        }
    }
    
    private void  handleCP(CPInstruction inst)
    {
        if(inst instanceof ANEWARRAY)
        {
            popNumPut(1, Type.OBJECT);
        }
        else if(inst instanceof CHECKCAST)
        {
            //NO CHANGE
        }
        else if(inst instanceof FieldOrMethod)
        {
            if(inst instanceof GETFIELD)
            {
                FieldInstruction f = (FieldInstruction) inst;
                popNumPut(1, f.getFieldType(pool));
            }
            else if(inst instanceof GETSTATIC)
            {
                FieldInstruction f = (FieldInstruction) inst;
                stack.push(f.getFieldType(pool));
            }
            else if(inst instanceof PUTFIELD)
            {
                popNum(2);
            }
            else if(inst instanceof PUTSTATIC)
            {
                stack.pop();
            }
            else if(inst instanceof INVOKEINTERFACE)
            {
                InvokeInstruction inv = (InvokeInstruction) inst;
                popNum(inv.getArgumentTypes(pool).length + 1);
                
                Type ret = inv.getReturnType(pool);
                if(ret != Type.VOID)
                    stack.push(ret);
            }
            else if(inst instanceof INVOKESPECIAL)
            {
                InvokeInstruction inv = (InvokeInstruction) inst;
                popNum(inv.getArgumentTypes(pool).length + 1);
                
                Type ret = inv.getReturnType(pool);
                if(ret != Type.VOID)
                    stack.push(ret);
            }
            else if(inst instanceof INVOKESTATIC)
            {
                InvokeInstruction inv = (InvokeInstruction) inst;
                popNum(inv.getArgumentTypes(pool).length);
                
                Type ret = inv.getReturnType(pool);
                if(ret != Type.VOID)
                    stack.push(ret);
            }
            else if(inst instanceof INVOKEVIRTUAL)
            {
                InvokeInstruction inv = (InvokeInstruction) inst;
                popNum(inv.getArgumentTypes(pool).length + 1);
                
                Type ret = inv.getReturnType(pool);
                if(ret != Type.VOID)
                    stack.push(ret);
            }
            else
            {
                throw new RuntimeException("Unknown field or method instruction type: "
                        + inst);
            }
        }
        else if(inst instanceof INSTANCEOF)
        {
            popNumPut(1, Type.INT);
        }
        else if(inst instanceof LDC)
        {
            LDC ldc = (LDC) inst;
            stack.push(ldc.getType(pool));
        }
        else if(inst instanceof LDC2_W)
        {
            LDC2_W ldc = (LDC2_W) inst;
            stack.push(ldc.getType(pool));
        }
        else if(inst instanceof MULTIANEWARRAY)
        {
            MULTIANEWARRAY multi = (MULTIANEWARRAY) inst;
            
            //pop counts, push arraryRef...
            popNumPut(multi.getDimensions(), Type.OBJECT);
        }
        else if(inst instanceof NEW)
        {
            stack.push(Type.OBJECT);
        }
        else
        {
            throw new RuntimeException("Unknown CP instruction type: "
                    + inst);
        }
    }
    
    
    private void handleLocal(LocalVariableInstruction inst)
    {
        if (inst instanceof IINC)
        {
            //NO CHANGE
        }
        else if (inst instanceof LoadInstruction)
        {
            if(inst instanceof ALOAD)
            {
                stack.push(Type.OBJECT);
            }
            else if(inst instanceof DLOAD)
            {
                stack.push(Type.DOUBLE);
            }
            else if(inst instanceof FLOAD)
            {
                stack.push(Type.FLOAT);
            }
            else if(inst instanceof ILOAD)
            {
                stack.push(Type.INT);
            }
            else if(inst instanceof LLOAD)
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
    }

    private void handleStack(StackInstruction inst)
    {
        if (inst instanceof DUP)
        {
            stack.push(stack.peek()); //duplicate!
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
        testClass(TypeStack.class);
        //testClass(Type.class);
    }
    
    public static void testClass(Class testClass) throws ClassNotFoundException
    {
        System.out.printf("testing %s%n...", testClass);

        JavaClass clazz = new ClassLoaderRepository(testClass
                .getClassLoader()).loadClass(testClass);
        for (Method meth : clazz.getMethods())
        {
            MethodGen mg = new MethodGen(meth, TypeStack.class.getName(),
                    new ConstantPoolGen(clazz.getConstantPool()));

            System.out.println("\n$$$$$$$$$$$$$$$$$$$$$$$$\nTesting method " + mg + " ...");
            TypeStack stack = new TypeStack(clazz.getConstantPool());
            for (Instruction inst : mg.getInstructionList().getInstructions())
            {
                stack.nextInstruction(inst);
                
                try
                {
                    System.out.printf("After inst %s, have type %s with size %d%n", 
                            inst.toString(), stack.peek().toString(), stack.size());
                }
                catch(EmptyStackException e)
                {
                    System.out.printf("After inst %s, stack is empty%n", inst.toString());
                }
            }
            
            assert stack.isEmpty() : "Stack must be emtpy after method is complete!!!";
        }
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
}
