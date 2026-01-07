package daikon.dcomp;

import static java.lang.classfile.ClassFile.ACC_ABSTRACT;
import static java.lang.classfile.ClassFile.ACC_ANNOTATION;
import static java.lang.classfile.ClassFile.ACC_BRIDGE;
import static java.lang.classfile.ClassFile.ACC_NATIVE;
import static java.lang.classfile.ClassFile.ACC_PRIVATE;
import static java.lang.classfile.ClassFile.ACC_PROTECTED;
import static java.lang.classfile.ClassFile.ACC_PUBLIC;
import static java.lang.classfile.ClassFile.ACC_STATIC;
import static java.lang.classfile.Opcode.AALOAD;
import static java.lang.classfile.Opcode.AASTORE;
import static java.lang.classfile.Opcode.ACONST_NULL;
import static java.lang.classfile.Opcode.ALOAD;
import static java.lang.classfile.Opcode.ALOAD_0;
import static java.lang.classfile.Opcode.ALOAD_1;
import static java.lang.classfile.Opcode.ALOAD_2;
import static java.lang.classfile.Opcode.ALOAD_3;
import static java.lang.classfile.Opcode.ANEWARRAY;
import static java.lang.classfile.Opcode.ARETURN;
import static java.lang.classfile.Opcode.ARRAYLENGTH;
import static java.lang.classfile.Opcode.ASTORE;
import static java.lang.classfile.Opcode.ASTORE_0;
import static java.lang.classfile.Opcode.ASTORE_1;
import static java.lang.classfile.Opcode.ASTORE_2;
import static java.lang.classfile.Opcode.ASTORE_3;
import static java.lang.classfile.Opcode.ATHROW;
import static java.lang.classfile.Opcode.BALOAD;
import static java.lang.classfile.Opcode.BASTORE;
import static java.lang.classfile.Opcode.BIPUSH;
import static java.lang.classfile.Opcode.CALOAD;
import static java.lang.classfile.Opcode.CASTORE;
import static java.lang.classfile.Opcode.CHECKCAST;
import static java.lang.classfile.Opcode.D2F;
import static java.lang.classfile.Opcode.D2I;
import static java.lang.classfile.Opcode.D2L;
import static java.lang.classfile.Opcode.DADD;
import static java.lang.classfile.Opcode.DALOAD;
import static java.lang.classfile.Opcode.DASTORE;
import static java.lang.classfile.Opcode.DCMPG;
import static java.lang.classfile.Opcode.DCMPL;
import static java.lang.classfile.Opcode.DCONST_0;
import static java.lang.classfile.Opcode.DCONST_1;
import static java.lang.classfile.Opcode.DDIV;
import static java.lang.classfile.Opcode.DLOAD;
import static java.lang.classfile.Opcode.DLOAD_0;
import static java.lang.classfile.Opcode.DLOAD_1;
import static java.lang.classfile.Opcode.DLOAD_2;
import static java.lang.classfile.Opcode.DLOAD_3;
import static java.lang.classfile.Opcode.DMUL;
import static java.lang.classfile.Opcode.DNEG;
import static java.lang.classfile.Opcode.DREM;
import static java.lang.classfile.Opcode.DRETURN;
import static java.lang.classfile.Opcode.DSTORE;
import static java.lang.classfile.Opcode.DSTORE_0;
import static java.lang.classfile.Opcode.DSTORE_1;
import static java.lang.classfile.Opcode.DSTORE_2;
import static java.lang.classfile.Opcode.DSTORE_3;
import static java.lang.classfile.Opcode.DSUB;
import static java.lang.classfile.Opcode.DUP;
import static java.lang.classfile.Opcode.DUP2;
import static java.lang.classfile.Opcode.DUP2_X1;
import static java.lang.classfile.Opcode.DUP2_X2;
import static java.lang.classfile.Opcode.DUP_X1;
import static java.lang.classfile.Opcode.DUP_X2;
import static java.lang.classfile.Opcode.F2D;
import static java.lang.classfile.Opcode.F2I;
import static java.lang.classfile.Opcode.F2L;
import static java.lang.classfile.Opcode.FADD;
import static java.lang.classfile.Opcode.FALOAD;
import static java.lang.classfile.Opcode.FASTORE;
import static java.lang.classfile.Opcode.FCMPG;
import static java.lang.classfile.Opcode.FCMPL;
import static java.lang.classfile.Opcode.FCONST_0;
import static java.lang.classfile.Opcode.FCONST_1;
import static java.lang.classfile.Opcode.FCONST_2;
import static java.lang.classfile.Opcode.FDIV;
import static java.lang.classfile.Opcode.FLOAD;
import static java.lang.classfile.Opcode.FLOAD_0;
import static java.lang.classfile.Opcode.FLOAD_1;
import static java.lang.classfile.Opcode.FLOAD_2;
import static java.lang.classfile.Opcode.FLOAD_3;
import static java.lang.classfile.Opcode.FMUL;
import static java.lang.classfile.Opcode.FNEG;
import static java.lang.classfile.Opcode.FREM;
import static java.lang.classfile.Opcode.FRETURN;
import static java.lang.classfile.Opcode.FSTORE;
import static java.lang.classfile.Opcode.FSTORE_0;
import static java.lang.classfile.Opcode.FSTORE_1;
import static java.lang.classfile.Opcode.FSTORE_2;
import static java.lang.classfile.Opcode.FSTORE_3;
import static java.lang.classfile.Opcode.FSUB;
import static java.lang.classfile.Opcode.GETFIELD;
import static java.lang.classfile.Opcode.GETSTATIC;
import static java.lang.classfile.Opcode.GOTO;
import static java.lang.classfile.Opcode.GOTO_W;
import static java.lang.classfile.Opcode.I2B;
import static java.lang.classfile.Opcode.I2C;
import static java.lang.classfile.Opcode.I2D;
import static java.lang.classfile.Opcode.I2F;
import static java.lang.classfile.Opcode.I2L;
import static java.lang.classfile.Opcode.I2S;
import static java.lang.classfile.Opcode.IADD;
import static java.lang.classfile.Opcode.IALOAD;
import static java.lang.classfile.Opcode.IAND;
import static java.lang.classfile.Opcode.IASTORE;
import static java.lang.classfile.Opcode.ICONST_0;
import static java.lang.classfile.Opcode.ICONST_1;
import static java.lang.classfile.Opcode.ICONST_2;
import static java.lang.classfile.Opcode.ICONST_3;
import static java.lang.classfile.Opcode.ICONST_4;
import static java.lang.classfile.Opcode.ICONST_5;
import static java.lang.classfile.Opcode.ICONST_M1;
import static java.lang.classfile.Opcode.IDIV;
import static java.lang.classfile.Opcode.IFEQ;
import static java.lang.classfile.Opcode.IFGE;
import static java.lang.classfile.Opcode.IFGT;
import static java.lang.classfile.Opcode.IFLE;
import static java.lang.classfile.Opcode.IFLT;
import static java.lang.classfile.Opcode.IFNE;
import static java.lang.classfile.Opcode.IFNONNULL;
import static java.lang.classfile.Opcode.IFNULL;
import static java.lang.classfile.Opcode.IF_ACMPEQ;
import static java.lang.classfile.Opcode.IF_ACMPNE;
import static java.lang.classfile.Opcode.IF_ICMPEQ;
import static java.lang.classfile.Opcode.IF_ICMPGE;
import static java.lang.classfile.Opcode.IF_ICMPGT;
import static java.lang.classfile.Opcode.IF_ICMPLE;
import static java.lang.classfile.Opcode.IF_ICMPLT;
import static java.lang.classfile.Opcode.IF_ICMPNE;
import static java.lang.classfile.Opcode.IINC;
import static java.lang.classfile.Opcode.ILOAD;
import static java.lang.classfile.Opcode.ILOAD_0;
import static java.lang.classfile.Opcode.ILOAD_1;
import static java.lang.classfile.Opcode.ILOAD_2;
import static java.lang.classfile.Opcode.ILOAD_3;
import static java.lang.classfile.Opcode.IMUL;
import static java.lang.classfile.Opcode.INEG;
import static java.lang.classfile.Opcode.INSTANCEOF;
import static java.lang.classfile.Opcode.INVOKEDYNAMIC;
import static java.lang.classfile.Opcode.INVOKEINTERFACE;
import static java.lang.classfile.Opcode.INVOKESPECIAL;
import static java.lang.classfile.Opcode.INVOKESTATIC;
import static java.lang.classfile.Opcode.INVOKEVIRTUAL;
import static java.lang.classfile.Opcode.IOR;
import static java.lang.classfile.Opcode.IREM;
import static java.lang.classfile.Opcode.IRETURN;
import static java.lang.classfile.Opcode.ISHL;
import static java.lang.classfile.Opcode.ISHR;
import static java.lang.classfile.Opcode.ISTORE;
import static java.lang.classfile.Opcode.ISTORE_0;
import static java.lang.classfile.Opcode.ISTORE_1;
import static java.lang.classfile.Opcode.ISTORE_2;
import static java.lang.classfile.Opcode.ISTORE_3;
import static java.lang.classfile.Opcode.ISUB;
import static java.lang.classfile.Opcode.IUSHR;
import static java.lang.classfile.Opcode.IXOR;
import static java.lang.classfile.Opcode.JSR;
import static java.lang.classfile.Opcode.JSR_W;
import static java.lang.classfile.Opcode.L2D;
import static java.lang.classfile.Opcode.L2F;
import static java.lang.classfile.Opcode.L2I;
import static java.lang.classfile.Opcode.LADD;
import static java.lang.classfile.Opcode.LALOAD;
import static java.lang.classfile.Opcode.LAND;
import static java.lang.classfile.Opcode.LASTORE;
import static java.lang.classfile.Opcode.LCMP;
import static java.lang.classfile.Opcode.LCONST_0;
import static java.lang.classfile.Opcode.LCONST_1;
import static java.lang.classfile.Opcode.LDC;
import static java.lang.classfile.Opcode.LDC2_W;
import static java.lang.classfile.Opcode.LDC_W;
import static java.lang.classfile.Opcode.LDIV;
import static java.lang.classfile.Opcode.LLOAD;
import static java.lang.classfile.Opcode.LLOAD_0;
import static java.lang.classfile.Opcode.LLOAD_1;
import static java.lang.classfile.Opcode.LLOAD_2;
import static java.lang.classfile.Opcode.LLOAD_3;
import static java.lang.classfile.Opcode.LMUL;
import static java.lang.classfile.Opcode.LNEG;
import static java.lang.classfile.Opcode.LOOKUPSWITCH;
import static java.lang.classfile.Opcode.LOR;
import static java.lang.classfile.Opcode.LREM;
import static java.lang.classfile.Opcode.LRETURN;
import static java.lang.classfile.Opcode.LSHL;
import static java.lang.classfile.Opcode.LSHR;
import static java.lang.classfile.Opcode.LSTORE;
import static java.lang.classfile.Opcode.LSTORE_0;
import static java.lang.classfile.Opcode.LSTORE_1;
import static java.lang.classfile.Opcode.LSTORE_2;
import static java.lang.classfile.Opcode.LSTORE_3;
import static java.lang.classfile.Opcode.LSUB;
import static java.lang.classfile.Opcode.LUSHR;
import static java.lang.classfile.Opcode.LXOR;
import static java.lang.classfile.Opcode.MONITORENTER;
import static java.lang.classfile.Opcode.MONITOREXIT;
import static java.lang.classfile.Opcode.MULTIANEWARRAY;
import static java.lang.classfile.Opcode.NEW;
import static java.lang.classfile.Opcode.NEWARRAY;
import static java.lang.classfile.Opcode.NOP;
import static java.lang.classfile.Opcode.POP;
import static java.lang.classfile.Opcode.POP2;
import static java.lang.classfile.Opcode.PUTFIELD;
import static java.lang.classfile.Opcode.PUTSTATIC;
import static java.lang.classfile.Opcode.RET;
import static java.lang.classfile.Opcode.RETURN;
import static java.lang.classfile.Opcode.SALOAD;
import static java.lang.classfile.Opcode.SASTORE;
import static java.lang.classfile.Opcode.SIPUSH;
import static java.lang.classfile.Opcode.SWAP;
import static java.lang.classfile.Opcode.TABLESWITCH;
import static java.lang.constant.ConstantDescs.CD_Class;
import static java.lang.constant.ConstantDescs.CD_Object;
import static java.lang.constant.ConstantDescs.CD_String;
import static java.lang.constant.ConstantDescs.CD_Throwable;
import static java.lang.constant.ConstantDescs.CD_boolean;
import static java.lang.constant.ConstantDescs.CD_byte;
import static java.lang.constant.ConstantDescs.CD_char;
import static java.lang.constant.ConstantDescs.CD_double;
import static java.lang.constant.ConstantDescs.CD_float;
import static java.lang.constant.ConstantDescs.CD_int;
import static java.lang.constant.ConstantDescs.CD_long;
import static java.lang.constant.ConstantDescs.CD_short;
import static java.lang.constant.ConstantDescs.CD_void;
import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.DynComp;
import daikon.chicory.ClassInfo;
import daikon.chicory.DaikonWriter;
import daikon.chicory.Instrument24;
import daikon.chicory.MethodGen24;
import daikon.chicory.MethodInfo;
import daikon.chicory.Runtime;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.bcelutil.SimpleLog;
import daikon.plumelib.options.Option;
import daikon.plumelib.reflection.Signatures;
import daikon.plumelib.util.EntryReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.classfile.Annotation;
import java.lang.classfile.Attribute;
import java.lang.classfile.ClassBuilder;
import java.lang.classfile.ClassElement;
import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassModel;
import java.lang.classfile.ClassTransform;
import java.lang.classfile.CodeBuilder;
import java.lang.classfile.CodeElement;
import java.lang.classfile.CodeModel;
import java.lang.classfile.FieldModel;
import java.lang.classfile.Instruction;
import java.lang.classfile.Interfaces;
import java.lang.classfile.Label;
import java.lang.classfile.MethodBuilder;
import java.lang.classfile.MethodElement;
import java.lang.classfile.MethodModel;
import java.lang.classfile.Opcode;
import java.lang.classfile.TypeKind;
import java.lang.classfile.attribute.CodeAttribute;
import java.lang.classfile.attribute.RuntimeVisibleAnnotationsAttribute;
import java.lang.classfile.constantpool.ClassEntry;
import java.lang.classfile.constantpool.ConstantPoolBuilder;
import java.lang.classfile.constantpool.LoadableConstantEntry;
import java.lang.classfile.constantpool.MethodRefEntry;
import java.lang.classfile.constantpool.NameAndTypeEntry;
import java.lang.classfile.instruction.ArrayLoadInstruction;
import java.lang.classfile.instruction.ArrayStoreInstruction;
import java.lang.classfile.instruction.BranchInstruction;
import java.lang.classfile.instruction.ConstantInstruction;
import java.lang.classfile.instruction.ExceptionCatch;
import java.lang.classfile.instruction.FieldInstruction;
import java.lang.classfile.instruction.InvokeDynamicInstruction;
import java.lang.classfile.instruction.InvokeInstruction;
import java.lang.classfile.instruction.LabelTarget;
import java.lang.classfile.instruction.LineNumber;
import java.lang.classfile.instruction.LoadInstruction;
import java.lang.classfile.instruction.LocalVariable;
import java.lang.classfile.instruction.LocalVariableType;
import java.lang.classfile.instruction.LookupSwitchInstruction;
import java.lang.classfile.instruction.NewMultiArrayInstruction;
import java.lang.classfile.instruction.NewReferenceArrayInstruction;
import java.lang.classfile.instruction.OperatorInstruction;
import java.lang.classfile.instruction.ReturnInstruction;
import java.lang.classfile.instruction.StackInstruction;
import java.lang.classfile.instruction.StoreInstruction;
import java.lang.classfile.instruction.SwitchCase;
import java.lang.classfile.instruction.TableSwitchInstruction;
import java.lang.classfile.instruction.ThrowInstruction;
import java.lang.constant.ClassDesc;
import java.lang.constant.MethodTypeDesc;
import java.lang.reflect.AccessFlag;
import java.net.URL;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.checkerframework.checker.signature.qual.DotSeparatedIdentifiers;
import org.checkerframework.checker.signature.qual.FieldDescriptor;
import org.checkerframework.checker.signature.qual.Identifier;
import org.checkerframework.checker.signature.qual.MethodDescriptor;
import org.checkerframework.dataflow.qual.Pure;

/**
 * Instruments a class file to perform Dynamic Comparability.
 *
 * <p>This class is responsible for modifying bytecodes. Specifically, its main task is to add calls
 * into the DynComp Runtime to calculate comparability values. These added calls are sometimes
 * referred to as "hooks".
 *
 * <p>Instrument24 and DCInstrument24 use Java's ({@code java.lang.classfile}) APIs for reading and
 * modifying .class files. Those APIs were added in JDK 24. Compared to BCEL, these APIs are more
 * complete and robust (no more fiddling with StackMaps) and are always up to date with any .class
 * file changes (since they are part of the JDK). (We will need to continue to support
 * Instrument.java using BCEL, as we anticipate our clients using JDK 21 or less for quite some
 * time.)
 */
public class DCInstrument24 {

  /** A log to which to print debugging information about program instrumentation. */
  protected static SimpleLog debugInstrument = new SimpleLog(false);

  /**
   * Used when testing to continue processing if an error occurs. Currently, this flag is only used
   * by BuildJDK.
   */
  @Option("Halt if an instrumentation error occurs")
  public static boolean quit_if_error = true;

  /**
   * Start of user code for current method, prior to instrumenting, as a CodeModel Label. Note that
   * there are two kinds of Labels in the {@code java.lang.classfile} API. When a class file is
   * input, it contains CodeModel labels that are immutable. When our instrumention generates new
   * code that needs a label it gets one from the CodeBuilder.
   */
  protected @MonotonicNonNull Label oldStartLabel;

  /** Start of user code for current method, after instrumenting, as a CodeBuilder Label. */
  protected @MonotonicNonNull Label newStartLabel;

  /**
   * Index into current method's instruction list that points to where a call to the DynComp runtime
   * enter routine should be inserted (if needed) and where to define the newStartLabel.
   */
  protected int newStartIndex;

  // Variables used for calculating the state of a method's operand stack via simulation.

  // TODO: The following four variables are redefined for each method instrumented.
  // Rather than being global variables, they should be wrapped in a data structure that is passed
  // to the places that need it.

  /** Mapping from a label to its index into the method's codeList. */
  @SuppressWarnings("nullness:initialization.static.field.uninitialized") // TODO: method-local
  protected static Map<Label, Integer> labelIndexMap;

  /**
   * Mapping from a label to its operand stack. This is the state of the operand stack at the label,
   * prior to the execution of any instruction at that label.
   */
  @SuppressWarnings("nullness:initialization.static.field.uninitialized") // TODO: method-local
  protected static Map<Label, OperandStack24> labelOperandStackMap;

  /**
   * The state of operand stack prior to each byte code instruction of the method being simulated.
   * There is a unique array entry for each instruction.
   */
  @SuppressWarnings("nullness:initialization.static.field.uninitialized") // TODO: method-local
  protected static OperandStack24[] stacks;

  /**
   * The type of each parameter and local variable of the method being simulated. This value is
   * often unchanged during the method execution, but may differ when the compiler allocates more
   * than one local variable at the same offset.
   */
  @SuppressWarnings("nullness:initialization.static.field.uninitialized") // TODO: method-local
  protected static ClassDesc[] locals;

  /**
   * Record containing a work item for the operand stack calculation. The instructionIndex is an
   * index into the method's instruction list. The stack is the state of the operand stack prior to
   * the execution of the instruction.
   */
  record WorkItem(int instructionIndex, OperandStack24 stack) {}

  /**
   * Comparator to sort WorkItems with lowest instructionIndex first. This corresponds to the lowest
   * byte code instruction offset.
   */
  protected static Comparator<WorkItem> indexComparator =
      new Comparator<>() {
        @Override
        public int compare(WorkItem w1, WorkItem w2) {
          return Integer.compare(w1.instructionIndex, w2.instructionIndex);
        }
      };

  /** Queue of items for a method's operand stack calculation. */
  protected static SortedSet<WorkItem> worklist = new TreeSet<>(indexComparator);

  // Variables used for the entire class.

  /** The current class file. */
  protected ClassFile classFile;

  /** The current class. */
  protected ClassModel classModel;

  /** ConstantPool builder for current class. */
  private ConstantPoolBuilder poolBuilder;

  /** True if we are tracking any variables in any methods of the current class. */
  private boolean trackClass = false;

  /** ClassGen for the current class. */
  protected ClassGen24 classGen;

  /** Is the current class a member of the JDK? */
  protected boolean in_jdk;

  /** Has an {@code <init>} method completed initialization? */
  protected boolean constructor_is_initialized;

  /**
   * Record used to describe a new LocalVariable. When instrumentation wants to create a new method,
   * it creates a list containing one of these records for each local variable of the method. This
   * list is passed to {@link #copyCode} and {@link #instrumentCode} where the new variables are
   * created.
   */
  public record myLocalVariable(int slot, String name, ClassDesc descriptor) {}

  /**
   * A local variable added by our instrumentation to each method. It is an {@code Object} array,
   * always named {@code dcomp_tag_frame$5A}. The DynComp runtime uses the array to store the
   * current tag value for each parameter on method entry. These tag values are used to track
   * variable interactions. This allows the DynComp runtime to group variables into comparability
   * sets. All variables in a comparability set belong to the same "abstract type" of data that the
   * programmer likely intended to represent.
   */
  protected LocalVariable tagFrameLocal;

  // Type descriptors

  /** "java.lang.Object[]". */
  protected static final ClassDesc objectArrayCD = CD_Object.arrayType(1);

  /** The special DCompMarker type. */
  protected final ClassDesc dcomp_marker;

  // Signature descriptors

  // No parameters

  /** Type array with no parameters. */
  protected static final ClassDesc[] noArgsSig = new ClassDesc[0];

  // One parameter

  /** Type array with an int. */
  protected static ClassDesc[] intSig = {CD_int};

  /** Type array with a long. */
  protected static ClassDesc[] longSig = {CD_long};

  /** Type array with an object. */
  protected static ClassDesc[] object_arg = {CD_Object};

  /** Type array with an Object array. */
  protected static final ClassDesc[] objectArrayCD_arg = {objectArrayCD};

  // Two parameters

  /** Type array with a long and an int. */
  protected static ClassDesc[] longIntSig = {CD_long, CD_int};

  /** Type array with two objects. */
  protected static ClassDesc[] objectObjectSig = {CD_Object, CD_Object};

  // Debug loggers

  /** Log file if debug_native is enabled. */
  protected static SimpleLog debug_native = new SimpleLog(false);

  /** Log file if debug_dup is enabled. */
  protected static SimpleLog debug_dup = new SimpleLog(false);

  /**
   * Debug information about which classes and/or methods are transformed and why. Use
   * debugInstrument for actual instrumentation details.
   */
  protected static SimpleLog debug_transform = new SimpleLog(false);

  // Flags to enable additional console output for debugging
  /** If true, enable JUnit analysis debugging. */
  protected static final boolean debugJunitAnalysis = false;

  /** If true, enable {@link #getDefiningInterface} debugging. */
  protected static final boolean debugGetDefiningInterface = false;

  /** If true, enable {@link #handleInvoke} debugging. */
  protected static final boolean debugHandleInvoke = false;

  /** If true, enable operand stack debugging. */
  protected static final boolean debugOperandStack = false;

  // End of debug loggers.

  /** Keeps track of the methods that were not successfully instrumented. */
  protected List<String> skipped_methods = new ArrayList<>();

  /** If we're using an instrumented JDK, then "java.lang"; otherwise, "daikon.dcomp". */
  protected @DotSeparatedIdentifiers String dcompMarkerPrefix;

  /**
   * If we're using an instrumented JDK and the JDK version is > 8, then "java.lang"; otherwise,
   * "daikon.dcomp".
   */
  protected @DotSeparatedIdentifiers String dcompRuntimePrefix;

  /** Either "daikon.dcomp.DCRuntime" or "java.lang.DCRuntime". */
  private @BinaryName String dcompRuntimeClassName;

  /** The ClassDesc for the DynComp runtime support class. */
  private ClassDesc runtimeCD;

  /** Set of JUnit test classes. */
  protected static Set<String> junitTestClasses = new HashSet<>();

  /** Possible states of JUnit test discovery. */
  protected enum JunitState {
    /** Have not seen a JUnit class file. */
    NOT_SEEN,
    /** Have seen a JUnit class file. */
    STARTING,
    /** Have seen a JUnit class file that loads JUnit test classes. */
    TEST_DISCOVERY,
    /** Have completed identifying JUnit test classes and are instrumenting the code. */
    INSTRUMENTING
  }

  /** Current state of JUnit test discovery. */
  protected static JunitState junit_state = JunitState.NOT_SEEN;

  /** Have we seen {@code JUnitCommandLineParseResult.parse}? */
  protected static boolean junit_parse_seen = false;

  /**
   * Map from each static qualified field name to a unique integer id. Note that while it's
   * intuitive to think that each static should show up exactly once, that is not always true. A
   * static defined in a superclass can be accessed through each of its subclasses. In this case,
   * tag accessor methods must be added in each subclass and each should return the id of the field
   * in the superclass. This map is populated in {@link build_field_to_offset_map} and used in
   * {@link create_tag_accessors}.
   */
  static Map<String, Integer> static_field_id = new LinkedHashMap<>();

  /**
   * Map from binary class name to its access_flags. Used to cache the results of the lookup done in
   * {@link #getAccessFlags}. If a class is marked ACC_ANNOTATION then it will not have been
   * instrumented.
   */
  static Map<String, Integer> accessFlags = new HashMap<>();

  /** Integer constant of access_flag value of ACC_ANNOTATION. */
  static Integer Integer_ACC_ANNOTATION = Integer.valueOf(ACC_ANNOTATION);

  /**
   * Array of classes whose fields are not initialized from Java (i.e., these classes are
   * initialized by the JVM). Since the fields are not initialized from Java, their tag storage is
   * not allocated as part of a store, but rather must be allocated as part of a load. We call a
   * special runtime method for this so that we can check for this in other cases.
   */
  protected static final String[] uninit_classes = {
    "java.lang.String",
    "java.lang.Class",
    "java.lang.StringBuilder",
    "java.lang.AbstractStringBuilder",
  };

  /**
   * List of Object methods. Since we can't instrument Object, none of these can be instrumented,
   * and most of them don't provide useful comparability information anyway.
   *
   * <p>The equals method and the clone method are not listed here. They are special-cased in the
   * {@link #handleInvoke} routine.
   */
  protected static final MethodDef[] obj_methods = {
    new MethodDef("finalize", noArgsSig),
    new MethodDef("getClass", noArgsSig),
    new MethodDef("hashCode", noArgsSig),
    new MethodDef("notify", noArgsSig),
    new MethodDef("notifyall", noArgsSig),
    new MethodDef("toString", noArgsSig),
    new MethodDef("wait", noArgsSig),
    new MethodDef("wait", longSig),
    new MethodDef("wait", longIntSig),
  };

  /** Represents a method (by its name and parameter types). */
  static class MethodDef {
    /** Name of this method. */
    String name;

    /** Parameter types for this method. */
    ClassDesc[] arg_types;

    /**
     * Create a new MethodDef.
     *
     * @param name of method
     * @param arg_types of method
     */
    MethodDef(String name, ClassDesc[] arg_types) {
      this.name = name;
      this.arg_types = arg_types;
    }

    /**
     * Equality test for MethodDef.
     *
     * @param name of method
     * @param arg_types of method
     */
    @EnsuresNonNullIf(result = true, expression = "#1")
    boolean equals(@GuardSatisfied MethodDef this, String name, ClassDesc[] arg_types) {
      if (!name.equals(this.name)) {
        return false;
      }
      return Arrays.equals(this.arg_types, arg_types);
    }

    @EnsuresNonNullIf(result = true, expression = "#1")
    @Pure
    @Override
    public boolean equals(@GuardSatisfied MethodDef this, @GuardSatisfied @Nullable Object obj) {
      if (!(obj instanceof MethodDef)) {
        return false;
      }
      MethodDef md = (MethodDef) obj;
      return equals(md.name, md.arg_types);
    }

    @Pure
    @Override
    public int hashCode(@GuardSatisfied MethodDef this) {
      return Objects.hash(name, Arrays.hashCode(arg_types));
    }
  }

  /**
   * Initialize the class information and whether or not that class is part of the JDK. Note that
   * the fields poolBuilder, classGen and tagFrameLocal are not initialized until later.
   */
  @SuppressWarnings("nullness:initialization")
  public DCInstrument24(ClassFile classFile, ClassModel classModel, boolean in_jdk) {
    this.classFile = classFile;
    this.classModel = classModel;
    this.in_jdk = in_jdk;
    constructor_is_initialized = false;
    if (Premain.jdk_instrumented) {
      dcompMarkerPrefix = "java.lang";
    } else {
      dcompMarkerPrefix = "daikon.dcomp";
    }
    dcomp_marker = ClassDesc.of(Signatures.addPackage(dcompMarkerPrefix, "DCompMarker"));
    dcompRuntimePrefix = dcompMarkerPrefix;
    if (BcelUtil.javaVersion == 8) {
      dcompRuntimePrefix = "daikon.dcomp";
    }
    dcompRuntimeClassName = Signatures.addPackage(dcompRuntimePrefix, "DCRuntime");
    runtimeCD = ClassDesc.of(dcompRuntimeClassName);

    // Turn on some of the logging based on debug option.
    debugInstrument.enabled = DynComp.debug || Premain.debug_dcinstrument;
    debug_native.enabled = DynComp.debug;
    debug_transform.enabled = daikon.dcomp.Instrument24.debug_transform.enabled;

    if (debugOperandStack) {
      // Create a new PrintStream with autoflush enabled.
      PrintStream newOut = new PrintStream(System.out, true, UTF_8);
      // Reassign System.out to the new PrintStream.
      System.setOut(newOut);
      // Create a new PrintStream with autoflush enabled.
      PrintStream newErr = new PrintStream(System.err, true, UTF_8);
      // Reassign System.err to the new PrintStream.
      System.setErr(newErr);
    }
  }

  /**
   * Instruments a class to perform dynamic comparability and returns the new class definition. A
   * second version of each method in the class is created which is instrumented for comparability.
   *
   * @return the modified JavaClass
   */
  public byte @Nullable [] instrument(ClassInfo classInfo) {

    // Don't know where I got this idea.  They are executed.  Don't remember why
    // adding dcomp marker causes problems.
    // Don't instrument annotations.  They aren't executed and adding
    // the marker argument causes subtle errors
    if (classModel.flags().has(AccessFlag.ANNOTATION)) {
      debug_transform.log("Not instrumenting annotation %s%n", classInfo.class_name);
      return null;
    }

    // If a class has an EvoSuite annotation it may be instrumented by Evosuite;
    // thus, we should not instrument it before Evosuite does.
    for (final Attribute<?> attribute : classModel.attributes()) {
      if (attribute instanceof RuntimeVisibleAnnotationsAttribute rvaa) {
        for (final Annotation item : rvaa.annotations()) {
          if (item.className().stringValue().startsWith("Lorg/evosuite/runtime")) {
            debug_transform.log(
                "Not instrumenting possible Evosuite target: %s%n", classInfo.class_name);
            return null;
          }
        }
      }
    }

    try {
      return classFile.build(
          classModel.thisClass().asSymbol(),
          classBuilder -> instrumentClass(classBuilder, classModel, classInfo));
    } catch (Throwable t) {
      if (debugInstrument.enabled) {
        t.printStackTrace();
      }
      System.err.printf(
          "DynComp warning: Class %s is being skipped due to the following:%n",
          classInfo.class_name);
      System.err.printf("%s.%n", t);
      return null;
    }
  }

  /**
   * Instrument the given class.
   *
   * @param classBuilder for the class
   * @param classModel for the class
   * @param classInfo for the given class
   */
  private void instrumentClass(
      ClassBuilder classBuilder, ClassModel classModel, ClassInfo classInfo) {

    @BinaryName String classname = classInfo.class_name;
    classGen = new ClassGen24(classModel, classname, classBuilder);
    poolBuilder = classBuilder.constantPool();

    debug_transform.log(
        "%nInstrumenting class%s: %s%n", in_jdk ? " (in JDK)" : "", classInfo.class_name);
    debug_transform.indent();

    debugInstrument.log("Attributes:%n");
    for (Attribute<?> a : classModel.attributes()) {
      debugInstrument.log("  %s%n", a);
    }

    debugInstrument.log("Class Interfaces:%n");
    for (ClassEntry ce : classModel.interfaces()) {
      debugInstrument.log("  %s%n", ce.asInternalName());
    }

    trackClass = false;

    // Check to see if this class has a {@code clone} or {@code toString} method.
    // If so, add the {@code DCompClone} and/or the {@code DCompToString} interface.
    add_clone_and_tostring_interfaces(classGen);

    boolean junit_test_class = false;

    if (!in_jdk) {
      // A very tricky special case: If JUnit is running and the current
      // class has been passed to JUnit on the command line, then this
      // is a JUnit test class and our normal instrumentation will
      // cause JUnit to complain about multiple constructors and
      // methods that should have no arguments. To work around these
      // restrictions, we replace rather than duplicate each method
      // we instrument and we do not add the dcomp marker parameter.
      // We must also remember the class name so if we see a subsequent
      // call to one of its methods we do not add the dcomp argument.

      debugInstrument.log("junit_state: %s%n", junit_state);

      StackTraceElement[] stack_trace;

      switch (junit_state) {
        case NOT_SEEN:
          if (classname.startsWith("org.junit")) {
            junit_state = JunitState.STARTING;
          }
          break;

        case STARTING:
          // Now check to see if JUnit is looking for test class(es).
          stack_trace = Thread.currentThread().getStackTrace();
          // [0] is getStackTrace
          for (int i = 1; i < stack_trace.length; i++) {
            if (debugJunitAnalysis) {
              System.out.printf(
                  "%s : %s%n", stack_trace[i].getClassName(), stack_trace[i].getMethodName());
            }
            if (isJunitTrigger(stack_trace[i].getClassName(), stack_trace[i].getMethodName())) {
              junit_parse_seen = true;
              junit_state = JunitState.TEST_DISCOVERY;
              break;
            }
          }
          break;

        case TEST_DISCOVERY:
          // Now check to see if JUnit is done looking for test class(es).
          boolean local_junit_parse_seen = false;
          stack_trace = Thread.currentThread().getStackTrace();
          // [0] is getStackTrace
          for (int i = 1; i < stack_trace.length; i++) {
            if (debugJunitAnalysis) {
              System.out.printf(
                  "%s : %s%n", stack_trace[i].getClassName(), stack_trace[i].getMethodName());
            }
            if (isJunitTrigger(stack_trace[i].getClassName(), stack_trace[i].getMethodName())) {
              local_junit_parse_seen = true;
              break;
            }
          }
          if (junit_parse_seen && !local_junit_parse_seen) {
            junit_parse_seen = false;
            junit_state = JunitState.INSTRUMENTING;
          } else if (!junit_parse_seen && local_junit_parse_seen) {
            junit_parse_seen = true;
          }
          break;

        case INSTRUMENTING:
          if (debugJunitAnalysis) {
            stack_trace = Thread.currentThread().getStackTrace();
            // [0] is getStackTrace
            for (int i = 1; i < stack_trace.length; i++) {
              System.out.printf(
                  "%s : %s%n", stack_trace[i].getClassName(), stack_trace[i].getMethodName());
            }
          }
          // nothing to do
          break;

        default:
          throw new DynCompError("invalid junit_state");
      }

      debugInstrument.log("junit_state: %s%n", junit_state);

      if (junit_state == JunitState.TEST_DISCOVERY) {
        // We have a possible JUnit test class.  We need to verify by
        // one of two methods.  Either the class is a subclass of
        // junit.framework.TestCase or one of its methods has a
        // RuntimeVisibleAnnotation of org/junit/Test.
        Deque<String> classnameStack = new ArrayDeque<>();
        String super_class;
        String this_class = classname;
        while (true) {
          super_class = getSuperclassName(this_class);
          if (super_class == null) {
            // something has gone wrong
            break;
          }
          if (debugJunitAnalysis) {
            System.out.printf("this_class: %s%n", this_class);
            System.out.printf("super_class: %s%n", super_class);
          }
          if (super_class.equals("junit.framework.TestCase")) {
            // This is a JUnit test class and so are the
            // elements of classnameStack.
            junit_test_class = true;
            junitTestClasses.add(this_class);
            while (!classnameStack.isEmpty()) {
              junitTestClasses.add(classnameStack.pop());
            }
            break;
          } else if (super_class.equals("java.lang.Object")) {
            // We're done; not a JUnit test class.
            // Ignore items on classnameStack.
            break;
          }
          // Recurse and check the super_class.
          classnameStack.push(this_class);
          this_class = super_class;
        }
      }

      // Even if we have not detected that JUnit is active, any class that
      // contains a method with a RuntimeVisibleAnnotation of org/junit/Test
      // needs to be marked as a JUnit test class. (Daikon issue #536)

      if (!junit_test_class) {
        // need to check for JUnit Test annotation on a method
        searchloop:
        for (MethodModel mm : classModel.methods()) {
          for (final Attribute<?> attribute : mm.attributes()) {
            if (attribute instanceof RuntimeVisibleAnnotationsAttribute rvaa) {
              if (debugJunitAnalysis) {
                System.out.printf("attribute: %s%n", attribute);
              }
              for (final Annotation item : rvaa.annotations()) {
                String description = item.className().stringValue();
                if (debugJunitAnalysis) {
                  System.out.printf("item: %s%n", description);
                }
                if (description.endsWith("org/junit/Test;") // JUnit 4
                    || description.endsWith("org/junit/jupiter/api/Test;") // JUnit 5
                ) {
                  junit_test_class = true;
                  junitTestClasses.add(classname);
                  break searchloop;
                }
              }
            }
          }
        }
      }

      if (junit_test_class) {
        debugInstrument.log("JUnit test class: %s%n", classname);
      } else {
        debugInstrument.log("Not a JUnit test class: %s%n", classname);
      }
    }

    classInfo.isJunitTestClass = junit_test_class;

    if (classModel.majorVersion() < ClassFile.JAVA_6_VERSION) {
      System.out.printf(
          "DynComp warning: ClassFile: %s - class file version (%d) is out of date and may not be"
              + " processed correctly.%n",
          classname, classModel.majorVersion());
      // throw new DynCompError("Classfile out of date");
    }

    processAllMethods(classModel, classBuilder, classInfo);

    // Have all top-level classes implement the DCompInstrumented interface.
    if (classGen.getSuperclassName().equals("java.lang.Object")) {
      @SuppressWarnings("signature:assignment") // CF needs regex for @MethodDescriptor
      @MethodDescriptor String objectToBoolean = "(Ljava/lang/Object;)Z";
      // Add equals method if it doesn't already exist. This ensures
      // that an instrumented version, equals(Object, DCompMarker),
      // will be created in this class.
      MethodModel eq = classGen.containsMethod("equals", objectToBoolean);
      if (eq == null) {
        debugInstrument.log("Adding equals method%n");
        add_equals_method(classBuilder, classGen, classInfo);
      }

      // Add DCompInstrumented interface and the required
      // equals_dcomp_instrumented method.
      add_dcomp_interface(classBuilder, classGen, classInfo);
    }

    // Add interfaces to the class being built.
    classBuilder.withInterfaces(classGen.getInterfaceList());

    // Copy all other ClassElements to output class unchanged.
    for (ClassElement ce : classModel) {
      debugInstrument.log("ClassElement: %s%n", ce);
      switch (ce) {
        case MethodModel mm -> {}
        case Interfaces i -> {}
        // Copy all other ClassElements to output class unchanged.
        default -> classBuilder.with(ce);
      }
    }

    // Add tag accessor methods for each primitive in the class.
    create_tag_accessors(classGen);

    // We don't need to track class initialization in the JDK because
    // that is only used when printing comparability which is only done
    // for client classes.
    if (!in_jdk) {
      // If no clinit method, we need to add our own.
      if (!classInfo.hasClinit) {
        createClinit(classBuilder, classInfo);
      }

      // The code that builds the list of daikon variables for each ppt
      // needs to know what classes are instrumented.  Its looks in the
      // Chicory runtime for this information.
      if (trackClass) {
        debug_transform.log("DCInstrument adding %s to all class list%n", classInfo.class_name);
        synchronized (daikon.chicory.SharedData.all_classes) {
          daikon.chicory.SharedData.all_classes.add(classInfo);
        }
      }
    }

    debug_transform.exdent();
    debug_transform.log("Instrumentation complete: %s%n", classInfo.class_name);
  }

  /**
   * Adds a call to the DynComp Runtime {@code set_class_initialized} method at the begining of
   * {@code mgen}. Clients pass the class static initializer {@code <clinit>} as {@code mgen}.
   *
   * @param mgen the method to modify, should be the class static initializer {@code <clinit>}
   * @param classInfo for the given class
   */
  private void addInvokeToClinit(MethodGen24 mgen, ClassInfo classInfo) {

    try {
      List<CodeElement> il = mgen.getInstructionList();
      // point to start of list
      ListIterator<CodeElement> li = il.listIterator();

      for (CodeElement ce : createCodeToMarkClassInitialized(poolBuilder, classInfo)) {
        li.add(ce);
      }
    } catch (DynCompError e) {
      throw e;
    } catch (Throwable t) {
      throw new DynCompError(
          String.format("Error processing %s.%s.%n", mgen.getClassName(), mgen.getName()), t);
    }
  }

  /**
   * Create a class initializer method, if none exists. We need a class initializer to have a place
   * to insert a call to the DynComp Runtime {@code set_class_initialized} method.
   *
   * @param classBuilder for the given class
   * @param classInfo for the given class
   */
  private void createClinit(ClassBuilder classBuilder, ClassInfo classInfo) {

    List<CodeElement> instructions = createCodeToMarkClassInitialized(poolBuilder, classInfo);
    instructions.add(ReturnInstruction.of(TypeKind.VOID)); // need to return!

    classBuilder.withMethod(
        "<clinit>",
        MethodTypeDesc.of(CD_void),
        ACC_STATIC,
        methodBuilder ->
            methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, instructions, null)));
  }

  /**
   * Create the list of instructions for a call to {@code set_class_initialized}. This is used to
   * keep track of when the class is initialized (so we don't look for fields in uninitialized
   * classes).
   *
   * @param poolBuilder for the given class
   * @param classInfo for the given class
   * @return the instruction list
   */
  private List<CodeElement> createCodeToMarkClassInitialized(
      ConstantPoolBuilder poolBuilder, ClassInfo classInfo) {

    List<CodeElement> instructions = new ArrayList<>();

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, "set_class_initialized", MethodTypeDesc.of(CD_void, CD_String));
    instructions.add(buildLDCInstruction(poolBuilder.stringEntry(classInfo.class_name)));
    instructions.add(InvokeInstruction.of(INVOKESTATIC, mre));

    return instructions;
  }

  /**
   * Instruments all the methods in a class to perform dynamic comparability.
   *
   * @param classModel for current class
   * @param classBuilder for current class
   * @param classInfo for the given class
   */
  private void processAllMethods(
      ClassModel classModel, ClassBuilder classBuilder, ClassInfo classInfo) {
    for (MethodModel mm : classModel.methods()) {
      processMethod(mm, classModel, classBuilder, classInfo);
    }
  }

  // The process of instrumenting a method is structured as a hierarchical pipeline: each layer
  // operates at a different level of abstraction and delegates detailed processing to the next
  // layer.
  //
  // processMethod:
  //   checks if need to track values of local variables
  //   adds DCompArgument, if required
  //
  // instrumentMethod:
  //   processes all non-code method elements and copies them to output
  //   calls instrumentCode to process the code attribute
  //
  // instrumentCode:
  //   process code labels
  //   calls instrumentCodeList
  //   copies instrumented code to output
  //
  // instrumentCodeList:
  //   add code to create the tag frame local
  //   calculate the state of the operand stack at each instruction
  //   calls instrumentInstruction
  //
  // instrumentInstruction:
  //   processes and instruments (if required) each individual instruction
  //

  /**
   * Instruments a method to perform dynamic comparability. It adds instrumentation code at the
   * entry and at each return from the method. In addition, it adds instrumentation code to the body
   * of the method to track variable interactions. When the instrumented method is executed, this
   * instrumentation allows the DynComp runtime to group variables into comparability sets. All
   * variables in a comparability set belong to the same "abstract type" of data that the programmer
   * likely intended to represent.
   *
   * @param methodModel for current method
   * @param classModel for current class
   * @param classBuilder for current class
   * @param classInfo for the given class
   */
  private void processMethod(
      MethodModel methodModel,
      ClassModel classModel,
      ClassBuilder classBuilder,
      ClassInfo classInfo) {

    @BinaryName String classname = classInfo.class_name;

    try {
      MethodGen24 mgen = new MethodGen24(methodModel, classname, classBuilder);

      if (debugInstrument.enabled) {
        ClassDesc[] paramTypes = mgen.getParameterTypes();
        String[] paramNames = mgen.getParameterNames();
        LocalVariable[] local_vars = mgen.getOriginalLocalVariables();
        String types = "", names = "", locals = "";

        for (int j = 0; j < paramTypes.length; j++) {
          @FieldDescriptor String paramFD = paramTypes[j].descriptorString();
          types =
              types + daikon.chicory.Instrument24.convertDescriptorToFqBinaryName(paramFD) + " ";
        }
        for (int j = 0; j < paramNames.length; j++) {
          names = names + paramNames[j] + " ";
        }
        for (int j = 0; j < local_vars.length; j++) {
          locals = locals + local_vars[j].name().stringValue() + " ";
        }
        debugInstrument.log("%nMethod = %s%n", mgen.getName());
        debugInstrument.log("paramTypes(%d): %s%n", paramTypes.length, types);
        debugInstrument.log("paramNames(%d): %s%n", paramNames.length, names);
        debugInstrument.log("localvars(%d): %s%n", local_vars.length, locals);
        //         debugInstrument.log("Original code: %s%n", mgen.getMethod().getCode());
        debugInstrument.log("Method Attributes:%n");
        for (Attribute<?> a : methodModel.attributes()) {
          debugInstrument.log("  %s%n", a);
        }
        debugInstrument.log("mgen.getSignature: %s%n", mgen.getSignature());
        MethodTypeDesc mtd = methodModel.methodTypeSymbol();
        debugInstrument.log("mtd.descriptorString: %s%n", mtd.descriptorString());
        debugInstrument.log("mtd.displayDescriptor: %s%n", mtd.displayDescriptor());
      }

      boolean track = false;
      if (!in_jdk) {
        // Note whether we want to track the variables in this method.
        track = should_track(classname, mgen.getName(), methodEntryName(classname, mgen));

        // We cannot track the variables in bridge methods the compiler has synthesized as
        // they are overloaded on return type which normal Java does not support.
        if ((mgen.getAccessFlagsMask() & ACC_BRIDGE) != 0) {
          track = false;
        }

        // If a method has its variables tracked, then we need to mark the class as having some
        // instrumented variables.
        if (track) {
          trackClass = true;
        }

        // If we are tracking variables, make sure the class is public
        int access_flags = classModel.flags().flagsMask();
        if (track && (access_flags & ACC_PUBLIC) == 0) {
          access_flags |= ACC_PUBLIC;
          access_flags &= ~ACC_PROTECTED;
          access_flags &= ~ACC_PRIVATE;
        }
        // reset class access flags in case they have been changed
        classBuilder.withFlags(access_flags);
      } else {
        // If JDK, don't modify class initialization methods.  They can't affect
        // user comparability and there isn't any way to get a second
        // copy of them.
        if (mgen.isClinit()) {
          debugInstrument.log("Copying method: %s%n", mgen.getName());
          debugInstrument.indent();
          outputMethodUnchanged(classBuilder, methodModel, mgen);
          debugInstrument.exdent();
          debugInstrument.log("End of copy%n");
          return;
        }
      }

      debug_transform.log("  Processing method %s, track=%b%n", simplify_method_name(mgen), track);
      debug_transform.indent();

      // `final` because local variables referenced from a lambda expression must be final or
      // effectively final.
      final boolean trackMethod = track;

      // main methods, <clinit> methods and all methods in a JUnit test class
      // need special treatment.  They are not duplicated and they do not have
      // a DCompMarker added to their parameter list.
      boolean addingDcompArg = true;

      if (mgen.isClinit()) {
        classInfo.hasClinit = true;
        addInvokeToClinit(mgen, classInfo);
        addingDcompArg = false;
      }

      if (mgen.isMain()) {
        addingDcompArg = false;
        createMainStub(mgen, classBuilder, classInfo);
      }

      if (classInfo.isJunitTestClass) {
        addingDcompArg = false;
      }

      if (addingDcompArg) {
        // make copy of original method
        debugInstrument.log("Copying method: %s%n", mgen.getName());
        debugInstrument.indent();
        outputMethodUnchanged(classBuilder, methodModel, mgen);
        debugInstrument.exdent();
        debugInstrument.log("End of copy%n");
      }

      MethodTypeDesc mtd = methodModel.methodTypeSymbol();
      if (addingDcompArg) {
        // The original parameterList is immutable, so we need to make a copy.
        List<ClassDesc> paramList = new ArrayList<ClassDesc>(mtd.parameterList());
        paramList.add(dcomp_marker);
        mtd = MethodTypeDesc.of(mtd.returnType(), paramList);
      }
      classBuilder.withMethod(
          methodModel.methodName().stringValue(),
          mtd,
          methodModel.flags().flagsMask(),
          methodBuilder ->
              instrumentMethod(methodBuilder, methodModel, mgen, classInfo, trackMethod));

      debug_transform.exdent();
    } catch (Throwable t) {
      if (debugInstrument.enabled) {
        t.printStackTrace();
      }
      String method = classname + "." + methodModel.methodName().stringValue();
      skip_method(method);
      if (quit_if_error) {
        if (t instanceof DynCompError) {
          throw t;
        }
        throw new DynCompError("Error processing " + method, t);
      } else {
        System.err.printf("Error processing %s: %s%n", method, t);
        System.err.printf("Method is NOT instrumented.%n");
      }
    }
  }

  /**
   * Copy the given method from the input class file to the output class with no changes. Uses
   * {@code copyMethod} to perform the actual copy.
   *
   * @param classBuilder for the output class
   * @param mm MethodModel describes the input method
   * @param mgen describes the output method
   */
  private void outputMethodUnchanged(ClassBuilder classBuilder, MethodModel mm, MethodGen24 mgen) {
    classBuilder.withMethod(
        mm.methodName().stringValue(),
        mm.methodTypeSymbol(),
        mm.flags().flagsMask(),
        methodBuilder -> copyMethod(methodBuilder, mm, mgen));
  }

  /**
   * Copy the given method from the input class file to the output class with no changes.
   *
   * @param methodBuilder for the output class
   * @param methodModel describes the input method
   * @param mgen describes the output method
   */
  private void copyMethod(MethodBuilder methodBuilder, MethodModel methodModel, MethodGen24 mgen) {

    for (MethodElement me : methodModel) {
      debugInstrument.log("MethodElement: %s%n", me);
      switch (me) {
        case CodeModel codeModel ->
            methodBuilder.withCode(
                codeBuilder -> copyCode(codeBuilder, mgen.getInstructionList(), null));

        // copy all other MethodElements to output class (unchanged)
        default -> methodBuilder.with(me);
      }
    }
  }

  /**
   * Copy an instruction list into its corresponding method in the output file. In addition, if the
   * {@code newLocals} list is not empty, create new local variables in the output method.
   *
   * @param codeBuilder for the given method's code
   * @param instructions instruction list to copy
   * @param newLocals method scope locals to define; may be null if none
   */
  private void copyCode(
      CodeBuilder codeBuilder,
      List<CodeElement> instructions,
      @Nullable List<myLocalVariable> newLocals) {

    if (newLocals != null) {
      for (myLocalVariable lv : newLocals) {
        codeBuilder.localVariable(
            lv.slot(),
            lv.name(),
            lv.descriptor(),
            codeBuilder.startLabel(),
            codeBuilder.endLabel());
      }
    }

    for (CodeElement ce : instructions) {
      debugInstrument.log("CodeElement: %s%n", ce);
      codeBuilder.with(ce);
    }
  }

  /**
   * Instrument the specified method for dynamic comparability.
   *
   * @param methodBuilder for the given method
   * @param methodModel for the given method
   * @param mgen describes the given method
   * @param classInfo for the current class
   * @param trackMethod true iff we need to track the daikon variables in this method
   */
  private void instrumentMethod(
      MethodBuilder methodBuilder,
      MethodModel methodModel,
      MethodGen24 mgen,
      ClassInfo classInfo,
      boolean trackMethod) {

    try {
      boolean codeModelSeen = false;
      for (MethodElement me : methodModel) {
        debugInstrument.log("MethodElement: %s%n", me);
        switch (me) {
          case CodeModel codeModel -> {
            codeModelSeen = true;
            methodBuilder.withCode(
                codeBuilder ->
                    instrumentCode(codeBuilder, codeModel, null, mgen, classInfo, trackMethod));
          }
          case RuntimeVisibleAnnotationsAttribute rvaa -> {
            // We do not want to copy the @HotSpotIntrinsicCandidate annotations from
            // the original method to our instrumented method as the signature will
            // not match anything in the JVM's list.  This won't cause an execution
            // problem but will produce a number of warnings.
            // JDK 11: @HotSpotIntrinsicCandidate
            // JDK 17: @IntrinsicCandidate
            boolean output = true;
            for (final Annotation item : rvaa.annotations()) {
              String description = item.className().stringValue();
              if (description.endsWith("IntrinsicCandidate;")) {
                output = false;
                debugInstrument.log("Annotation not copied: %s%n", description);
              }
            }
            if (output) {
              methodBuilder.with(me);
            }
          }
          // copy all other MethodElements to output class (unchanged)
          default -> methodBuilder.with(me);
        }
      }
      if (!codeModelSeen) {
        debugInstrument.log("No CodeModel for method: %s%n", mgen.getName());
        if ((mgen.getAccessFlagsMask() & ACC_NATIVE) != 0) {
          // We need to build our wrapper method for a call to native code.
          methodBuilder.withCode(
              codeBuilder -> instrumentCode(codeBuilder, null, null, mgen, classInfo, trackMethod));

          // Turn off the native flag in wrapper method.
          methodBuilder.withFlags(mgen.getAccessFlagsMask() & ~ACC_NATIVE);
        } else {
          // Interface and/or Abstract; do nothing.
        }
      }
    } catch (DynCompError e) {
      throw e;
    } catch (Throwable t) {
      throw new DynCompError("Error processing " + classInfo.class_name + "." + mgen.getName(), t);
    }
  }

  /**
   * Generate instrumentation code for the given method. This includes reading in and processing the
   * original instruction list, calling {@code insertInstrumentationCode} to add the instrumentation
   * code, and then copying the modified instruction list to the output method while updating the
   * code labels, if needed. In addition, if the {@code newLocals} list is not empty, add them to
   * the {@code localsTable}.
   *
   * <p>If DCInstrument24 generated the method or if it is a native method, then {@code codeModel}
   * == null.
   *
   * @param codeBuilder for the given method's code
   * @param codeModel for the input method's code; may be null
   * @param newLocals method scope locals to define; may be null if none
   * @param mgen describes the output method
   * @param classInfo for the current class
   * @param trackMethod true iff we need to track the daikon variables in this method
   */
  private void instrumentCode(
      CodeBuilder codeBuilder,
      @Nullable CodeModel codeModel,
      @Nullable List<myLocalVariable> newLocals,
      MethodGen24 mgen,
      ClassInfo classInfo,
      boolean trackMethod) {

    // method_info_index is not used at this point in DCInstrument
    MethodGen24.MInfo24 minfo = new MethodGen24.MInfo24(0, mgen.getMaxLocals(), codeBuilder);
    debugInstrument.log("nextLocalIndex: %d%n", minfo.nextLocalIndex);

    if (newLocals != null) {
      for (myLocalVariable lv : newLocals) {
        mgen.localsTable.add(
            LocalVariable.of(
                lv.slot(),
                lv.name(),
                lv.descriptor(),
                codeBuilder.startLabel(),
                codeBuilder.endLabel()));
      }
      mgen.setOriginalLocalVariables(
          mgen.localsTable.toArray(new LocalVariable[mgen.localsTable.size()]));
    }

    // Clean up parameter names and add in any unused parameters that the Java compiler has
    // optimized out.
    if (mgen.fixLocals(minfo)) {
      // localsTable was changed
      debugInstrument.log("Revised LocalVariableTable:%n");
      for (LocalVariable lv : mgen.localsTable) {
        debugInstrument.log("  %s%n", lv);
      }
    }

    if (debugInstrument.enabled) {
      String[] paramNames = mgen.getParameterNames();
      debugInstrument.log("paramNames: %s%n", paramNames.length);
      for (String paramName : paramNames) {
        debugInstrument.log("param name: %s%n", paramName);
      }
    }

    debugInstrument.log("nextLocalIndex: %d%n", minfo.nextLocalIndex);

    // If the method is native
    if ((mgen.getAccessFlagsMask() & ACC_NATIVE) != 0) {

      debugInstrument.log("Native Method%n");
      // Create Java code that cleans up the tag stack and calls the real native method.
      fix_native(mgen);

      // Add the DCompMarker parameter to distinguish our version.
      add_dcomp_param(mgen, minfo);

      // Copy the modified local variable table to the output class.
      debugInstrument.log("LocalVariableTable:%n");
      for (LocalVariable lv : mgen.localsTable) {
        codeBuilder.localVariable(
            lv.slot(), lv.name().stringValue(), lv.typeSymbol(), lv.startScope(), lv.endScope());
        @FieldDescriptor String lvFD = lv.typeSymbol().descriptorString();
        debugInstrument.log(
            "  %s : %s%n", lv, daikon.chicory.Instrument24.convertDescriptorToFqBinaryName(lvFD));
      }

      // Copy the modified instruction list to the output class.
      for (CodeElement ce : mgen.getInstructionList()) {
        debugInstrument.log("CodeElement: %s%n", ce);
        codeBuilder.with(ce);
      }

    } else { // normal method

      if (!classInfo.isJunitTestClass) {
        // Add the DCompMarker parameter to distinguish our version.
        add_dcomp_param(mgen, minfo);
      }

      if (debugInstrument.enabled) {
        System.out.printf("nextLocalIndex: %d%n", minfo.nextLocalIndex);
        System.out.printf("LocalVariableTable:%n");
        for (LocalVariable lv : mgen.localsTable) {
          System.out.printf("  %s%n", lv);
        }
        System.out.println(
            "instrumentCode - param types: " + Arrays.toString(mgen.getParameterTypes()));
        System.out.printf("length: %d%n", mgen.getParameterTypes().length);
      }

      int paramCount = (mgen.isStatic() ? 0 : 1) + mgen.getParameterTypes().length;
      debugInstrument.log("paramCount: %d%n", paramCount);

      // Create a MethodInfo that describes this method's parameters
      // and exit line numbers (information not available via reflection)
      // and add it to the list for this class.
      MethodInfo mi = null;
      if (trackMethod && !in_jdk) {
        @SuppressWarnings("nullness:assignment") // the method exists
        @NonNull MethodInfo miTmp = create_method_info_if_instrumented(classInfo, mgen);
        mi = miTmp;
        classInfo.method_infos.add(mi);
        DCRuntime.methods.add(mi);
      }

      @SuppressWarnings("JdkObsolete")
      List<CodeElement> codeList = new LinkedList<>();

      // no codeModel if DCInstrument24 generated the method
      if (codeModel != null) {
        debugInstrument.log("Code Attributes:%n");
        for (Attribute<?> a : codeModel.attributes()) {
          debugInstrument.log("  %s%n", a);
        }
      }

      // Create the local to store the tag frame for this method
      tagFrameLocal = createTagFrameLocal(mgen, minfo);

      debugInstrument.log("nextLocalIndex: %d%n", minfo.nextLocalIndex);
      debugInstrument.log("maxlocals: %d%n", mgen.getMaxLocals());

      // The localsTable was initialized in the MethodGen24 constructor. Here we initialize the
      // codeList. We also remove the local variable type records. Some instrumentation changes
      // require these to be updated, but it should be safe to just delete them since the
      // LocalVariableTypeTable is optional and really only of use to a debugger. We also save the
      // CodeModel label at the start of the byte codes, if there is one. If there isn't, that is
      // okay as it means the original code did not reference byte code offset 0 so inserting our
      // instrumentation code at that point will not cause a problem.
      @SuppressWarnings("nullness:assignment") // can't have gotten here if CodeAttribute is null
      @NonNull CodeAttribute ca = mgen.getCodeAttribute();
      for (CodeElement ce : mgen.getInstructionList()) {
        debugInstrument.log("CodeElement: %s%n", ce);
        switch (ce) {
          case LocalVariable lv -> {} // we have alreay processed these
          case LocalVariableType lvt -> {} // we can discard local variable types
          case LabelTarget l -> {
            if (ca.labelToBci(l.label()) == 0) {
              oldStartLabel = l.label();
            }
            codeList.add(ce);
          }
          default -> codeList.add(ce); // save all other elements
        }
      }

      // Create newStartLabel now so instrumentCodeList can use it.
      newStartLabel = codeBuilder.newLabel();

      // Instrument the method
      instrumentCodeList(codeModel, mgen, minfo, codeList);

      if (trackMethod && !in_jdk) {
        assert mi != null : "@AssumeAssertion(nullness): mi was assigned under same conditions";
        add_enter(mgen, minfo, codeList, DCRuntime.methods.size() - 1);
        add_exit(mgen, mi, minfo, codeList, DCRuntime.methods.size() - 1);
      }

      // Copy the modified local variable table to the output class.
      debugInstrument.log("LocalVariableTable:%n");
      for (LocalVariable lv : mgen.localsTable) {
        codeBuilder.localVariable(
            lv.slot(), lv.name().stringValue(), lv.typeSymbol(), lv.startScope(), lv.endScope());
        @FieldDescriptor String lvFD = lv.typeSymbol().descriptorString();
        debugInstrument.log(
            "  %s : %s%n", lv, daikon.chicory.Instrument24.convertDescriptorToFqBinaryName(lvFD));
      }

      // Copy the modified instruction list to the output class.
      ListIterator<CodeElement> li = codeList.listIterator();
      CodeElement ce;
      while (li.hasNext()) {
        if (li.nextIndex() == newStartIndex) {
          codeBuilder.labelBinding(newStartLabel);
          debugInstrument.log("Label: %s%n", newStartLabel);
        }
        ce = li.next();
        // If this instruction references a Label, we need to see if it is the oldStartLabel
        // and, if so, replace the target with the newStartLabel.
        ce = retargetStartLabel(ce);
        debugInstrument.log("CodeElement: %s%n", ce);
        codeBuilder.with(ce);
      }

      // generateExceptionHandlerCode returns null if there isn't one.
      List<CodeElement> handlerCode = generateExceptionHandlerCode(mgen);
      if (handlerCode != null) {
        Label handlerLabel = codeBuilder.newBoundLabel();
        for (CodeElement ceh : handlerCode) {
          codeBuilder.with(ceh);
        }
        // Using handlerLabel for the end of the try region is technically one instruction
        // too many, but it shouldn't matter and is easily available.
        codeBuilder.exceptionCatch(newStartLabel, handlerLabel, handlerLabel, CD_Throwable);
      }
    }
  }

  /**
   * Returns true if the specified classname.methodName is the root of JUnit startup code.
   *
   * @param classname class containing the given method
   * @param methodName method to be checked
   * @return true if the given method is a JUnit trigger
   */
  boolean isJunitTrigger(String classname, @Identifier String methodName) {
    if (classname.contains("JUnitCommandLineParseResult") && methodName.equals("parse")) {
      // JUnit 4
      return true;
    }
    if (classname.contains("EngineDiscoveryRequestResolution") && methodName.equals("resolve")) {
      // JUnit 5
      return true;
    }
    return false;
  }

  // ///////////////////////////////////////////////////////////////////////////
  // General Java Runtime instrumentation strategy:
  //
  // It is a bit of a misnomer, but the Daikon code and documentation uses the term JDK to refer
  // to the Java Runtime Environment class libraries. In Java 8 and earlier, they were usually found
  // in {@code <your java installation>/jre/lib/rt.jar}. For these versions of Java, we
  // pre-instrumented the entire rt.jar.
  //
  // In Java 9 and later, the Java Runtime classes have been divided into modules that are
  // usually found in: {@code <your java installation>/jmods/*.jmod}.
  //
  // With the conversion to modules for Java 9 and beyond, we have elected to pre-instrument only
  // java.base.jmod and instrument all other Java Runtime (aka JDK) classes dynamically as they are
  // loaded.
  //
  // Post Java 8 there are increased security checks when loading JDK classes. In particular, the
  // core classes contained in the java.base module may not reference anything outside of java.base.
  // This means we cannot pre-instrument classes in the same manner as was done for Java 8 as this
  // would introduce external references to the DynComp runtime (DCRuntime.java).
  //
  // However, we can get around this restriction in the following manner: We create a shadow
  // DynComp runtime called java.lang.DCRuntime that contains all the public methods of
  // daikon.dcomp.DCRuntime, but with method bodies that contain only a return statement. We
  // pre-instrument java.base the same as we would for JDK 8, but change all references to
  // daikon.dcomp.DCRuntime to refer to java.lang.DCRuntime instead, and thus pass the security load
  // test. During DynComp initialization (in Premain) we use java.lang.instrument.redefineClasses to
  // replace the dummy java.lang.DCRuntime with a version where each method calls the corresponding
  // method in daikon.dcomp.DCRuntime. The Java runtime does not enforce the security check in this
  // case.

  /**
   * Instruments a JDK class to perform dynamic comparability and returns the new class definition.
   * A second version of each method in the class is created which is instrumented for
   * comparability.
   *
   * @return the modified JavaClass
   */
  public byte @Nullable [] instrument_jdk_class(ClassInfo classInfo) {

    @BinaryName String classname = classInfo.class_name;

    // Don't know where I got this idea.  They are executed.  Don't remember why
    // adding dcomp marker causes problems.
    // Don't instrument annotations.  They aren't executed and adding
    // the marker argument causes subtle errors
    if (classModel.flags().has(AccessFlag.ANNOTATION)) {
      debug_transform.log("Not instrumenting annotation %s%n", classname);
      // Return class file unmodified.
      return classFile.transformClass(classModel, ClassTransform.ACCEPT_ALL);
    }

    int i = classname.lastIndexOf('.');
    if (i > 0) {
      // Don't instrument problem packages.
      // See Premain.java for a list and explanations.
      String packageName = classname.substring(0, i);
      if (Premain.problem_packages.contains(packageName)) {
        debug_transform.log("Skipping problem package %s%n", packageName);
        // Return class file unmodified.
        return classFile.transformClass(classModel, ClassTransform.ACCEPT_ALL);
      }
    }

    if (Runtime.isJava9orLater()) {
      // Don't instrument problem classes.
      // See Premain.java for a list and explanations.
      if (Premain.problem_classes.contains(classname)) {
        debug_transform.log("Skipping problem class %s%n", classname);
        // Return class file unmodified.
        return classFile.transformClass(classModel, ClassTransform.ACCEPT_ALL);
      }
      dcompRuntimeClassName = "java.lang.DCRuntime";
    }

    try {
      return classFile.build(
          classModel.thisClass().asSymbol(),
          classBuilder -> instrumentClass(classBuilder, classModel, classInfo));
    } catch (Throwable t) {
      if (debugInstrument.enabled) {
        t.printStackTrace();
      }
      System.err.printf(
          "DynComp warning: Class %s is being skipped due to the following:%n",
          classInfo.class_name);
      System.err.printf("%s.%n", t);
      // Return class file unmodified.
      return classFile.transformClass(classModel, ClassTransform.ACCEPT_ALL);
    }
  }

  /**
   * Instrument the specified method for dynamic comparability.
   *
   * @param codeModel for the method's code
   * @param mgen describes the given method
   * @param minfo for the given method's code
   * @param instructions instruction list for method
   */
  @RequiresNonNull("newStartLabel")
  private void instrumentCodeList(
      @Nullable CodeModel codeModel,
      MethodGen24 mgen,
      MethodGen24.MInfo24 minfo,
      List<CodeElement> instructions) {

    List<CodeElement> newCode = createTagFrame(mgen);

    // The start of the list of CodeElements looks as follows:
    //   LocalVariable declarations (if any)
    //   Label for start of code (if present)
    //   LineNumber for start of code (if present)
    //   <the actual code for the method>
    //
    // We want to insert the tag_frame setup code after the LocalVariables (if any) and after the
    // inital label (if present), but before any LineNumber or Instruction.
    ListIterator<CodeElement> li = instructions.listIterator();
    CodeElement inst;
    try {
      while (li.hasNext()) {
        inst = li.next();
        if ((inst instanceof LineNumber) || (inst instanceof Instruction)) {
          break;
        }
      }

      // Insert the TagFrame code before the LineNumber or Instruction we just located.
      // Back up the iterator to point to just before `inst`, then copy the newCode.
      li.previous();
      for (CodeElement ce : newCode) {
        li.add(ce);
      }

      // We want to remember the current location in the instruction list. It is where a call to the
      // DynComp runtime enter routine should be inserted, if we are tracking this method.
      // This will also be used to indicate where the newStartLabel should be defined.
      // Finally, we will also return to this location for code instrumentation after calculating
      // the operand stack values.
      newStartIndex = li.nextIndex();

      // The next section of code calculates the operand stack value(s) for the current method.
      if (debugOperandStack) {
        System.out.printf("%nStarting operand stack calculation%n");
      }

      // Build a map of labels to instruction list offsets
      labelIndexMap = new HashMap<>();
      li = instructions.listIterator();
      while (li.hasNext()) {
        inst = li.next();
        if (inst instanceof LabelTarget lt) {
          if (debugOperandStack) {
            System.out.println("label target: " + lt.label() + ", index: " + li.previousIndex());
          }
          // remember where this label is located within the instruction list
          labelIndexMap.put(lt.label(), li.previousIndex());
        }
      }
      if (oldStartLabel != null) {
        // change offset of oldStartLabel to where we will define newStartLabel
        labelIndexMap.put(oldStartLabel, newStartIndex);
      }
      labelIndexMap.put(newStartLabel, newStartIndex);

      // Create an array to hold the calculated operand stack
      // prior to each byte code instruction.
      stacks = new OperandStack24[instructions.size()];

      // Create an array containing the type of each local variable.
      // This will be indexed by the local variable's slot number. Note that a
      // variable of type long or double takes two slots; hence, there may
      // be unused entries in the locals table.
      locals = new ClassDesc[mgen.getMaxLocals()];
      // UNDONE: Should we init locals for 'this' and params only?
      for (final LocalVariable lv : mgen.localsTable) {
        // System.out.printf("local(%d): %s%n", lv.slot(), lv.typeSymbol());
        locals[lv.slot()] = lv.typeSymbol();
      }

      labelOperandStackMap = new HashMap<>();

      // Create a worklist of instruction locations and operand stacks.

      // Create a work item for start of user's code.
      if (oldStartLabel != null) {
        addLabelToWorklist(oldStartLabel, new OperandStack24(mgen.getMaxStack()));
      } else {
        addLabelToWorklist(newStartLabel, new OperandStack24(mgen.getMaxStack()));
      }

      if (codeModel != null) {
        // Create a work item for each exception handler.
        for (ExceptionCatch ec : codeModel.exceptionHandlers()) {
          Label l = ec.handler();
          Optional<ClassEntry> catchType = ec.catchType();
          OperandStack24 temp = new OperandStack24(mgen.getMaxStack());
          if (catchType.isPresent()) {
            temp.push(catchType.get().asSymbol());
          } else {
            temp.push(CD_Throwable);
          }
          addLabelToWorklist(l, temp);
        }
      }

      WorkItem item;
      OperandStack24 stack;
      int instIndex;
      while (!worklist.isEmpty()) {
        item = worklist.first();
        worklist.remove(item);
        if (debugOperandStack) {
          System.out.println(
              "pull from worklist: " + item.instructionIndex() + ", stack: " + item.stack());
        }
        li = instructions.listIterator(item.instructionIndex());
        stack = item.stack();
        boolean proceed = true;
        while (proceed) {
          if (!li.hasNext()) {
            throw new DynCompError("error in instruction list");
          }
          instIndex = li.nextIndex();
          inst = li.next();
          if (debugOperandStack) {
            System.out.println("instIndex: " + instIndex);
            System.out.println("Operand Stack in: " + stack);
          }
          proceed = CalcStack24.simulateCodeElement(mgen, minfo, inst, instIndex, stack);
          if (debugOperandStack) {
            System.out.println("Operand Stack out: " + stack);
            System.out.println("proceed: " + proceed);
          }
        }
      }

      if (debugOperandStack) {
        System.out.printf("%nStarting instruction instrumentation%n");
      }
      // set list iterator to start of the user instructions
      li = instructions.listIterator(newStartIndex);

      // We set instIndex here and increment each time through loop.
      // This should match the index into stacks we used above.
      instIndex = li.nextIndex();
      while (li.hasNext()) {
        inst = li.next();

        if (debugOperandStack) {
          System.out.println("code element in: " + inst);
          System.out.println("current stack: " + stacks[instIndex]);
        }
        // Get the translation for this instruction (if any)
        List<CodeElement> new_il = instrumentInstruction(mgen, minfo, inst, stacks[instIndex]);
        if (new_il != null) {
          li.remove(); // remove the instruction we instrumented
          for (CodeElement ce : new_il) {
            if (debugOperandStack) {
              System.out.println("code element out: " + ce);
            }
            li.add(ce);
          }
        }
        instIndex++;
      }
    } catch (DynCompError e) {
      throw e;
    } catch (Throwable t) {
      throw new DynCompError(
          String.format("Error processing %s.%s.%n", mgen.getClassName(), mgen.getName()), t);
    }
  }

  /**
   * Create a worklist item.
   *
   * @param target label where to start operand stack simulation
   * @param stack state of operand stack at target
   */
  protected static void addLabelToWorklist(Label target, OperandStack24 stack) {
    OperandStack24 existing = labelOperandStackMap.get(target);
    if (existing == null) {
      if (debugOperandStack) {
        System.out.println(
            "push to worklist: " + target + ", " + labelIndexMap.get(target) + ", stack: " + stack);
      }
      labelOperandStackMap.put(target, stack.getClone());
      @SuppressWarnings("nullness:unboxing.of.nullable")
      int indexInCodeList = labelIndexMap.get(target);
      worklist.add(new WorkItem(indexInCodeList, stack.getClone()));
    } else {
      // will throw if stacks don't match
      verifyOperandStackMatches(target, existing, stack);
      // stacks match, don't add duplicate to worklist
      if (debugOperandStack) {
        System.out.println(
            "duplicate worklist item: "
                + target
                + ", "
                + labelIndexMap.get(target)
                + ", stack: "
                + stack);
      }
    }
  }

  /**
   * Verify that the operand stacks match at a label.
   *
   * @param target label where control flow merges
   * @param existing state of operand stack at target
   * @param current state of operand stack at transfer to target
   */
  protected static void verifyOperandStackMatches(
      Label target, OperandStack24 existing, OperandStack24 current) {
    if (existing.equals(current)) {
      if (debugOperandStack) {
        System.out.println("operand stacks match at: " + target);
      }
      return;
    }
    // stacks don't match
    System.err.flush();
    System.out.flush();
    System.out.println("operand stacks do not match at label:" + target);
    System.out.println("existing stack: " + existing);
    System.out.println("current stack: " + current);
    System.out.flush();
    throw new DynCompError("operand stacks do not match at label:" + target);
  }

  /**
   * Adds the method name and containing class name to {@code skip_methods}, the list of
   * uninstrumented methods.
   *
   * @param m method to add to skipped_methods list
   */
  void skip_method(String m) {
    skipped_methods.add(m);
  }

  /**
   * Returns the list of uninstrumented methods. (Note: instrument_jdk_class() needs to have been
   * called first.)
   */
  public List<String> get_skipped_methods() {
    return skipped_methods;
  }

  /**
   * In {@link #instrumentCode} we build a try/catch block around the entire method. Here we
   * generate the exception handler code so that if an exception occurs when the instrumented method
   * is executed, the tag stack is cleaned up and the exception is rethrown.
   *
   * @param mgen method to add exception handler
   * @return code list for handler, or null if method should not have a handler
   */
  public @Nullable List<CodeElement> generateExceptionHandlerCode(MethodGen24 mgen) {

    if (mgen.getName().equals("main")) {
      return null;
    }
    // <init> methods (constructors) are problematic
    // for adding a whole-method exception handler.  The start of
    // the exception handler should be after the primary object is
    // initialized - but this is hard to determine without a full
    // analysis of the code.  Hence, we just skip these methods.
    if (!mgen.isStatic() && mgen.isConstructor()) {
      return null;
    }

    List<CodeElement> instructions = new ArrayList<>();

    instructions.add(StackInstruction.of(DUP));
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, "exception_exit", MethodTypeDesc.of(CD_void, CD_Object));
    instructions.add(InvokeInstruction.of(INVOKESTATIC, mre));
    instructions.add(ThrowInstruction.of());
    return instructions;
  }

  /**
   * Adds a call to DCRuntime.enter at the beginning of the method.
   *
   * @param mgen method to modify
   * @param minfo for the given method's code
   * @param instructions instruction list for method
   * @param method_info_index index for MethodInfo
   */
  private void add_enter(
      MethodGen24 mgen,
      MethodGen24.MInfo24 minfo,
      List<CodeElement> instructions,
      int method_info_index) {
    List<CodeElement> newCode = callEnterOrExit(mgen, minfo, method_info_index, "enter", -1);
    instructions.addAll(newStartIndex, newCode);
    // Update the newStartIndex to account for the instrumentation code we just added.
    newStartIndex += newCode.size();
  }

  /**
   * Creates the local used to store the tag frame and returns it.
   *
   * @param mgen method to modify
   * @param minfo for the given method's code
   * @return LocalVariable for the tag_frame local
   */
  LocalVariable createTagFrameLocal(MethodGen24 mgen, MethodGen24.MInfo24 minfo) {
    return StackMapUtils24.addNewSpecialLocal(
        mgen, minfo, "dcomp_tag_frame$5a", objectArrayCD, false);
  }

  /**
   * Generates the code to create the tag frame for this method and store it in tagFrameLocal. This
   * needs to be before the call to DCRuntime.enter (since it is passed to that method).
   *
   * @param mgen describes the given method
   * @return instruction list for tag_frame setup code
   */
  private List<CodeElement> createTagFrame(MethodGen24 mgen) {

    ClassDesc paramTypes[] = mgen.getParameterTypes();

    // Determine the offset of the first argument in the frame.
    int offset = 1;
    if (mgen.isStatic()) {
      offset = 0;
    }

    // allocate an extra slot to save the tag frame depth for debugging
    int frame_size = mgen.getMaxLocals() + 2;

    // unsigned byte max = 255.  minus the character '0' (decimal 48)
    // Largest frame size noted so far is 123.
    if (frame_size > 206) {
      throw new DynCompError("method too large to instrument: " + mgen.getName());
    }
    String params = Character.toString((char) (frame_size + '0'));
    // Character.forDigit (frame_size, Character.MAX_RADIX);
    List<Integer> paramList = new ArrayList<>();
    for (ClassDesc paramType : paramTypes) {
      if (paramType.isPrimitive()) {
        paramList.add(offset);
      }
      offset += TypeKind.from(paramType).slotSize();
    }
    for (int ii = paramList.size() - 1; ii >= 0; ii--) {
      char tmpChar = (char) (paramList.get(ii) + '0');
      params += tmpChar;
      // Character.forDigit (paramList.get(ii), Character.MAX_RADIX);
    }

    // Create code to create and init the tag_frame and store the result in tagFrameLocal.
    List<CodeElement> instructions = new ArrayList<>();

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, "create_tag_frame", MethodTypeDesc.of(objectArrayCD, CD_String));
    instructions.add(buildLDCInstruction(poolBuilder.stringEntry(params)));
    instructions.add(InvokeInstruction.of(INVOKESTATIC, mre));
    instructions.add(StoreInstruction.of(TypeKind.REFERENCE, tagFrameLocal.slot()));

    debugInstrument.log("Store Tag frame local at index %d%n", tagFrameLocal.slot());

    return instructions;
  }

  /**
   * Pushes the object, method info index, parameters, and return value on the stack and calls the
   * specified method (normally {@code enter()} or {@code exit}) in DCRuntime. The parameters are
   * passed as an array of objects.
   *
   * @param mgen method to modify
   * @param minfo for the given method's code
   * @param method_info_index index for MethodInfo
   * @param enterOrExit the method to invoke: "enter" or "exit"
   * @param line source line number if type is exit
   * @return instruction list for the enter or exit code
   */
  private List<CodeElement> callEnterOrExit(
      MethodGen24 mgen,
      MethodGen24.MInfo24 minfo,
      int method_info_index,
      String enterOrExit,
      int line) {

    List<CodeElement> instructions = new ArrayList<>();
    ClassDesc paramTypes[] = mgen.getParameterTypes();

    // Push the tag frame
    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, tagFrameLocal.slot()));

    // Push the object.  Push null if this is a static method or a constructor.
    if (mgen.isStatic() || (enterOrExit.equals("enter") && mgen.isConstructor())) {
      instructions.add(ConstantInstruction.ofIntrinsic(ACONST_NULL));
    } else { // must be an instance method
      instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 0));
    }

    // The offset of the first parameter.
    int param_offset = mgen.isStatic() ? 0 : 1;

    // Push the MethodInfo index
    instructions.add(loadIntegerConstant(method_info_index));

    // Create an array of objects with elements for each parameter.
    instructions.add(loadIntegerConstant(paramTypes.length));
    instructions.add(NewReferenceArrayInstruction.of(poolBuilder.classEntry(CD_Object)));

    // Put each argument into the array.
    int param_index = param_offset;
    for (int ii = 0; ii < paramTypes.length; ii++) {
      instructions.add(StackInstruction.of(DUP));
      instructions.add(loadIntegerConstant(ii));
      ClassDesc at = paramTypes[ii];
      if (at.isPrimitive()) {
        instructions.add(ConstantInstruction.ofIntrinsic(ACONST_NULL));
      } else { // it's a reference of some sort
        instructions.add(LoadInstruction.of(TypeKind.REFERENCE, param_index));
      }
      instructions.add(ArrayStoreInstruction.of(AASTORE));
      param_index += TypeKind.from(at).slotSize();
    }

    // If this is an exit, push the return value and line number.
    // The return value is stored in the local "return__$trace2_val".
    // If the return value is a primitive, push a null.
    if (enterOrExit.equals("exit")) {
      ClassDesc returnType = mgen.getReturnType();
      if (returnType.equals(CD_void)) {
        instructions.add(ConstantInstruction.ofIntrinsic(ACONST_NULL));
      } else {
        LocalVariable return_local = getReturnLocal(mgen, returnType, minfo);
        if (returnType.isPrimitive()) {
          instructions.add(ConstantInstruction.ofIntrinsic(ACONST_NULL));
        } else {
          instructions.add(LoadInstruction.of(TypeKind.REFERENCE, return_local.slot()));
        }
      }
      // push line number
      instructions.add(loadIntegerConstant(line));
    }

    MethodTypeDesc methodArgs;
    // Call the specified method.
    if (enterOrExit.equals("exit")) {
      methodArgs =
          MethodTypeDesc.of(
              CD_void, objectArrayCD, CD_Object, CD_int, objectArrayCD, CD_Object, CD_int);
    } else {
      methodArgs = MethodTypeDesc.of(CD_void, objectArrayCD, CD_Object, CD_int, objectArrayCD);
    }
    MethodRefEntry mre = poolBuilder.methodRefEntry(runtimeCD, enterOrExit, methodArgs);
    instructions.add(InvokeInstruction.of(INVOKESTATIC, mre));

    return instructions;
  }

  /**
   * Transforms one instruction to track comparability. Returns a list of instructions that replaces
   * the specified instruction. Returns null if the instruction should not be replaced.
   *
   * @param mgen method to modify
   * @param minfo for the given method's code
   * @param ce instruction to be instrumented
   * @param stack current contents of the operand stack
   * @return instrumentation for inst, or null if none
   */
  @Nullable List<CodeElement> instrumentInstruction(
      MethodGen24 mgen, MethodGen24.MInfo24 minfo, CodeElement ce, OperandStack24 stack) {

    switch (ce) {
      case Instruction inst -> {
        switch (inst.opcode()) {

          // Replace the object comparison instructions with a call to
          // DCRuntime.object_eq or DCRuntime.object_ne.  Those methods
          // return a boolean which is used in a ifeq/ifne instruction.
          case IF_ACMPEQ:
            return object_comparison((BranchInstruction) inst, "object_eq", IFNE);
          case IF_ACMPNE:
            return object_comparison((BranchInstruction) inst, "object_ne", IFNE);

          // These instructions compare the integer on the top of the stack
          // to zero.  Nothing is made comparable by this, so we need only
          // discard the tag on the top of the stack.
          case IFEQ:
          case IFNE:
          case IFLT:
          case IFGE:
          case IFGT:
          case IFLE:
            {
              return discard_tag_code(inst, 1);
            }

          // Instanceof pushes either 0 or 1 on the stack depending on whether
          // the object on top of stack is of the specified type.  The DynComp runtime will push a
          // new, unique
          // tag for a constant, since nothing is made comparable by this.
          case INSTANCEOF:
            return build_il(dcr_call("push_const", CD_void, noArgsSig), inst);

          // Duplicates the item on the top of stack.  If the value on the
          // top of the stack is a primitive, we need to do the same on the
          // tag stack.  Otherwise, we need do nothing.
          case DUP:
            return dup_tag(inst, stack);

          // Duplicates the item on the top of the stack and inserts it 2
          // values down in the stack.  If the value at the top of the stack
          // is not a primitive, there is nothing to do.  If the second
          // value is not a primitive, then we need only to insert the duped
          // value down 1 on the tag stack (which contains only primitives).
          case DUP_X1:
            return dup_x1_tag(inst, stack);

          // Duplicate the category 1 item on the top of the stack and insert it either
          // two or three items down in the stack.
          case DUP_X2:
            return dup_x2_tag(inst, stack);

          // Duplicate either one category 2 item or two category 1 items.
          case DUP2:
            return dup2_tag(inst, stack);

          // Duplicate either the top 2 category 1 items or a single
          // category 2 item and insert it 2 or 3 values down on the
          // stack.
          case DUP2_X1:
            return dup2_x1_tag(inst, stack);

          // Duplicate the top one or two items and insert them two, three, or four values down.
          case DUP2_X2:
            return dup2_x2_tag(inst, stack);

          // Pop a category 1 item from the top of the stack.  We want to discard
          // the top of the tag stack iff the item on the top of the stack is a
          // primitive.
          case POP:
            return pop_tag(inst, stack);

          // Pops either the top 2 category 1 items or a single category 2 item
          // from the top of the stack.  We must do the same to the tag stack
          // if the values are primitives.
          case POP2:
            return pop2_tag(inst, stack);

          // Swaps the two category 1 items on the top of the stack.  We need
          // to swap the top of the tag stack if the two top elements on the
          // real stack are primitives.
          case SWAP:
            return swap_tag(inst, stack);

          case IF_ICMPEQ:
          case IF_ICMPGE:
          case IF_ICMPGT:
          case IF_ICMPLE:
          case IF_ICMPLT:
          case IF_ICMPNE:
            {
              return build_il(dcr_call("cmp_op", CD_void, noArgsSig), inst);
            }

          case GETFIELD:
          case PUTFIELD:
          case GETSTATIC:
          case PUTSTATIC:
            {
              return load_store_field(mgen, minfo, ((FieldInstruction) inst));
            }

          case DLOAD:
          case DLOAD_0:
          case DLOAD_1:
          case DLOAD_2:
          case DLOAD_3:
          case FLOAD:
          case FLOAD_0:
          case FLOAD_1:
          case FLOAD_2:
          case FLOAD_3:
          case ILOAD:
          case ILOAD_0:
          case ILOAD_1:
          case ILOAD_2:
          case ILOAD_3:
          case LLOAD:
          case LLOAD_0:
          case LLOAD_1:
          case LLOAD_2:
          case LLOAD_3:
            {
              return load_local((LoadInstruction) inst, tagFrameLocal, "push_local_tag");
            }

          case DSTORE:
          case DSTORE_0:
          case DSTORE_1:
          case DSTORE_2:
          case DSTORE_3:
          case FSTORE:
          case FSTORE_0:
          case FSTORE_1:
          case FSTORE_2:
          case FSTORE_3:
          case ISTORE:
          case ISTORE_0:
          case ISTORE_1:
          case ISTORE_2:
          case ISTORE_3:
          case LSTORE:
          case LSTORE_0:
          case LSTORE_1:
          case LSTORE_2:
          case LSTORE_3:
            {
              return store_local((StoreInstruction) inst, tagFrameLocal, "pop_local_tag");
            }

          // Adjusts the tag stack for load constant opcodes. If the constant is a primitive, pushes
          // its tag on the tag stack. If the constant is a reference (string, class), does nothing.
          case LDC:
          case LDC_W:
          case LDC2_W:
            {
              if (((ConstantInstruction) inst).typeKind().equals(TypeKind.REFERENCE)) {
                return null;
              }
              return build_il(dcr_call("push_const", CD_void, noArgsSig), inst);
            }

          // Push the tag for the array onto the tag stack.  This causes
          // anything comparable to the length to be comparable to the array
          // as an index.
          case ARRAYLENGTH:
            {
              return array_length(inst);
            }

          case BIPUSH:
          case SIPUSH:
          case DCONST_0:
          case DCONST_1:
          case FCONST_0:
          case FCONST_1:
          case FCONST_2:
          case ICONST_0:
          case ICONST_1:
          case ICONST_2:
          case ICONST_3:
          case ICONST_4:
          case ICONST_5:
          case ICONST_M1:
          case LCONST_0:
          case LCONST_1:
            {
              return build_il(dcr_call("push_const", CD_void, noArgsSig), inst);
            }

          // Primitive Binary operators.  Each is augmented with a call to
          // DCRuntime.binary_tag_op that merges the tags and updates the tag
          // Stack.
          case DADD:
          case DCMPG:
          case DCMPL:
          case DDIV:
          case DMUL:
          case DREM:
          case DSUB:
          case FADD:
          case FCMPG:
          case FCMPL:
          case FDIV:
          case FMUL:
          case FREM:
          case FSUB:
          case IADD:
          case IAND:
          case IDIV:
          case IMUL:
          case IOR:
          case IREM:
          case ISHL:
          case ISHR:
          case ISUB:
          case IUSHR:
          case IXOR:
          case LADD:
          case LAND:
          case LCMP:
          case LDIV:
          case LMUL:
          case LOR:
          case LREM:
          case LSHL:
          case LSHR:
          case LSUB:
          case LUSHR:
          case LXOR:
            return build_il(dcr_call("binary_tag_op", CD_void, noArgsSig), inst);

          // Computed jump based on the int on the top of stack.  Since that int
          // is not made comparable to anything, we just discard its tag.  One
          // might argue that the key should be made comparable to each value in
          // the jump table.  But the tags for those values are not available.
          // And since they are all constants, its not clear how interesting it
          // would be anyway.
          case LOOKUPSWITCH:
          case TABLESWITCH:
            return discard_tag_code(inst, 1);

          // Make the integer argument to ANEWARRAY comparable to the new
          // array's index.
          case ANEWARRAY:
          case NEWARRAY:
            return new_array(inst);

          // If the new array has 2 dimensions, make the integer arguments
          // comparable to the corresponding indices of the new array.
          // For any other number of dimensions, discard the tags for the
          // arguments.
          case MULTIANEWARRAY:
            return multi_newarray_dc((NewMultiArrayInstruction) inst);

          // Mark the array and its index as comparable.  Also for primitives,
          // push the tag of the array element on the tag stack
          case AALOAD:
          case BALOAD:
          case CALOAD:
          case DALOAD:
          case FALOAD:
          case IALOAD:
          case LALOAD:
          case SALOAD:
            return array_load(mgen, (ArrayLoadInstruction) inst);

          // Prefix the return with a call to the correct normal_exit method
          // to handle the tag stack
          case ARETURN:
          case DRETURN:
          case FRETURN:
          case IRETURN:
          case LRETURN:
          case RETURN:
            return return_tag(mgen, inst);

          // Throws an exception.  This clears the operand stack of the current
          // frame.  We need to clear the tag stack as well.
          case ATHROW:
            return build_il(dcr_call("throw_op", CD_void, noArgsSig), inst);

          // Opcodes that don't need any modifications.  Here for reference.
          // Note that while we include JSR, JSR_W, RET and RET_W here, it is
          // only for documentation. They will throw a "Unexpected instruction opcode"
          // error during the operand stack calculation phase.
          case ACONST_NULL:
          case ALOAD:
          case ALOAD_0:
          case ALOAD_1:
          case ALOAD_2:
          case ALOAD_3:
          case ASTORE:
          case ASTORE_0:
          case ASTORE_1:
          case ASTORE_2:
          case ASTORE_3:
          case CHECKCAST:
          case D2F: // double to float
          case D2I: // double to integer
          case D2L: // double to long
          case DNEG: // Negate double on top of stack
          case F2D: // float to double
          case F2I: // float to integer
          case F2L: // float to long
          case FNEG: // Negate float on top of stack
          case GOTO:
          case GOTO_W:
          case I2B: // integer to byte
          case I2C: // integer to char
          case I2D: // integer to double
          case I2F: // integer to float
          case I2L: // integer to long
          case I2S: // integer to short
          case IFNONNULL:
          case IFNULL:
          case IINC: // increment local variable by a constant
          case IINC_W: // increment local variable by a constant
          case INEG: // negate integer on top of stack
          case JSR: // pushes return address on the stack, but that
          // is thought of as an object, so we don't need a tag for it.
          case JSR_W:
          case L2D: // long to double
          case L2F: // long to float
          case L2I: // long to int
          case LNEG: // negate long on top of stack
          case MONITORENTER:
          case MONITOREXIT:
          case NEW:
          case NOP:
          case RET: // this is the internal JSR return
          case RET_W:
            return null;

          // Handle subroutine calls.  Calls to instrumented code are modified
          // to call the instrumented version (with the DCompMarker argument).
          // Calls to uninstrumented code (rare) discard primitive arguments
          // from the tag stack and produce an arbitrary return tag.
          case INVOKESTATIC:
          case INVOKEVIRTUAL:
          case INVOKESPECIAL:
          case INVOKEINTERFACE:
            return handleInvoke((InvokeInstruction) inst, mgen);

          case INVOKEDYNAMIC:
            return handleInvokeDynamic((InvokeDynamicInstruction) inst);

          // Mark the array and its index as comparable.  For primitives, store
          // the tag for the value on the top of the stack in the tag storage
          // for the array.
          case AASTORE:
            return array_store(inst, "aastore", CD_Object);
          case BASTORE:
            // The JVM uses bastore for both byte and boolean.
            // We need to differentiate.
            ClassDesc arrayref = stack.peek(2);
            ClassDesc ct = arrayref.componentType();
            if (ct == null) {
              throw new Error("stack item not an arrayref: " + inst);
            }
            if (ct.equals(CD_boolean)) {
              return array_store(inst, "zastore", CD_boolean);
            } else {
              return array_store(inst, "bastore", CD_byte);
            }
          case CASTORE:
            return array_store(inst, "castore", CD_char);
          case DASTORE:
            return array_store(inst, "dastore", CD_double);
          case FASTORE:
            return array_store(inst, "fastore", CD_float);
          case IASTORE:
            return array_store(inst, "iastore", CD_int);
          case LASTORE:
            return array_store(inst, "lastore", CD_long);
          case SASTORE:
            return array_store(inst, "sastore", CD_short);

          default:
            throw new DynCompError("Unexpected instruction opcode: " + inst.opcode());
        }
      }

      // We ignore PseudoInstructions.

      case ExceptionCatch ec -> {
        // Ignore ExceptionCatch CodeElements.
        return null;
      }

      case Label l -> {
        // Ignore Label CodeElements.
        return null;
      }

      case LineNumber ln -> {
        // Ignore LineNumber CodeElements.
        return null;
      }

      default -> {
        throw new DynCompError("Unexpected CodeElement: " + ce);
      }
    }
  }

  /**
   * Adds a call to DCruntime.exit() at each return from the method. This call calculates
   * comparability on any variables that are being tracked.
   *
   * @param mgen method to modify
   * @param mi MethodInfo for the given method's code
   * @param minfo MInfo24 for the given method's code
   * @param instructions instruction list for method
   * @param method_info_index index for MethodInfo
   */
  private void add_exit(
      MethodGen24 mgen,
      MethodInfo mi,
      MethodGen24.MInfo24 minfo,
      List<CodeElement> instructions,
      int method_info_index) {

    // Iterator over all of the exit line numbers for this method, in order.
    // We will read one element from it each time that we encounter a
    // return instruction.
    Iterator<Integer> exitLocationIter = mi.exit_locations.iterator();

    // Loop through each instruction, looking for return instructions.
    ListIterator<CodeElement> li = instructions.listIterator();
    while (li.hasNext()) {
      CodeElement inst = li.next();

      // If this is a return instruction, Call DCRuntime.exit to calculate
      // comparability on Daikon variables
      if (inst instanceof ReturnInstruction) {
        List<CodeElement> newCode = new ArrayList<>();
        ClassDesc type = mgen.getReturnType();
        if (!type.equals(CD_void)) {
          TypeKind typeKind = TypeKind.from(type);
          LocalVariable returnLocal = getReturnLocal(mgen, type, minfo);
          if (typeKind.slotSize() == 1) {
            newCode.add(StackInstruction.of(DUP));
          } else {
            newCode.add(StackInstruction.of(DUP2));
          }
          newCode.add(StoreInstruction.of(typeKind, returnLocal.slot()));
        }
        if (!exitLocationIter.hasNext()) {
          throw new DynCompError("Not enough exit locations in the exitLocationIter");
        }
        newCode.addAll(
            callEnterOrExit(mgen, minfo, method_info_index, "exit", exitLocationIter.next()));

        // Back up iterator to point to `inst`, the return instruction, and insert the
        // instrumentation.
        li.previous();
        for (CodeElement ce : newCode) {
          li.add(ce);
        }
        // skip over the return instruction
        li.next();
      }
    }
  }

  /**
   * Returns the interface class name containing the implementation of the given method. The
   * interfaces of {@code startClass} are recursively searched.
   *
   * @param startClass the ClassModel whose interfaces are to be searched
   * @param methodName the target method to search for
   * @param paramTypes the target method's parameter types
   * @return the name of the interface class containing target method, or null if not found
   */
  private @Nullable @BinaryName String getDefiningInterface(
      ClassModel startClass, @Identifier String methodName, ClassDesc[] paramTypes) {

    if (debugGetDefiningInterface) {
      System.out.println("searching interfaces of: " + ClassGen24.getClassName(startClass));
    }
    for (ClassEntry classEntry : startClass.interfaces()) {
      @BinaryName String interfaceName = classEntry.asInternalName().replace('/', '.');
      if (debugGetDefiningInterface) {
        System.out.println("interface: " + interfaceName);
      }
      ClassModel cm;
      try {
        @SuppressWarnings("nullness:assignment")
        @NonNull ClassModel cmTmp = getClassModel(interfaceName);
        cm = cmTmp;
      } catch (Throwable t) {
        throw new DynCompError(String.format("Unable to load class: %s", interfaceName), t);
      }
      for (MethodModel jm : cm.methods()) {
        String jmName = jm.methodName().stringValue();
        MethodTypeDesc mtd = jm.methodTypeSymbol();
        if (debugGetDefiningInterface) {
          System.out.println("  " + jmName + Arrays.toString(mtd.parameterArray()));
        }
        if (jmName.equals(methodName) && Arrays.equals(mtd.parameterArray(), paramTypes)) {
          // We have a match.
          return interfaceName;
        }
      }
      // no match found; does this interface extend other interfaces?
      @BinaryName String foundAbove = getDefiningInterface(cm, methodName, paramTypes);
      if (foundAbove != null) {
        // We have a match.
        return foundAbove;
      }
    }
    // nothing found
    return null;
  }

  /**
   * Process an InvokeDynamic instruction. We don't instrument lambda methods, so just clean up the
   * tag stack.
   *
   * @param invoke a method invocation bytecode instruction
   * @return instructions to replace the given instruction
   */
  private List<CodeElement> handleInvokeDynamic(InvokeDynamicInstruction invoke) {

    // Get information about the call
    // There isn't really a class holding the method.
    String classname = "";
    MethodTypeDesc mtd = invoke.typeSymbol();
    ClassDesc returnType = mtd.returnType();
    ClassDesc[] paramTypes = mtd.parameterArray();
    if (debugHandleInvoke) {
      System.out.println("invokedynamic: " + invoke);
      System.out.printf("  callee_instrumented: false%n");
    }

    return cleanInvokeTagStack(invoke, classname, returnType, paramTypes);
  }

  /**
   * Process an Invoke instruction. There are three cases:
   *
   * <ul>
   *   <li>convert calls to Object.equals to calls to dcomp_equals or dcomp_super_equals
   *   <li>convert calls to Object.clone to calls to dcomp_clone or dcomp_super_clone
   *   <li>otherwise, determine whether the target of the invoke is instrumented or not (this is the
   *       {@code callee_instrumented} variable)
   *       <ul>
   *         <li>If the target method is instrumented, add a DCompMarker parameter to the end of the
   *             parameter list.
   *         <li>If the target method is not instrumented, we must account for the fact that the
   *             instrumentation code generated up to this point has assumed that the target method
   *             is instrumented. Hence, generate code to discard a primitive tag from the
   *             DCRuntime's per-thread comparability data stack for each primitive argument. If the
   *             return type of the target method is a primitive, add code to push a tag onto the
   *             runtime comparability data stack to represent the primitive return value.
   *       </ul>
   * </ul>
   *
   * @param invoke a method invocation bytecode instruction
   * @param mgen host method of invoke
   * @return instructions to replace the given instruction
   */
  private List<CodeElement> handleInvoke(InvokeInstruction invoke, MethodGen24 mgen) {

    // Get information about the call.
    @BinaryName String classname = invoke.owner().asInternalName().replace('/', '.');
    String methodName = invoke.name().stringValue();
    MethodTypeDesc mtd = invoke.typeSymbol();
    ClassDesc returnType = mtd.returnType();
    ClassDesc[] paramTypes = mtd.parameterArray();

    if (debugHandleInvoke) {
      System.out.println();
      System.out.println("InvokeInst: " + invoke);
      System.out.println("returnType: " + returnType);
      System.out.println("classname: " + classname);
    }

    if (is_object_equals(methodName, returnType, paramTypes)) {

      // Replace calls to Object's equals method with calls to our
      // replacement, a static method in DCRuntime.
      List<CodeElement> il = new ArrayList<>();
      il.add(
          dcr_call(
              invoke.opcode().equals(INVOKESPECIAL) ? "dcomp_super_equals" : "dcomp_equals",
              returnType,
              new ClassDesc[] {CD_Object, CD_Object}));
      return il;
    }

    if (is_object_clone(methodName, returnType, paramTypes)) {

      // Replace calls to Object's clone method with calls to our
      // replacement, a static method in DCRuntime.

      return instrument_clone_call(invoke, returnType, classname);
    }

    boolean callee_instrumented =
        isTargetInstrumented(invoke, mgen, classname, methodName, paramTypes);

    if (debugHandleInvoke) {
      System.out.printf("handleInvoke(%s)%n", invoke);
      System.out.printf("  invoke host: %s.%s%n", classGen.getClassName(), mgen.getName());
      System.out.printf("  invoke targ: %s.%s%n", classname, methodName);
      System.out.printf("  callee_instrumented: %s%n", callee_instrumented);
    }

    if (callee_instrumented) {

      List<CodeElement> il = new ArrayList<>();

      // Push the DCompMarker argument as we are calling the instrumented version.
      il.add(ConstantInstruction.ofIntrinsic(ACONST_NULL));

      // Add the DCompMarker type to the parameter types list.
      List<ClassDesc> new_param_types = new ArrayList<>(Arrays.asList(paramTypes));
      new_param_types.add(dcomp_marker);

      NameAndTypeEntry nte =
          poolBuilder.nameAndTypeEntry(methodName, MethodTypeDesc.of(returnType, new_param_types));
      il.add(InvokeInstruction.of(invoke.opcode(), invoke.owner(), nte, invoke.isInterface()));
      return il;

    } else { // not instrumented, discard the tags before making the call
      return cleanInvokeTagStack(invoke, classname, returnType, paramTypes);
    }
  }

  /**
   * Clean up the tag stack for an invoke instruction that calls a non-instrumented method. Items
   * were pushed onto the tag stack for each argument to the invoke. But we have since discovered
   * that the target method is not instrumented. Thus the DynComp runtime must discard (pop) these
   * tags before the invoke is executed. In addition, if the method being invoked has a primitive
   * return type, the runtime must push a new, unique tag after the invoke.
   *
   * @param invoke a method invocation bytecode instruction
   * @param classname target class of the invoke
   * @param returnType return type of method
   * @param paramTypes parameter types of target method
   * @return instructions to replace the given instruction
   */
  private List<CodeElement> cleanInvokeTagStack(
      Instruction invoke, String classname, ClassDesc returnType, ClassDesc[] paramTypes) {
    List<CodeElement> il = new ArrayList<>();

    // JUnit test classes are a bit strange.  They are marked as not being callee_instrumented
    // because they do not have the dcomp_marker added to the parameter list, but
    // they actually contain instrumentation code.  So we do not want to discard
    // the primitive tags prior to the call.
    if (!junitTestClasses.contains(classname)) {
      il = discard_primitive_tags(paramTypes);
    }

    // Add a tag for the return type if it is primitive.
    if (is_primitive(returnType)) {
      if (debugHandleInvoke) {
        System.out.printf("push tag for return type of %s%n", returnType);
      }
      il.add(dcr_call("push_const", CD_void, noArgsSig));
    }
    il.add(invoke);
    return il;
  }

  /**
   * Returns instructions that will discard (pop) any primitive tags corresponding to the specified
   * parameters. Returns an empty instruction list if there are no primitive arguments to discard.
   *
   * @param paramTypes parameter types of target method
   * @return an instruction list that discards primitive tags from DCRuntime's per-thread
   *     comparability data stack
   */
  private List<CodeElement> discard_primitive_tags(ClassDesc[] paramTypes) {
    int primitive_cnt = 0;
    for (ClassDesc paramType : paramTypes) {
      if (paramType.isPrimitive()) {
        primitive_cnt++;
      }
    }
    if (primitive_cnt > 0) {
      return discard_tag_code(null, primitive_cnt);
    }
    // Must return a mutable array because some clients mutate it.
    return new ArrayList<>();
  }

  /**
   * Returns true if the invoked method (the callee) is instrumented.
   *
   * @param invoke instruction whose target is to be checked
   * @param mgen method containing the invoke
   * @param classname target class of the invoke (the callee)
   * @param methodName target method of the invoke (the callee)
   * @param paramTypes parameter types of target method
   * @return true if the target is instrumented
   */
  private boolean isTargetInstrumented(
      InvokeInstruction invoke,
      MethodGen24 mgen,
      @BinaryName String classname,
      @Identifier String methodName,
      ClassDesc[] paramTypes) {

    boolean targetInstrumented;
    Opcode op = invoke.opcode();

    if (is_object_method(methodName, paramTypes)) {
      targetInstrumented = false;
    } else {
      // At this point, we will never see classname = java.lang.Object.
      targetInstrumented = isClassnameInstrumented(classname, methodName);

      if (debugHandleInvoke) {
        System.out.printf("isClassnameInstrumented: %s%n", targetInstrumented);
        System.out.printf("invoke host: %s.%s%n", classGen.getClassName(), mgen.getName());
        System.out.printf("invoke targ: %s.%s%n", classname, methodName);
      }

      if (Premain.problem_methods.contains(classname + "." + methodName)) {
        debugInstrument.log(
            "Don't call instrumented version of problem method %s.%s.%n", classname, methodName);
        targetInstrumented = false;
      }

      // targetInstrumented (the return value of this method) has been set.
      // Now, adjust it for some special cases.
      // Every adjustment is from `true` to `false`.

      // There are two special cases we need to detect:
      //   calls to annotations
      //   calls to functional interfaces
      //
      // Annotation classes are never instrumented so we must set
      // the targetInstrumented flag to false.
      //
      // Functional interfaces are a bit more complicated. These are primarily (only?)
      // used by Lambda functions.  Lambda methods are generated dynamically at
      // run time via the InvokeDynamic instruction.  They are not seen by our
      // ClassFileTransformer so are never instrumented.  Thus we must set the
      // targetInstrumented flag to false when we see a call to a Lambda method.
      // The heuristic we use is to assume that any InvokeInterface or InvokeVirtual
      // call to a functional interface is a call to a Lambda method.
      //
      // The Java compiler detects functional interfaces automatically, but the
      // user can declare their intent with the @FunctionInterface annotation.
      // The Java runtime is annotated in this manner.  Hence, we look for this
      // annotation to detect a call to a functional interface.  In practice, we
      // could detect functional interfaces in a manner similar to the Java
      // compiler, but for now we will go with this simpler method.
      //
      // Note that to simplify our code we set the access flags for a functional
      // interface to ANNOTATION in our accessFlags map.
      //
      if (targetInstrumented == true && (op.equals(INVOKEINTERFACE) || op.equals(INVOKEVIRTUAL))) {
        Integer access = getAccessFlags(classname);

        if ((access & ACC_ANNOTATION) != 0) {
          targetInstrumented = false;
        }

        // UNDONE: New code added above should handle the case below.  Need to find a test
        // case and verify this code is no longer needed.
        // This is a bit of a hack.  An invokeinterface instruction with a
        // a target of "java.util.stream.<something>" might be calling a
        // lambda method in which case we don't want to add the dcomp_marker.
        // Might lose something in "normal" cases, but no easy way to detect.
        if (classname.startsWith("java.util.stream")) {
          targetInstrumented = false;
        }

        // In a similar fashion, when the Java runtime is processing annotations, there might
        // be an invoke (via reflection) of a member of the java.lang.annotation package; this
        // too should not have the dcomp_marker added.
        if (classname.startsWith("java.lang.annotation")) {
          targetInstrumented = false;
        }
      }

      // If we are not using the instrumented JDK, then we need to track down the
      // actual target of an INVOKEVIRTUAL to see if it has been instrumented or not.
      if (targetInstrumented == true && op.equals(INVOKEVIRTUAL)) {
        if (!Premain.jdk_instrumented && !mgen.getName().equals("equals_dcomp_instrumented")) {

          if (debugHandleInvoke) {
            System.out.println("method: " + methodName);
            System.out.println("paramTypes: " + Arrays.toString(paramTypes));
            System.out.printf("invoke host: %s.%s%n", mgen.getClassName(), mgen.getName());
          }

          @BinaryName String targetClassname = classname;
          // Search this class for the target method. If not found, set targetClassname to
          // its superclass and try again.
          mainloop:
          while (true) {
            // Check that the class exists
            ClassModel targetClass;
            try {
              targetClass = getClassModel(targetClassname);
            } catch (Throwable e) {
              targetClass = null;
            }
            if (targetClass == null) {
              // We cannot locate or read the .class file, better assume not instrumented.
              if (debugHandleInvoke) {
                System.out.printf("Unable to locate class: %s%n%n", targetClassname);
              }
              targetInstrumented = false;
              break;
            }
            if (debugHandleInvoke) {
              System.out.println("target class: " + targetClassname);
            }

            for (MethodModel jm : targetClass.methods()) {
              String jmName = jm.methodName().stringValue();
              MethodTypeDesc mtd = jm.methodTypeSymbol();
              if (debugHandleInvoke) {
                System.out.println("  " + jmName + Arrays.toString(mtd.parameterArray()));
              }
              if (jmName.equals(methodName) && Arrays.equals(mtd.parameterArray(), paramTypes)) {
                // We have a match.
                if (debugHandleInvoke) {
                  System.out.printf("we have a match%n%n");
                }
                if (BcelUtil.inJdk(targetClassname)) {
                  targetInstrumented = false;
                }
                break mainloop;
              }
            }

            {
              // no methods match - search this class's interfaces
              @BinaryName String found;
              try {
                found = getDefiningInterface(targetClass, methodName, paramTypes);
              } catch (Throwable e) {
                // We cannot locate or read the .class file, better assume it is not instrumented.
                targetInstrumented = false;
                break;
              }
              if (found != null) {
                // We have a match.
                if (debugHandleInvoke) {
                  System.out.printf("we have a match%n%n");
                }
                if (BcelUtil.inJdk(found)) {
                  targetInstrumented = false;
                }
                break;
              }
            }

            // Method not found; perhaps inherited from superclass.
            if (targetClassname.equals("java.lang.Object")) {
              // The target class was Object; the search completed without finding a matching
              // method.
              if (debugHandleInvoke) {
                System.out.printf("Unable to locate method: %s%n%n", methodName);
              }
              targetInstrumented = false;
              break;
            }

            // Recurse looking in the superclass.
            targetClassname = getSuperclassName(targetClassname);
          }
        }
      }
    }

    if (op.equals(INVOKESPECIAL)) {
      if (classname.equals(classGen.getSuperclassName()) && methodName.equals("<init>")) {
        this.constructor_is_initialized = true;
      }
    }

    return targetInstrumented;
  }

  /**
   * Returns the access flags for the given class.
   *
   * @param classname the class whose access flags to return
   * @return the access flags for the given class
   */
  private Integer getAccessFlags(String classname) {
    Integer access = accessFlags.get(classname);
    if (access == null) {
      // We have not seen this class before. Check to see if the target class is
      // an Annotation or a FunctionalInterface.
      ClassModel cm = getClassModel(classname);
      if (cm != null) {
        access = cm.flags().flagsMask();

        debugInstrument.log("classModel: %s%n", cm.thisClass().name().stringValue());
        debugInstrument.log("getAccessFlags: %s%n", classname);
        // Now check for FunctionalInterface
        searchloop:
        for (Attribute<?> attribute : cm.attributes()) {
          debugInstrument.log("attribute: %s%n", attribute);
          if (attribute instanceof RuntimeVisibleAnnotationsAttribute rvaa) {
            for (final Annotation item : rvaa.annotations()) {
              String annotation = item.className().stringValue();
              if (debugHandleInvoke) {
                System.out.println("annotation: " + annotation);
              }
              if (annotation.endsWith("FunctionalInterface;")) {
                access = Integer_ACC_ANNOTATION;
                if (debugHandleInvoke) {
                  System.out.println("FunctionalInterface is true");
                }
                break searchloop;
              }
            }
          }
        }
      } else {
        // We cannot locate or read the .class file, better pretend it is an Annotation.
        if (debugHandleInvoke) {
          System.out.printf("Unable to locate class: %s%n", classname);
        }
        access = Integer_ACC_ANNOTATION;
      }
      accessFlags.put(classname, access);
    }
    return access;
  }

  /**
   * Returns true if the specified class is instrumented or we presume it will be instrumented by
   * the time it is executed.
   *
   * @param classname class to be checked
   * @param methodName method to be checked (currently unused)
   * @return true if classname is instrumented
   */
  private boolean isClassnameInstrumented(
      @BinaryName String classname, @Identifier String methodName) {

    if (debugHandleInvoke) {
      System.out.printf("Checking callee instrumented on %s.%s%n", classname, methodName);
    }

    // Our copy of daikon.plumelib is not instrumented.  It would be odd, though,
    // to see calls to this.
    if (classname.startsWith("daikon.plumelib")) {
      return false;
    }

    // When a class contains an existing <clinit>, it will be instrumented. Thus, we need to mark
    // our added call to 'DCRuntime.set_class_initialized' as not instrumented.
    if (classname.endsWith("DCRuntime") && methodName.equals("set_class_initialized")) {
      return false;
    }

    // Special-case JUnit test classes.
    if (junitTestClasses.contains(classname)) {
      return false;
    }

    if (daikon.dcomp.Instrument24.is_transformer(classname.replace('.', '/'))) {
      return false;
    }

    // Special-case the execution trace tool.
    if (classname.startsWith("minst.Minst")) {
      return false;
    }

    // We should probably change the interface to include method name
    // and use "classname.methodname" as arg to pattern matcher.
    // If any of the omit patterns match, use the uninstrumented version of the method
    for (Pattern p : DynComp.ppt_omit_pattern) {
      if (p.matcher(classname).find()) {
        if (debugHandleInvoke) {
          System.out.printf("callee instrumented = false: %s.%s%n", classname, methodName);
        }
        return false;
      }
    }

    // If it's not a JDK class, presume it's instrumented.
    if (!BcelUtil.inJdk(classname)) {
      return true;
    }

    int i = classname.lastIndexOf('.');
    if (i > 0 && Premain.problem_packages.contains(classname.substring(0, i))) {
      debugInstrument.log(
          "Don't call instrumented member of problem package %s%n", classname.substring(0, i));
      return false;
    }

    if (Premain.problem_classes.contains(classname)) {
      debugInstrument.log("Don't call instrumented member of problem class %s%n", classname);
      return false;
    }

    // We have decided not to use the instrumented version of Random as
    // the method generates values based on an initial seed value.
    // (Typical of random() algorithms.) Instrumentation would have the undesirable side
    // effect of putting all the generated values in the same comparison
    // set when they should be distinct.
    // Note: If we find other classes that should not use the instrumented
    // versions, we should consider making this a searchable list.
    if (classname.equals("java.util.Random")) {
      return false;
    }

    // If using the instrumented JDK, then everthing but object is instrumented.
    if (Premain.jdk_instrumented && !classname.equals("java.lang.Object")) {
      return true;
    }

    return false;
  }

  /**
   * Returns a list of the superclasses of this class. This class itself is not in the list. The
   * returned list is in ascending order; that is, java.lang.Object is always the last element,
   * unless the argument is java.lang.Object.
   *
   * @return list of superclasses
   */
  public List<@BinaryName String> getStrictSuperclassNames() {
    final List<@BinaryName String> allSuperclassNames = new ArrayList<>();
    @BinaryName String classname = classGen.getClassName();
    while (!classname.equals("java.lang.Object")) {
      classname = getSuperclassName(classname);
      allSuperclassNames.add(classname);
    }
    return allSuperclassNames;
  }

  /**
   * Given a classname return its superclass name. Note that we copy BCEL and report that the
   * superclass of {@code java.lang.Object} is {@code java.lang.Object} rather than saying there is
   * no superclass.
   *
   * @param classname the fully-qualified name of the class in binary form. E.g., "java.util.List"
   * @return name of superclass
   */
  private @BinaryName String getSuperclassName(String classname) {
    ClassModel cm = getClassModel(classname);
    if (cm == null) {
      throw new SuperclassNameError(classname);
    }
    return ClassGen24.getSuperclassName(cm);
  }

  /** Unchecked exception thrown if {@link #getSuperclassName} cannot find a superclass name. */
  private static class SuperclassNameError extends Error {
    static final long serialVersionUID = 20251203;

    /**
     * Creates a SuperclassNameError.
     *
     * @param classname the name of the class whose parent cannot be found
     */
    SuperclassNameError(String classname) {
      super(classname);
    }
  }

  /** Cache for {@link #getClassModel} method. */
  private static Map<String, ClassModel> classModelCache = new ConcurrentHashMap<>();

  /**
   * There are times when it is useful to inspect a class file other than the one we are currently
   * instrumenting. We cannot use {@code classForName} to do this as it might trigger a recursive
   * call to Instrument which would not work at this point.
   *
   * <p>Given a class name, we treat it as a system resource, create a {@code Path} to it and have
   * {@code java.lang.classfile} read and create a {@code ClassModel} object.
   *
   * @param classname the fully-qualified name of the class in binary form, e.g., "java.util.List"
   * @return the ClassModel of the corresponding classname or null
   */
  private @Nullable ClassModel getClassModel(String classname) {
    ClassModel cached = classModelCache.get(classname);
    if (cached != null) {
      return cached;
    }

    ClassFile classFile = ClassFile.of();
    URL class_url = ClassLoader.getSystemResource(classname.replace('.', '/') + ".class");
    if (class_url != null) {
      try (InputStream inputStream = class_url.openStream()) {
        if (inputStream != null) {
          byte[] buffer = inputStream.readAllBytes();
          ClassModel result = classFile.parse(buffer);
          classModelCache.put(classname, result);
          return result;
        }
      } catch (Throwable t) {
        throw new DynCompError(
            String.format("Error while reading %s %s%n", classname, class_url), t);
      }
    }
    // Do not cache a null result, because a subsequent invocation might return non-null.
    return null;
  }

  /**
   * Returns true if the method is Object.equals().
   *
   * @param methodName method to check
   * @param returnType return type of method
   * @param paramTypes array of parameter types to method
   * @return true if method is Object.equals()
   */
  @Pure
  boolean is_object_equals(
      @Identifier String methodName, ClassDesc returnType, ClassDesc[] paramTypes) {
    return (methodName.equals("equals")
        && returnType.equals(CD_boolean)
        && paramTypes.length == 1
        && paramTypes[0].equals(CD_Object));
  }

  /**
   * Returns true if the specified method is Object.clone().
   *
   * @param methodName method to check
   * @param returnType return type of method
   * @param paramTypes array of parameter types to method
   * @return true if method is Object.clone()
   */
  @Pure
  boolean is_object_clone(
      @Identifier String methodName, ClassDesc returnType, ClassDesc[] paramTypes) {
    return methodName.equals("clone") && returnType.equals(CD_Object) && (paramTypes.length == 0);
  }

  /**
   * Instrument calls to the Object method {@code clone}. An instrumented version is called if it
   * exists, the non-instrumented version if it does not.
   *
   * @param invoke invoke instruction to inspect and replace
   * @param returnType return type of method
   * @param classname target class
   * @return instruction list to call the correct version of clone or toString
   */
  private List<CodeElement> instrument_clone_call(
      InvokeInstruction invoke, ClassDesc returnType, @BinaryName String classname) {

    List<CodeElement> il = new ArrayList<>();
    if (classname.startsWith("[")) {
      // <array>.clone() is never instrumented, return original invoke.
      il.add(invoke);
      return il;
    }

    // push the target class
    il.add(buildLDCInstruction(poolBuilder.classEntry(ClassDesc.of(classname))));

    if (invoke.opcode().equals(INVOKESPECIAL)) {
      // This is a super call.

      // Runtime will discover if the object's superclass has an instrumented clone method.
      // If so, call it; otherwise call the uninstrumented version.
      // use CD_Class
      il.add(dcr_call("dcomp_super_clone", returnType, new ClassDesc[] {CD_Object, CD_Class}));

    } else {
      // This is a regular (non-super) clone() call.

      // Runtime will discover if the object has an instrumented clone method.
      // If so, call it; otherwise call the uninstrumented version.
      il.add(dcr_call("dcomp_clone", returnType, new ClassDesc[] {CD_Object, CD_Class}));
    }

    return il;
  }

  /**
   * Create the instructions that replace the object eq or ne branch instruction. They are replaced
   * by a call to the specified compareMethod (which returns a boolean) followed by the specified
   * boolean ifeq or ifne instruction.
   *
   * @param branch a branch instruction
   * @param compareMethod name of DCRuntime routine to call
   * @param boolean_if branch instruction to gerate of the runtime call
   * @return instruction list to do object comparison
   */
  private List<CodeElement> object_comparison(
      BranchInstruction branch, String compareMethod, Opcode boolean_if) {
    List<CodeElement> il = new ArrayList<>();

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, compareMethod, MethodTypeDesc.of(CD_boolean, objectObjectSig));
    il.add(InvokeInstruction.of(INVOKESTATIC, mre));
    il.add(BranchInstruction.of(boolean_if, branch.target()));
    return il;
  }

  /**
   * Handles load and store field instructions. If the field is a primitive the instructions must be
   * augmented to either push (load) or pop (store) the tag on the tag stack. This is accomplished
   * by calling the tag get/set method for this field.
   *
   * @param mgen describes the given method
   * @param minfo for the given method's code
   * @param fi the field instruction
   * @return instruction list to access the field
   */
  private @Nullable List<CodeElement> load_store_field(
      MethodGen24 mgen, MethodGen24.MInfo24 minfo, FieldInstruction fi) {

    ClassDesc field_type = fi.typeSymbol();
    debugInstrument.log("field_type: %s%n", field_type);
    int field_size = TypeKind.from(field_type).slotSize();
    if (!field_type.isPrimitive()) {
      return null;
    }

    List<CodeElement> il = new ArrayList<>();
    Opcode op = fi.opcode();
    String fieldName = fi.name().stringValue();
    @BinaryName String owner = fi.owner().asInternalName().replace('/', '.');
    ClassDesc ownerCD = fi.owner().asSymbol();

    // If this class doesn't support tag fields, don't load/store them.
    if (!tag_fields_ok(mgen, owner)) {
      if (op.equals(GETFIELD) || op.equals(GETSTATIC)) {
        il.add(dcr_call("push_const", CD_void, noArgsSig));
      } else {
        il.add(loadIntegerConstant(1));
        il.add(dcr_call("discard_tag", CD_void, intSig));
      }

      // Perform the original field command.
      il.add(fi);
      return il;
    }

    if (op.equals(GETSTATIC)) {
      MethodRefEntry mre =
          poolBuilder.methodRefEntry(
              ownerCD,
              Premain.tag_method_name(Premain.GET_TAG, owner, fieldName),
              MethodTypeDesc.of(CD_void, noArgsSig));
      il.add(InvokeInstruction.of(INVOKESTATIC, mre));
    } else if (op.equals(PUTSTATIC)) {
      MethodRefEntry mre =
          poolBuilder.methodRefEntry(
              ownerCD,
              Premain.tag_method_name(Premain.SET_TAG, owner, fieldName),
              MethodTypeDesc.of(CD_void, noArgsSig));
      il.add(InvokeInstruction.of(INVOKESTATIC, mre));
    } else if (op.equals(GETFIELD)) {
      il.add(StackInstruction.of(DUP)); // dup 'this'
      MethodRefEntry mre =
          poolBuilder.methodRefEntry(
              ownerCD,
              Premain.tag_method_name(Premain.GET_TAG, owner, fieldName),
              MethodTypeDesc.of(CD_void, noArgsSig));
      il.add(InvokeInstruction.of(INVOKEVIRTUAL, mre));
    } else { // must be PUTFIELD
      if (field_size == 2) {
        LocalVariable lv = get_tmp2_local(mgen, minfo, field_type);
        il.add(StoreInstruction.of(TypeKind.from(field_type), lv.slot()));
        il.add(StackInstruction.of(DUP)); // dup 'this'
        MethodRefEntry mre =
            poolBuilder.methodRefEntry(
                ownerCD,
                Premain.tag_method_name(Premain.SET_TAG, owner, fieldName),
                MethodTypeDesc.of(CD_void, noArgsSig));
        il.add(InvokeInstruction.of(INVOKEVIRTUAL, mre));
        il.add(LoadInstruction.of(TypeKind.from(field_type), lv.slot()));
      } else {
        il.add(StackInstruction.of(SWAP)); // swap 'this' and 'value'
        il.add(StackInstruction.of(DUP)); // dup 'this'
        MethodRefEntry mre =
            poolBuilder.methodRefEntry(
                ownerCD,
                Premain.tag_method_name(Premain.SET_TAG, owner, fieldName),
                MethodTypeDesc.of(CD_void, noArgsSig));
        il.add(InvokeInstruction.of(INVOKEVIRTUAL, mre));
        il.add(StackInstruction.of(SWAP)); // swap 'value' and 'this' back
      }
    }

    // Perform the original field command.
    il.add(fi);

    return il;
  }

  /**
   * Handles load local instructions. The instructions must be augmented to push the tag on the tag
   * stack. This is accomplished by calling the specified method in DCRuntime and passing that
   * method the tag_frame frame and the offset of local/parameter.
   *
   * @param load a load instruction
   * @param tagFrameLocal local variable for the tag_frame
   * @param method name of DCRuntime routine to call
   * @return instruction list to do object comparison
   */
  private List<CodeElement> load_local(
      LoadInstruction load, LocalVariable tagFrameLocal, String method) {
    List<CodeElement> il = new ArrayList<>();

    // debugInstrument.log("CreateLoad %s %d%n", load.opcode(), load.slot());

    // Push the tag_frame frame
    il.add(LoadInstruction.of(TypeKind.REFERENCE, tagFrameLocal.slot()));

    // push index of local
    il.add(loadIntegerConstant(load.slot()));

    // Call the runtime method to handle loading the local/parameter
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, method, MethodTypeDesc.of(CD_void, objectArrayCD, CD_int));
    il.add(InvokeInstruction.of(INVOKESTATIC, mre));

    // the original load instruction
    il.add(load);
    return il;
  }

  /**
   * Handles store local instructions. The instructions must be augmented to pop the tag off the tag
   * stack. This is accomplished by calling the specified method in DCRuntime and passing that
   * method the tag_frame frame and the offset of local/parameter.
   *
   * @param store a store instruction
   * @param tagFrameLocal local variable for the tag_frame
   * @param method name of DCRuntime routine to call
   * @return instruction list to do object comparison
   */
  private List<CodeElement> store_local(
      StoreInstruction store, LocalVariable tagFrameLocal, String method) {
    List<CodeElement> il = new ArrayList<>();

    // debugInstrument.log("CreateStore %s %d%n", store.opcode(), store.slot());

    // Push the tag_frame frame
    il.add(LoadInstruction.of(TypeKind.REFERENCE, tagFrameLocal.slot()));

    // push index of local
    il.add(loadIntegerConstant(store.slot()));

    // Call the runtime method to handle storeing the local/parameter
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, method, MethodTypeDesc.of(CD_void, objectArrayCD, CD_int));
    il.add(InvokeInstruction.of(INVOKESTATIC, mre));

    // the original store instruction
    il.add(store);
    return il;
  }

  /**
   * Gets the local variable used to store a category2 temporary. This is used in the PUTFIELD code
   * to temporarily store the value being placed in the field.
   *
   * @param mgen describes the given method
   * @param minfo for the given method's code
   * @param type type of the local temp
   * @return the local temp
   */
  LocalVariable get_tmp2_local(MethodGen24 mgen, MethodGen24.MInfo24 minfo, ClassDesc type) {

    String name = "dcomp_$tmp_" + type.descriptorString();

    // See if the local has already been created
    for (LocalVariable lv : mgen.localsTable) {
      if (lv.name().stringValue().equals(name)) {
        assert lv.typeSymbol().equals(type) : lv + " " + type;
        return lv;
      }
    }

    // Create the variable
    return createLocalWithMethodScope(mgen, minfo, name, type);
  }

  /**
   * Returns the local variable used to store the return result. If it is not present, creates it
   * with the specified type. If the variable is known to already exist, the type can be null.
   *
   * @param mgen describes the given method
   * @param returnType the type of the return; may be null if the variable is known to already exist
   * @param minfo for the given method's code
   * @return a local variable to save the return value
   */
  @SuppressWarnings("nullness")
  private LocalVariable getReturnLocal(
      MethodGen24 mgen, @Nullable ClassDesc returnType, MethodGen24.MInfo24 minfo) {

    // If a type was specified and the variable was found, they must match.
    if (minfo.returnLocal == null) {
      assert returnType != null : " return__$trace2_val doesn't exist";
    } else {
      assert minfo.returnLocal.typeSymbol().equals((Object) returnType)
          : " returnType = " + returnType + "; current type = " + minfo.returnLocal.typeSymbol();
    }

    if (minfo.returnLocal == null) {
      debugInstrument.log("Adding return local of type %s%n", returnType);
      minfo.returnLocal =
          createLocalWithMethodScope(mgen, minfo, "return__$trace2_val", returnType);
    }

    return minfo.returnLocal;
  }

  /**
   * Creates a MethodInfo corresponding to the specified method. The exit location information for
   * the method is collected. Returns null if the method contains no instructions.
   *
   * @param classInfo class containing the method
   * @param mgen method to inspect
   * @return MethodInfo for the method
   */
  private @Nullable MethodInfo create_method_info_if_instrumented(
      ClassInfo classInfo, MethodGen24 mgen) {

    // Get the parameter names for this method
    String[] paramNames = mgen.getParameterNames();

    if (debugInstrument.enabled) {
      debugInstrument.log("create_method_info_if_instrumented: %s%n", paramNames.length);
      for (String paramName : paramNames) {
        debugInstrument.log("param name: %s%n", paramName);
      }
    }

    // Get the parameter types for this method.
    ClassDesc[] paramTypes = mgen.getParameterTypes();
    @ClassGetName String[] param_type_strings = new @ClassGetName String[paramTypes.length];
    for (int i = 0; i < paramTypes.length; i++) {
      param_type_strings[i] = Instrument24.classDescToClassGetName(paramTypes[i]);
    }

    // Loop through each instruction and find the line number for each return opcode.
    List<Integer> exit_line_numbers = new ArrayList<>();

    // Tells whether each exit loc in the method is included or not (based on filters).
    List<Boolean> isIncluded = new ArrayList<>();

    debugInstrument.log("Looking for exit points in %s%n", mgen.getName());
    List<CodeElement> il = mgen.getInstructionList();
    if (il.isEmpty()) {
      return null;
    }

    int line_number = 0;
    int last_line_number = 0;

    for (CodeElement inst : il) {
      boolean foundLine = false;

      if (inst instanceof LineNumber ln) {
        line_number = ln.line();
        foundLine = true;
      }

      if (inst instanceof ReturnInstruction) {
        debugInstrument.log("Exit at line %d%n", line_number);

        // Only do incremental lines if we don't have the line generator.
        if (line_number == last_line_number && foundLine == false) {
          debugInstrument.log("Could not find line %d%n", line_number);
          line_number++;
        }
        last_line_number = line_number;

        exit_line_numbers.add(line_number);
        isIncluded.add(true);
      }
    }

    return new MethodInfo(
        classInfo, mgen.getName(), paramNames, param_type_strings, exit_line_numbers, isIncluded);
  }

  /**
   * Creates code that makes the index comparable (for indexing purposes) with the array in array
   * load instructions. First the arrayref and its index are duplicated on the stack. Then the
   * appropriate array load method is called to mark them as comparable and update the tag stack.
   * Finally the original load instruction is performed.
   *
   * @param mgen method to check
   * @param inst an array load instruction
   * @return instruction list that calls the runtime to handle the array load instruction
   */
  private List<CodeElement> array_load(MethodGen24 mgen, ArrayLoadInstruction inst) {
    List<CodeElement> il = new ArrayList<>();

    // Duplicate the array ref and index and pass them to DCRuntime
    // which will make the index comparable with the array.  In the case
    // of primtives it will also get the tag for the primitive and push
    // it on the tag stack.
    il.add(StackInstruction.of(DUP2));
    String method = "primitive_array_load";
    if (inst.typeKind().equals(TypeKind.REFERENCE)) {
      method = "ref_array_load";
    } else if (is_class_initialized_by_jvm(mgen.getClassName())) {
      method = "primitive_array_load_null_ok";
    }

    il.add(dcr_call(method, CD_void, new ClassDesc[] {CD_Object, CD_int}));

    // Perform the original instruction
    il.add(inst);

    return il;
  }

  /**
   * Creates code to make the index comparable (for indexing purposes) with the array in the array
   * store instruction. This is accomplished by calling the specified method and passing it the
   * array reference, index, and value (of base_type). The method will mark the array and index as
   * comparable and perform the array store.
   *
   * @param inst an array store instruction
   * @param method runtime method to call
   * @param base_type type of array store
   * @return instruction list that calls the runtime to handle the array store instruction
   */
  List<CodeElement> array_store(CodeElement inst, @Identifier String method, ClassDesc base_type) {
    List<CodeElement> il = new ArrayList<>();
    ClassDesc array_type = base_type.arrayType(1);
    // if (method.equals("aastore")) {
    // System.out.println("array_store aastore: " + method);
    // il.add(loadIntegerConstant(counter++));
    // il.add(dcr_call(method, CD_void, new ClassDesc[] {array_type, CD_int, base_type, CD_int}));
    // } else {
    // System.out.println("array_store other: " + method);
    il.add(dcr_call(method, CD_void, new ClassDesc[] {array_type, CD_int, base_type}));
    // }
    return il;
  }

  /**
   * Creates code that pushes the array's tag onto the tag stack, so that the index is comparable to
   * the array length. First, the arrayref is duplicated on the stack. Then a method is called to
   * push the array's tag onto the tag stack. Finally the original arraylength instruction is
   * performed.
   *
   * @param inst an arraylength instruction
   * @return instruction list that calls the runtime to handle the arraylength instruction
   */
  private List<CodeElement> array_length(Instruction inst) {
    List<CodeElement> il = new ArrayList<>();

    // Duplicate the array ref and pass it to DCRuntime which will push
    // it onto the tag stack.
    il.add(StackInstruction.of(DUP));
    il.add(dcr_call("push_array_tag", CD_void, object_arg));

    // Perform the original instruction
    il.add(inst);

    return il;
  }

  /**
   * Creates code to make the declared length of a new array comparable to its index.
   *
   * @param inst a anewarray or newarray instruction
   * @return instruction list that calls the runtime to handle the newarray instruction
   */
  private List<CodeElement> new_array(Instruction inst) {
    List<CodeElement> il = new ArrayList<>();

    // Perform the original instruction
    il.add(inst);

    // Duplicate the array ref from the top of the stack and pass it
    // to DCRuntime which will push it onto the tag stack.
    il.add(StackInstruction.of(DUP));
    il.add(dcr_call("push_array_tag", CD_void, object_arg));

    // Make the array and the count comparable. Also, pop the tags for
    // the array and the count off the tag stack.
    il.add(dcr_call("cmp_op", CD_void, noArgsSig));

    return il;
  }

  /**
   * Creates code to make the declared lengths of a new two-dimensional array comparable to the
   * corresponding indices.
   *
   * @param inst a multianewarray instruction
   * @return instruction list that calls the runtime to handle the multianewarray instruction
   */
  private List<CodeElement> multiarray2(Instruction inst) {
    List<CodeElement> il = new ArrayList<>();

    // Duplicate both count arguments
    il.add(StackInstruction.of(DUP2));

    // Perform the original instruction
    il.add(inst);

    // Duplicate the new arrayref and put it below the count arguments
    // Stack is now: ..., arrayref, count1, count2, arrayref
    il.add(StackInstruction.of(DUP_X2));

    il.add(dcr_call("multianewarray2", CD_void, new ClassDesc[] {CD_int, CD_int, objectArrayCD}));

    return il;
  }

  /**
   * Returns true if this ppt should be included. A ppt is included if it matches ones of the select
   * patterns and doesn't match any of the omit patterns.
   *
   * @param className class to test
   * @param methodName method to test
   * @param pptName ppt to look for
   * @return true if this ppt should be included
   */
  boolean should_track(
      @BinaryName String className, @Identifier String methodName, String pptName) {

    debugInstrument.log("Considering tracking ppt: %s, %s, %s%n", className, methodName, pptName);

    // Don't track any JDK classes
    if (BcelUtil.inJdk(className)) {
      debug_transform.log("not including %s as it is a JDK class%n", className);
      return false;
    }

    // Don't track toString methods because we call them in
    // our debug statements.
    if (pptName.contains("toString")) {
      debug_transform.log("not including %s as it is a toString method%n", pptName);
      return false;
    }

    // Call `shouldIgnore` to check ppt-omit-patterns and ppt-select-patterns.
    return !daikon.chicory.Instrument24.shouldIgnore(className, methodName, pptName);
  }

  /**
   * Constructs a ppt entry name from a Method.
   *
   * @param fullClassName class name
   * @param mgen method
   * @return corresponding ppt name
   */
  static String methodEntryName(String fullClassName, MethodGen24 mgen) {

    // Get an array of the type names
    ClassDesc[] paramTypes = mgen.getParameterTypes();
    String[] type_names = new String[paramTypes.length];
    for (int ii = 0; ii < paramTypes.length; ii++) {
      @FieldDescriptor String paramFD = paramTypes[ii].descriptorString();
      type_names[ii] = daikon.chicory.Instrument24.convertDescriptorToFqBinaryName(paramFD);
    }

    return DaikonWriter.methodEntryName(fullClassName, type_names, "", mgen.getName());
  }

  /**
   * Constructs a call to a static method in DCRuntime.
   *
   * @param methodName method to call
   * @param returnType type of method return
   * @param paramTypes array of method parameter types
   * @return InvokeInstruction for the call
   */
  InvokeInstruction dcr_call(
      @Identifier String methodName, ClassDesc returnType, ClassDesc[] paramTypes) {
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            runtimeCD, methodName, MethodTypeDesc.of(returnType, paramTypes));
    return InvokeInstruction.of(INVOKESTATIC, mre);
  }

  /**
   * Create the code to call discard_tag(tag_count). If inst is not null, append it to the end of
   * that code.
   *
   * @param inst instruction to be replaced
   * @param tag_count number of tags to discard
   * @return instruction list to discard tag(s)
   */
  List<CodeElement> discard_tag_code(@Nullable CodeElement inst, int tag_count) {
    List<CodeElement> il = new ArrayList<>();
    il.add(loadIntegerConstant(tag_count));
    il.add(dcr_call("discard_tag", CD_void, intSig));
    if (inst != null) {
      il.add(inst);
    }
    return il;
  }

  /**
   * Duplicates a category 1 item on the top of stack. If it is a primitive, we need to do the same
   * to the tag stack. Otherwise, we do nothing.
   */
  @Nullable List<CodeElement> dup_tag(CodeElement inst, OperandStack24 stack) {
    ClassDesc top = stack.peek();
    if (debug_dup.enabled) {
      debug_dup.log("DUP -> %s [... %s]%n", "dup", stack_contents(stack, 2));
    }
    if (top.isPrimitive()) {
      return build_il(dcr_call("dup", CD_void, noArgsSig), inst);
    }
    return null;
  }

  /**
   * Duplicates the item on the top of the stack and inserts it 2 values down in the stack. If the
   * value at the top of the stack is not a primitive, there is nothing to do here. If the second
   * value is not a primitive, then we need only to insert the duped value down 1 on the tag stack
   * (which contains only primitives).
   */
  @Nullable List<CodeElement> dup_x1_tag(CodeElement inst, OperandStack24 stack) {
    ClassDesc top = stack.peek();
    String op;
    if (!top.isPrimitive()) {
      return null;
    } else if (stack.peek(1).isPrimitive()) {
      op = "dup_x1";
    } else {
      op = "dup";
    }
    if (debug_dup.enabled) {
      debug_dup.log("DUP_X1 -> %s [... %s]%n", op, stack_contents(stack, 2));
    }
    return build_il(dcr_call(op, CD_void, noArgsSig), inst);
  }

  /**
   * Dup the category 1 value on the top of the stack and insert it either two or three values down
   * on the stack.
   */
  @Nullable List<CodeElement> dup_x2_tag(CodeElement inst, OperandStack24 stack) {
    ClassDesc value1 = stack.peek();
    if (!value1.isPrimitive()) {
      return null;
    }
    ClassDesc value2 = stack.peek(1);
    String op;
    if (is_category2(value2)) {
      op = "dup_x1";
    } else {
      ClassDesc value3 = stack.peek(2);
      if (value2.isPrimitive() && value3.isPrimitive()) {
        op = "dup_x2";
      } else if (value2.isPrimitive() || value3.isPrimitive()) {
        op = "dup_x1";
      } else {
        op = "dup";
      }
    }
    if (debug_dup.enabled) {
      debug_dup.log("DUP_X2 -> %s [... %s]%n", op, stack_contents(stack, 3));
    }
    return build_il(dcr_call(op, CD_void, noArgsSig), inst);
  }

  /**
   * Duplicate either one category 2 value or two category 1 values. If the value(s) are primitives
   * we need to do the same to the tag stack. Otherwise, we do nothing.
   */
  @Nullable List<CodeElement> dup2_tag(CodeElement inst, OperandStack24 stack) {
    ClassDesc top = stack.peek();
    String op;
    if (is_category2(top)) {
      op = "dup";
    } else if (top.isPrimitive() && stack.peek(1).isPrimitive()) {
      op = "dup2";
    } else if (top.isPrimitive() || stack.peek(1).isPrimitive()) {
      op = "dup";
    } else {
      // both of the top two items are not primitive, nothing to dup
      return null;
    }
    if (debug_dup.enabled) {
      debug_dup.log("DUP2 -> %s [... %s]%n", op, stack_contents(stack, 2));
    }
    return build_il(dcr_call(op, CD_void, noArgsSig), inst);
  }

  /**
   * Duplicates either the top 2 category 1 values or a single category 2 value and inserts it 2 or
   * 3 values down on the stack.
   */
  @Nullable List<CodeElement> dup2_x1_tag(CodeElement inst, OperandStack24 stack) {
    ClassDesc value1 = stack.peek();
    ClassDesc value2 = stack.peek(1);
    String op;
    if (is_category2(value1)) {
      if (value2.isPrimitive()) {
        op = "dup_x1";
      } else { // not a primitive, so just dup
        op = "dup";
      }
    } else { // value1 is not category 2
      ClassDesc value3 = stack.peek(2);
      if (value1.isPrimitive()) {
        if (value2.isPrimitive() && value3.isPrimitive()) {
          op = "dup2_x1";
        } else if (value2.isPrimitive()) {
          op = "dup2";
        } else if (value3.isPrimitive()) {
          op = "dup_x1";
        } else {
          // neither value2 nor value3 is primitive
          op = "dup";
        }
      } else { // value1 is not primitive
        if (value2.isPrimitive() && value3.isPrimitive()) {
          op = "dup_x1";
        } else if (value2.isPrimitive()) {
          op = "dup";
        } else { // neither value2 or value3 is primitive
          return null;
        }
      }
    }
    if (debug_dup.enabled) {
      debug_dup.log("DUP2_X1 -> %s [... %s]%n", op, stack_contents(stack, 3));
    }
    return build_il(dcr_call(op, CD_void, noArgsSig), inst);
  }

  /**
   * Duplicate the top one or two operand stack values and insert two, three, or four values down.
   */
  @Nullable List<CodeElement> dup2_x2_tag(CodeElement inst, OperandStack24 stack) {
    ClassDesc value1 = stack.peek();
    ClassDesc value2 = stack.peek(1);
    String op;
    if (is_category2(value1)) {
      if (is_category2(value2)) {
        op = "dup_x1";
      } else {
        ClassDesc value3 = stack.peek(2);
        if (value2.isPrimitive() && value3.isPrimitive()) {
          op = "dup_x2";
        } else if (value2.isPrimitive() || value3.isPrimitive()) {
          op = "dup_x1";
        } else {
          // neither value2 or value3 is primitive
          op = "dup";
        }
      }
    } else { // value1 and value2 are not category 2
      ClassDesc value3 = stack.peek(2);
      if (value1.isPrimitive()) {
        if (is_category2(value2)) {
          throw new DynCompError("not supposed to happen " + stack_contents(stack, 3));
        } else if (is_category2(value3)) {
          if (value2.isPrimitive()) {
            op = "dup2_x1";
          } else {
            op = "dup_x1";
          }
        } else if (value2.isPrimitive()) {
          // value1 and value2 are primitive
          ClassDesc value4 = stack.peek(3);
          if (value3.isPrimitive() && value4.isPrimitive()) {
            op = "dup2_x2";
          } else if (value3.isPrimitive() || value4.isPrimitive()) {
            op = "dup2_x1";
          } else {
            // neither value3 or value4 is primitive
            op = "dup2";
          }
        } else { // value1 is primitive value2 is not primitive
          ClassDesc value4 = stack.peek(3);
          if (value3.isPrimitive() && value4.isPrimitive()) {
            op = "dup_x2";
          } else if (value3.isPrimitive() || value4.isPrimitive()) {
            op = "dup_x1";
          } else {
            // neither value3 or value4 is primitive
            op = "dup";
          }
        }
      } else { // value1 is not primitive
        if (is_category2(value2)) {
          throw new DynCompError("not supposed to happen " + stack_contents(stack, 3));
        } else if (is_category2(value3)) {
          if (value2.isPrimitive()) {
            op = "dup_x1";
          } else {
            return null; // nothing to dup
          }
        } else if (value2.isPrimitive()) {
          // value1 is not primitive value2 is primitive
          ClassDesc value4 = stack.peek(3);
          if (value3.isPrimitive() && value4.isPrimitive()) {
            op = "dup_x2";
          } else if (value3.isPrimitive() || value4.isPrimitive()) {
            op = "dup_x1";
          } else {
            // neither value3 or value4 is primitive
            op = "dup";
          }
        } else { // neither value1 or value2 is primitive
          return null; // nothing to dup
        }
      }
    }
    if (debug_dup.enabled) {
      debug_dup.log("DUP_X2 -> %s [... %s]%n", op, stack_contents(stack, 3));
    }
    return build_il(dcr_call(op, CD_void, noArgsSig), inst);
  }

  /**
   * Pops a category 1 value from the top of the stack. We want to discard the top of the tag stack
   * iff the item on the top of the stack is a primitive.
   */
  @Nullable List<CodeElement> pop_tag(CodeElement inst, OperandStack24 stack) {
    ClassDesc top = stack.peek();
    if (debug_dup.enabled) {
      debug_dup.log("POP -> %s [... %s]%n", "pop", stack_contents(stack, 1));
    }
    if (top.isPrimitive()) {
      return discard_tag_code(inst, 1);
    }
    return null;
  }

  /**
   * Pops either the top 2 category 1 values or a single category 2 value from the top of the stack.
   * We must do the same to the tag stack if the values are primitives.
   */
  @Nullable List<CodeElement> pop2_tag(CodeElement inst, OperandStack24 stack) {
    ClassDesc top = stack.peek();
    if (debug_dup.enabled) {
      debug_dup.log("POP2 -> %s [... %s]%n", "pop2", stack_contents(stack, 1));
    }
    if (is_category2(top)) {
      return discard_tag_code(inst, 1);
    } else {
      int cnt = 0;
      if (top.isPrimitive()) {
        cnt++;
      }
      if (stack.peek(1).isPrimitive()) {
        cnt++;
      }
      if (cnt > 0) {
        return discard_tag_code(inst, cnt);
      }
    }
    return null;
  }

  /**
   * Swaps the two category 1 values on the top of the stack. We need to swap the top of the tag
   * stack if the two top elements on the real stack are primitives.
   */
  @Nullable List<CodeElement> swap_tag(CodeElement inst, OperandStack24 stack) {
    ClassDesc type1 = stack.peek();
    ClassDesc type2 = stack.peek(1);
    if (debug_dup.enabled) {
      debug_dup.log("SWAP -> %s [... %s]%n", "swap", stack_contents(stack, 2));
    }
    if (type1.isPrimitive() && type2.isPrimitive()) {
      return build_il(dcr_call("swap", CD_void, noArgsSig), inst);
    }
    return null;
  }

  /**
   * Handle the instruction that allocates multi-dimensional arrays. If the new array has 2
   * dimensions, make the integer arguments comparable to the corresponding indices of the new
   * array. For any other number of dimensions, discard the tags for the arguments. Higher
   * dimensions should really be handled as well, but there are very few cases of this and the
   * resulting code would be quite complex (see multiarray2 for details).
   */
  private List<CodeElement> multi_newarray_dc(NewMultiArrayInstruction inst) {
    int dims = inst.dimensions();
    if (dims == 2) {
      return multiarray2(inst);
    } else {
      return discard_tag_code(inst, dims);
    }
  }

  /**
   * Create an instruction list that calls the runtime to handle returns for the tag stack followed
   * by the original return instruction.
   *
   * @param mgen method to check
   * @param inst return instruction to be replaced
   * @return the instruction list
   */
  private List<CodeElement> return_tag(MethodGen24 mgen, Instruction inst) {
    List<CodeElement> il = new ArrayList<>();

    ClassDesc type = mgen.getReturnType();

    // Push the tag frame
    il.add(LoadInstruction.of(TypeKind.REFERENCE, tagFrameLocal.slot()));

    if (is_primitive(type)) {
      il.add(dcr_call("normal_exit_primitive", CD_void, objectArrayCD_arg));
    } else {
      il.add(dcr_call("normal_exit", CD_void, objectArrayCD_arg));
    }
    il.add(inst);
    return il;
  }

  /**
   * Returns true if the specified type is a primitive (int, float, double, etc).
   *
   * @param type type to check
   * @return true if type is primitive
   */
  @Pure
  boolean is_primitive(ClassDesc type) {
    return type.isPrimitive() && !type.equals(CD_void);
  }

  /**
   * Returns true if the specified type is a category 2 (8 byte) type.
   *
   * @param type type to check
   * @return true if type requires 8 bytes
   */
  @Pure
  boolean is_category2(ClassDesc type) {
    return type.equals(CD_double) || type.equals(CD_long);
  }

  /**
   * Modify a doubled native method to call its original method. It pops all of the parameter tags
   * off of the tag stack. If there is a primitive return value it puts a new tag value on the stack
   * for it.
   *
   * <p>TODO: add a way to provide a synopsis for native methods that affect comparability.
   *
   * @param mgen the interface method. Must be native.
   */
  void fix_native(MethodGen24 mgen) {

    ClassDesc[] paramTypes = mgen.getParameterTypes();

    debug_native.log("Native call %s%n", mgen);

    // Discard the tags for any primitive arguments passed to the method.
    List<CodeElement> il = discard_primitive_tags(paramTypes);

    // push a tag if there is a primitive return value
    ClassDesc returnType = mgen.getReturnType();
    if (is_primitive(returnType)) {
      il.add(dcr_call("push_const", CD_void, noArgsSig));
    }

    // If the method is not static, push the instance on the stack
    if (!mgen.isStatic()) {
      il.add(LoadInstruction.of(TypeKind.REFERENCE, 0)); // load this
    }

    // if call is sun.reflect.Reflection.getCallerClass(int depth)
    // TODO: This method was deleted in JDK 9.  At some point we should remove support.
    if (mgen.getName().equals("getCallerClass")
        && (paramTypes.length == 1) // 'int depth'
        && mgen.getClassName().equals("sun.reflect.Reflection")) {

      // The call returns the class 'depth' frames up the stack. Since we have added
      // our wrapper method call in between, we need to increment that number by 1.
      il.add(LoadInstruction.of(TypeKind.INT, 0)); // load depth
      il.add(loadIntegerConstant(1));
      il.add(OperatorInstruction.of(IADD));
      // System.out.printf("adding 1 in %s.%s%n", mgen.getClassName(),
      //                   mgen.getName());

    } else { // normal call

      // push each argument on the stack
      int param_index = 1;
      if (mgen.isStatic()) {
        param_index = 0;
      }
      for (ClassDesc paramType : paramTypes) {
        il.add(LoadInstruction.of(TypeKind.from(paramType), param_index));
        param_index += TypeKind.from(paramType).slotSize();
      }
    }

    // Call the native method
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            ClassDesc.of(mgen.getClassName()),
            mgen.getName(),
            MethodTypeDesc.of(returnType, paramTypes));
    Opcode op = mgen.isStatic() ? INVOKESTATIC : INVOKEVIRTUAL;
    il.add(InvokeInstruction.of(op, mre));

    // generate return instruction
    il.add(ReturnInstruction.of(TypeKind.from(returnType)));

    mgen.setInstructionList(il);
  }

  /**
   * Convenience function to build an instruction list.
   *
   * @param instructions a variable number of instructions
   * @return an instruction list
   */
  private List<CodeElement> build_il(CodeElement... instructions) {
    return new ArrayList<>(Arrays.asList(instructions));
  }

  /**
   * Returns true if tag fields are used within the specified method of the specified class. We can
   * safely use class fields except in Object, String, and Class. If checking a class, mgen is null.
   *
   * @param mgen method to check
   * @param classname class containing {@code mgen}
   * @return true if tag fields may be used in class for method
   */
  boolean tag_fields_ok(@Nullable MethodGen24 mgen, @BinaryName String classname) {

    // Prior to Java 8 an interface could not contain any implementations.
    if (classGen.isInterface()) {
      if (classModel.majorVersion() < ClassFile.JAVA_8_VERSION) {
        return false;
      }
    }

    if (mgen != null) {
      if (mgen.getName().equals("<init>")) {
        if (!this.constructor_is_initialized) {
          return false;
        }
      }
    }

    if (!Premain.jdk_instrumented) {
      if (BcelUtil.inJdk(classname)) {
        return false;
      }
    }

    if (!classname.startsWith("java.lang")) {
      return true;
    }

    return !(classname.equals("java.lang.String")
        || classname.equals("java.lang.Class")
        || classname.equals("java.lang.Object")
        || classname.equals("java.lang.ClassLoader"));
  }

  /**
   * Returns a string describing the top max_items items on the stack.
   *
   * @param stack OperandStack
   * @param max_items number of items to describe
   * @return string describing the top max_items on the operand stack
   */
  static String stack_contents(OperandStack24 stack, int max_items) {
    String contents = "";
    if (max_items >= stack.size()) {
      max_items = stack.size() - 1;
    }
    for (int ii = max_items; ii >= 0; ii--) {
      if (contents.length() != 0) {
        contents += ", ";
      }
      contents += stack.peek(ii);
    }
    return contents;
  }

  /**
   * Creates tag get and set accessor methods for each field in the class. An accessor is created
   * for each field (including final, static, and private fields). The accessors share the modifiers
   * of their field (except that all are final). Accessors are named {@code
   * <field>_<class>__$get_tag} and {@code <field>_<class>__$set_tag}. The class name must be
   * included because field names can shadow one another.
   *
   * <p>If tag_fields_ok is true for the class, then tag fields are created and the accessor uses
   * the tag fields. If not, tag storage is created separately and accessed via the field number.
   *
   * <p>Accessors are also created for each visible superclass field that is not hidden by a field
   * in this class. These accessors just call the superclasses accessor.
   *
   * <p>Any accessors created are added to the class.
   *
   * @param classGen class to check for fields
   */
  void create_tag_accessors(ClassGen24 classGen) {

    // If this class doesn't support tag fields, don't create them
    if (!tag_fields_ok(null, classGen.getClassName())) {
      return;
    }

    Set<String> field_set = new HashSet<>();
    Map<FieldModel, Integer> field_to_offset_map = build_field_to_offset_map(classModel);

    // Build accessors for all fields declared in this class
    for (FieldModel fm : classModel.fields()) {

      String fieldName = fm.fieldName().stringValue();
      assert !field_set.contains(fieldName) : fieldName + "-" + classGen.getClassName();
      field_set.add(fieldName);

      // skip primitive fields
      // MLR: skip non primitive fields?
      if (!fm.fieldTypeSymbol().isPrimitive()) {
        continue;
      }

      @SuppressWarnings("nullness:unboxing.of.nullable")
      int tagOffset =
          fm.flags().has(AccessFlag.STATIC)
              ? static_field_id.get(full_name(classModel, fm))
              : field_to_offset_map.get(fm);
      create_get_tag(classGen, fm, tagOffset);
      create_set_tag(classGen, fm, tagOffset);
    }

    // Build accessors for each field declared in a superclass that is
    // is not shadowed in a subclass
    for (@BinaryName String scn : getStrictSuperclassNames()) {
      ClassModel scm = getClassModel(scn);
      if (scm == null) {
        throw new DynCompError("Can't load ClassModel for: " + scn);
      }

      for (FieldModel fm : scm.fields()) {
        if (fm.flags().has(AccessFlag.PRIVATE)) {
          continue;
        }
        if (field_set.contains(fm.fieldName().stringValue())) {
          continue;
        }
        if (!fm.fieldTypeSymbol().isPrimitive()) {
          continue;
        }

        field_set.add(fm.fieldName().stringValue());
        @SuppressWarnings("nullness:unboxing.of.nullable")
        int tagOffset =
            fm.flags().has(AccessFlag.STATIC)
                ? static_field_id.get(full_name(scm, fm))
                : field_to_offset_map.get(fm);
        create_get_tag(classGen, fm, tagOffset);
        create_set_tag(classGen, fm, tagOffset);
      }
    }
  }

  /**
   * Builds a Map that relates each field in jc and each of its superclasses to a unique offset. The
   * offset can be used to index into a tag array for this class. Instance fields are placed in the
   * returned map and static fields are placed in static map (shared between all classes).
   *
   * @param classModel class to check for fields
   * @return field offset map
   */
  Map<FieldModel, Integer> build_field_to_offset_map(ClassModel classModel) {

    Optional<ClassEntry> ce = classModel.superclass();
    if (!ce.isPresent()) {
      // class is java.lang.Object, no primitive fields
      return new LinkedHashMap<>();
    }

    // Get the offsets for each field in the superclass.
    String superclassName = ce.get().asInternalName().replace('/', '.');
    ClassModel super_cm = getClassModel(superclassName);
    if (super_cm == null) {
      throw new DynCompError("Can't get superclass for " + superclassName);
    }

    Map<FieldModel, Integer> field_to_offset_map = build_field_to_offset_map(super_cm);
    int offset = field_to_offset_map.size();

    // Determine the offset for each primitive field in the class
    // Also make sure the static_tags list is large enough for all
    // of the tags.
    for (FieldModel fm : classModel.fields()) {
      if (!fm.fieldTypeSymbol().isPrimitive()) {
        continue;
      }
      if (fm.flags().has(AccessFlag.STATIC)) {
        if (!in_jdk) {
          int min_size = static_field_id.size() + DCRuntime.max_jdk_static;
          while (DCRuntime.static_tags.size() <= min_size) {
            DCRuntime.static_tags.add(null);
          }
          static_field_id.put(full_name(classModel, fm), min_size);
        } else { // building jdk
          String full_name = full_name(classModel, fm);
          if (static_field_id.containsKey(full_name)) {
            // System.out.printf("Reusing static field %s value %d%n",
            //                    full_name, static_field_id.get(full_name));
          } else {
            // System.out.printf("Allocating new static field %s%n",
            //                    full_name);
            static_field_id.put(full_name, static_field_id.size() + 1);
          }
        }
      } else {
        field_to_offset_map.put(fm, offset);
        offset++;
      }
    }

    return field_to_offset_map;
  }

  /**
   * Creates a get tag method for field fm. The tag corresponding to field fm will be pushed on the
   * tag stack.
   *
   * <pre>{@code
   * void <field>_<class>__$get_tag() {
   *   #if fm.isStatic()
   *     DCRuntime.push_static_tag (tag_offset)
   *   #else
   *     DCRuntime.push_field_tag (this, tag_offset);
   * }
   * }</pre>
   *
   * @param classGen class whose accessors are being built. Not necessarily the class declaring fm
   *     (if fm is inherited).
   * @param fm field to build an accessor for
   * @param tag_offset offset of fm in the tag storage for this field
   */
  void create_get_tag(ClassGen24 classGen, FieldModel fm, int tag_offset) {

    // Determine the method to call in DCRuntime.  Instance fields and static
    // fields are handled separately.  Also instance fields in special
    // classes that are created by the JVM are handled separately since only
    // in those classes can fields be read without being written (in java)
    String classname = classGen.getClassName();
    String methodname = "push_field_tag";
    ClassDesc[] params;
    final boolean isStatic = fm.flags().has(AccessFlag.STATIC) ? true : false;

    if (isStatic) {
      methodname = "push_static_tag";
      params = new ClassDesc[] {CD_int};
    } else if (is_class_initialized_by_jvm(classname)) {
      methodname = "push_field_tag_null_ok";
      params = new ClassDesc[] {CD_Object, CD_int};
    } else {
      methodname = "push_field_tag";
      params = new ClassDesc[] {CD_Object, CD_int};
    }

    String accessor_name =
        Premain.tag_method_name(Premain.GET_TAG, classname, fm.fieldName().stringValue());

    List<CodeElement> newCode = new ArrayList<>();

    newCode.add(loadIntegerConstant(tag_offset));
    newCode.add(dcr_call(methodname, CD_void, params));
    newCode.add(ReturnInstruction.of(TypeKind.VOID));

    int access_flags = fm.flags().flagsMask();
    if (classGen.isInterface()) {
      // method in interface cannot be final
      access_flags &= ~AccessFlag.FINAL.mask();
      if (classModel.majorVersion() < ClassFile.JAVA_8_VERSION) {
        // If class file version is prior to 8 then a method in an interface
        // cannot be static (it's implicit) and must be abstract.
        access_flags &= ~AccessFlag.STATIC.mask();
        access_flags |= AccessFlag.ABSTRACT.mask();
      }
    } else {
      access_flags |= AccessFlag.FINAL.mask();
    }

    // make method public
    access_flags &= ~ACC_PRIVATE;
    access_flags &= ~ACC_PROTECTED;
    access_flags |= ACC_PUBLIC;

    // Create the get accessor method
    classGen
        .getClassBuilder()
        .withMethod(
            accessor_name,
            MethodTypeDesc.of(CD_void),
            access_flags,
            methodBuilder ->
                methodBuilder.withCode(
                    codeBuilder -> buildTagAccessor(codeBuilder, newCode, isStatic, classname)));
  }

  /**
   * Creates a set tag method for field fm. The tag on the top of the tag stack will be popped off
   * and placed in the tag storeage corresponding to field
   *
   * <pre>{@code
   * void <field>_<class>__$set_tag() {
   *   #if fm.isStatic()
   *     DCRuntime.pop_static_tag (tag_offset)
   *   #else
   *     DCRuntime.pop_field_tag (this, tag_offset);
   * }
   * }</pre>
   *
   * @param classGen class whose accessors are being built. Not necessarily the class declaring fm
   *     (if fm is inherited).
   * @param fm field to build an accessor for
   * @param tag_offset offset of fm in the tag storage for this field
   */
  void create_set_tag(ClassGen24 classGen, FieldModel fm, int tag_offset) {

    String classname = classGen.getClassName();
    String methodname = "pop_field_tag";
    ClassDesc[] params = {CD_Object, CD_int};
    final boolean isStatic = fm.flags().has(AccessFlag.STATIC) ? true : false;

    if (isStatic) {
      methodname = "pop_static_tag";
      params = new ClassDesc[] {CD_int};
    }

    String setter_name =
        Premain.tag_method_name(Premain.SET_TAG, classname, fm.fieldName().stringValue());

    List<CodeElement> newCode = new ArrayList<>();

    newCode.add(loadIntegerConstant(tag_offset));
    newCode.add(dcr_call(methodname, CD_void, params));
    newCode.add(ReturnInstruction.of(TypeKind.VOID));

    int access_flags = fm.flags().flagsMask();
    if (classGen.isInterface()) {
      // method in interface cannot be final
      access_flags &= ~AccessFlag.FINAL.mask();
      if (classModel.majorVersion() < ClassFile.JAVA_8_VERSION) {
        // If class file version is prior to 8 then a method in an interface
        // cannot be static (it's implicit) and must be abstract.
        access_flags &= ~AccessFlag.STATIC.mask();
        access_flags |= AccessFlag.ABSTRACT.mask();
      }
    } else {
      access_flags |= AccessFlag.FINAL.mask();
    }

    // Create the setter method.
    classGen
        .getClassBuilder()
        .withMethod(
            setter_name,
            MethodTypeDesc.of(CD_void),
            access_flags,
            methodBuilder ->
                methodBuilder.withCode(
                    codeBuilder -> buildTagAccessor(codeBuilder, newCode, isStatic, classname)));
  }

  /**
   * Build a tag accessor method.
   *
   * @param codeBuilder for the given method's code
   * @param instructions instruction list to copy
   * @param isStatic true iff accessor is static
   * @param classname classname holding accessor
   */
  private void buildTagAccessor(
      CodeBuilder codeBuilder, List<CodeElement> instructions, boolean isStatic, String classname) {

    Label startLabel = codeBuilder.newLabel();
    Label endLabel = codeBuilder.newLabel();
    if (!isStatic) {
      codeBuilder.localVariable(0, "this", ClassDesc.of(classname), startLabel, endLabel);
      codeBuilder.labelBinding(startLabel);
      codeBuilder.with(LoadInstruction.of(TypeKind.REFERENCE, 0)); // aload_0 (load this)
    }
    for (CodeElement ce : instructions) {
      debugInstrument.log("CodeElement: %s%n", ce);
      codeBuilder.with(ce);
    }
    codeBuilder.labelBinding(endLabel); // shouldn't matter if isStatic
  }

  /**
   * Adds the DCompInstrumented interface to the given class. Also adds the following method to the
   * class, so that it implements the DCompInstrumented interface:
   *
   * <pre>{@code
   * public boolean equals_dcomp_instrumented(Object o) {
   *   return this.equals(o, null);
   * }
   * }</pre>
   *
   * The method does nothing except call the instrumented equals method {@code boolean
   * equals(Object, DCompMarker)}.
   *
   * @param classBuilder for the class
   * @param classGen class to add method to
   */
  void add_dcomp_interface(ClassBuilder classBuilder, ClassGen24 classGen, ClassInfo classInfo) {

    classGen.addInterface(DCRuntime.instrumentation_interface);
    debugInstrument.log("Added interface DCompInstrumented%n");

    List<CodeElement> instructions = new ArrayList<>();
    List<myLocalVariable> localsTable = new ArrayList<>();
    Consumer<? super MethodBuilder> codeHandler1 =
        methodBuilder -> {
          methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, instructions, localsTable));
        };

    int access_flags = ACC_PUBLIC;
    if (classGen.isInterface()) {
      access_flags |= ACC_ABSTRACT;
      codeHandler1 = methodBuilder -> {};
    }

    // Defining local variables is not strictly necessary, but we do
    // it to reduce the diffs with previous versions of DynComp.
    localsTable.add(new myLocalVariable(0, "this", ClassDesc.of(classInfo.class_name)));
    localsTable.add(new myLocalVariable(1, "obj", CD_Object));

    MethodTypeDesc mtdNormal, mtdDComp;
    mtdNormal = MethodTypeDesc.of(CD_boolean, CD_Object);

    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 0)); // load this
    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 1)); // load obj

    if (!classInfo.isJunitTestClass) {
      instructions.add(ConstantInstruction.ofIntrinsic(ACONST_NULL)); // use null for marker
      mtdDComp = MethodTypeDesc.of(CD_boolean, CD_Object, dcomp_marker);
    } else {
      // for JUnit test class, the instrumented version has no dcomp arg
      mtdDComp = mtdNormal;
    }

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(ClassDesc.of(classGen.getClassName()), "equals", mtdDComp);
    instructions.add(InvokeInstruction.of(INVOKEVIRTUAL, mre));
    instructions.add(ReturnInstruction.of(TypeKind.BOOLEAN));

    if (!classInfo.isJunitTestClass) {
      // build the uninstrumented equals_dcomp_instrumented method
      classBuilder.withMethod("equals_dcomp_instrumented", mtdNormal, access_flags, codeHandler1);
    }

    // now build the instrumented version of the equals_dcomp_instrumented method
    if (classGen.isInterface()) {
      // no code if interface
      classBuilder.withMethod(
          "equals_dcomp_instrumented", mtdDComp, access_flags, methodBuilder -> {});
    } else {
      @BinaryName String classname = classInfo.class_name;
      // create pseudo MethodGen24
      MethodGen24 mgen =
          new MethodGen24(
              classname,
              classBuilder,
              "equals_dcomp_instrumented",
              access_flags,
              mtdNormal,
              instructions,
              3, // maxStack
              2); // maxLocals
      boolean track = should_track(classname, mgen.getName(), methodEntryName(classname, mgen));
      classBuilder.withMethod(
          "equals_dcomp_instrumented",
          mtdDComp,
          access_flags,
          methodBuilder ->
              methodBuilder.withCode(
                  codeBuilder ->
                      instrumentCode(codeBuilder, null, localsTable, mgen, classInfo, track)));
    }
  }

  /**
   * Adds the following method to a class:
   *
   * <pre>{@code
   * public boolean equals (Object obj) {
   *   return super.equals(obj);
   * }
   * }</pre>
   *
   * Throws a ClassFormatError if the equals method is already defined in the class.
   *
   * @param classBuilder for the class
   * @param classGen class to add method to
   */
  void add_equals_method(ClassBuilder classBuilder, ClassGen24 classGen, ClassInfo classInfo) {

    List<CodeElement> instructions = new ArrayList<>();
    List<myLocalVariable> localsTable = new ArrayList<>();
    Consumer<? super MethodBuilder> codeHandler1 =
        methodBuilder -> {
          methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, instructions, localsTable));
        };

    int access_flags = ACC_PUBLIC;
    if (classGen.isInterface()) {
      access_flags |= ACC_ABSTRACT;
      codeHandler1 = methodBuilder -> {};
    }

    // Defining local variables is not strictly necessary, but we do
    // it to reduce the diffs with previous versions of DynComp.
    localsTable.add(new myLocalVariable(0, "this", ClassDesc.of(classInfo.class_name)));
    localsTable.add(new myLocalVariable(1, "obj", CD_Object));

    MethodTypeDesc mtdNormal, mtdDComp;
    mtdNormal = MethodTypeDesc.of(CD_boolean, CD_Object);

    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 0)); // load this
    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 1)); // load obj
    MethodRefEntry mre =
        poolBuilder.methodRefEntry(ClassDesc.of(classGen.getSuperclassName()), "equals", mtdNormal);
    instructions.add(InvokeInstruction.of(INVOKESPECIAL, mre));
    instructions.add(ReturnInstruction.of(TypeKind.BOOLEAN));

    if (!classInfo.isJunitTestClass) {
      // build the uninstrumented equals method
      classBuilder.withMethod("equals", mtdNormal, access_flags, codeHandler1);
      // since not JUnit test class, add dcomp_marker to instrumented version of equals built below
      mtdDComp = MethodTypeDesc.of(CD_boolean, CD_Object, dcomp_marker);
    } else {
      // for JUnit test class, we build only the instrumented version with no dcomp arg
      mtdDComp = mtdNormal;
    }

    // now build the instrumented version of the equals method
    if (classGen.isInterface()) {
      // no code if interface
      classBuilder.withMethod("equals", mtdDComp, access_flags, methodBuilder -> {});
    } else {
      @BinaryName String classname = classInfo.class_name;
      // create pseudo MethodGen24
      MethodGen24 mgen =
          new MethodGen24(
              classname, classBuilder, "equals", access_flags, mtdNormal, instructions, 2, 2);
      boolean track = should_track(classname, mgen.getName(), methodEntryName(classname, mgen));
      classBuilder.withMethod(
          "equals",
          mtdDComp,
          access_flags,
          methodBuilder ->
              methodBuilder.withCode(
                  codeBuilder ->
                      instrumentCode(codeBuilder, null, localsTable, mgen, classInfo, track)));
    }
  }

  /**
   * Adds interfaces to indicate which of the Object methods (currently clone and toString) the
   * class overrides. Callers will call the instrumented version of the method if it exists,
   * otherwise they will call the uninstrumented version.
   *
   * @param classGen class to check
   */
  void add_clone_and_tostring_interfaces(ClassGen24 classGen) {

    @SuppressWarnings("signature:assignment")
    @MethodDescriptor String noArgsReturnObject = "()Ljava/lang/Object;";
    MethodModel cl = classGen.containsMethod("clone", noArgsReturnObject);
    if (cl != null) {
      classGen.addInterface(Signatures.addPackage(dcompRuntimePrefix, "DCompClone"));
    }

    @SuppressWarnings("signature:assignment")
    @MethodDescriptor String noArgsReturnString = "()Ljava/lang/String;";
    MethodModel ts = classGen.containsMethod("toString", noArgsReturnString);
    if (ts != null) {
      classGen.addInterface(Signatures.addPackage(dcompRuntimePrefix, "DCompToString"));
    }
  }

  /**
   * Add a dcomp marker parameter to indicate this is the instrumented version of the method.
   *
   * @param mgen method to add dcomp marker to
   * @param minfo for the given method's code
   */
  void add_dcomp_param(MethodGen24 mgen, MethodGen24.MInfo24 minfo) {

    // Don't modify main or the JVM won't be able to find it.
    if (mgen.isMain()) {
      return;
    }

    // Don't modify class init methods, they don't take arguments
    if (mgen.isClinit()) {
      return;
    }

    // Add the dcomp marker parameter to indicate this is the
    // instrumented version of the method.
    StackMapUtils24.addNewSpecialLocal(mgen, minfo, "marker", dcomp_marker, true);
  }

  /**
   * Returns true if the method is defined in Object.
   *
   * @param methodName method to check
   * @param paramTypes array of parameter types to method
   * @return true if method is member of Object
   */
  @Pure
  boolean is_object_method(@Identifier String methodName, ClassDesc[] paramTypes) {
    // Note: kind of weird we don't check that classname = Object but it's been
    // that way forever. Just means foo.finialize(), e.g., will be marked uninstrumented.
    for (MethodDef md : obj_methods) {
      if (md.equals(methodName, paramTypes)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns true if the class is one of those that has values initialized by the JVM or native
   * methods.
   *
   * @param classname class to check
   * @return true if classname has members that are uninitialized
   */
  @Pure
  boolean is_class_initialized_by_jvm(String classname) {

    for (String u_name : uninit_classes) {
      if (u_name.equals(classname)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Creates a pseudo "main" method with a DcompMarker parameter that does nothing but call the
   * original "main" method without the DCompMarker argument.
   *
   * @param mgen describes the "main" method
   * @param classBuilder for current class
   * @param classInfo for the given class
   */
  void createMainStub(MethodGen24 mgen, ClassBuilder classBuilder, ClassInfo classInfo) {
    List<CodeElement> instructions = new ArrayList<>();
    instructions.add(LoadInstruction.of(TypeKind.REFERENCE, 0)); // load arg0

    MethodRefEntry mre =
        poolBuilder.methodRefEntry(
            ClassDesc.of(classInfo.class_name),
            "main",
            MethodTypeDesc.of(CD_void, CD_String.arrayType(1)));
    instructions.add(InvokeInstruction.of(INVOKESTATIC, mre)); // call real main
    instructions.add(ReturnInstruction.of(TypeKind.VOID));

    classBuilder.withMethod(
        "main",
        MethodTypeDesc.of(CD_void, CD_String.arrayType(1), dcomp_marker),
        mgen.getAccessFlagsMask(),
        methodBuilder ->
            methodBuilder.withCode(codeBuilder -> copyCode(codeBuilder, instructions, null)));
  }

  /**
   * Writes the static map from field names to their integer ids to the specified file. Can be read
   * with restore_static_field_id. Each line contains a key/value combination with a blank
   * separating them.
   *
   * @param file where to write the static field ids
   * @throws IOException if unable to find or open the file
   */
  static void save_static_field_id(File file) throws IOException {

    PrintStream ps = new PrintStream(file, UTF_8);
    for (Map.Entry<@KeyFor("static_field_id") String, Integer> entry : static_field_id.entrySet()) {
      ps.printf("%s  %d%n", entry.getKey(), entry.getValue());
    }
    ps.close();
  }

  /**
   * Restores the static map from the specified file.
   *
   * @param file where to read the static field ids
   * @throws IOException if unable to create an EntryReader
   * @see #save_static_field_id(File)
   */
  static void restore_static_field_id(File file) throws IOException {
    try (EntryReader er = new EntryReader(file, "UTF-8")) {
      for (String line : er) {
        String[] key_val = line.split("  *");
        assert !static_field_id.containsKey(key_val[0]) : key_val[0] + " " + key_val[1];
        static_field_id.put(key_val[0], Integer.valueOf(key_val[1]));
      }
    }
  }

  /**
   * Returns the fully-qualified fieldname of the specified field.
   *
   * @param cm class containing the field
   * @param fm the field
   * @return string containing the fully-qualified name
   */
  protected static String full_name(ClassModel cm, FieldModel fm) {
    return ClassGen24.getClassName(cm) + "." + fm.fieldName().stringValue();
  }

  /**
   * Returns simplified name of a method. Both exceptions and annotations are removed.
   *
   * @param mgen the method
   * @return string containing the simplified method name
   */
  protected String simplify_method_name(MethodGen24 mgen) {
    // Remove exceptions from the full method name
    // String full_name = m.toString().replaceFirst("\\s*throws.*", "");
    // Remove annotations from full method name
    // return full_name.replaceFirst("(?s) \\[.*", "");
    return mgen.toString();
  }

  /** Used for processing a switch instruction. */
  private record ModifiedSwitchInfo(Label modifiedTarget, List<SwitchCase> modifiedCaseList) {}

  /**
   * Checks to see if the instruction targets the method's CodeModel startLabel (held in
   * oldStartLabel). If so, it replaces the target with the newStartLabel. Unfortunately, the class
   * file API does not allow us to simply replace the label, we have to replace the entire
   * instruction. Note that oldStartLabel may be null, but that is okay as any comparison to it will
   * fail and we will do nothing.
   *
   * @param inst the instruction to check
   * @return the original instruction or its replacement
   */
  @RequiresNonNull("newStartLabel")
  private CodeElement retargetStartLabel(CodeElement inst) {
    ModifiedSwitchInfo info;
    switch (inst) {
      case BranchInstruction bi -> {
        if (bi.target().equals(oldStartLabel)) {
          return BranchInstruction.of(bi.opcode(), newStartLabel);
        }
      }
      case ExceptionCatch ec -> {
        if (ec.tryStart().equals(oldStartLabel)) {
          return ExceptionCatch.of(ec.handler(), newStartLabel, ec.tryEnd(), ec.catchType());
        }
      }
      case LookupSwitchInstruction ls -> {
        info = retargetStartLabel(ls.defaultTarget(), ls.cases());
        if (info != null) {
          return LookupSwitchInstruction.of(info.modifiedTarget, info.modifiedCaseList);
        }
      }
      case TableSwitchInstruction ts -> {
        info = retargetStartLabel(ts.defaultTarget(), ts.cases());
        if (info != null) {
          return TableSwitchInstruction.of(
              ts.lowValue(), ts.highValue(), info.modifiedTarget, info.modifiedCaseList);
        }
      }
      default -> {}
    }
    return inst;
  }

  /**
   * Checks to see if a switch instruction's default target or any of the case targets refer to
   * oldStartLabel. If so, replace those targets with the newStartLabel, and return the result in a
   * ModifiedSwitchInfo. Otherwise, return null.
   *
   * @param defaultTarget the default target for the switch instruction
   * @param caseList the case list for the switch instruction
   * @return a ModifiedSwitchInfo with the changed values, or null if no changes
   */
  @RequiresNonNull("newStartLabel")
  private @Nullable ModifiedSwitchInfo retargetStartLabel(
      Label defaultTarget, List<SwitchCase> caseList) {
    Label modifiedTarget;
    boolean modified = false;

    if (defaultTarget.equals(oldStartLabel)) {
      modifiedTarget = newStartLabel;
      modified = true;
    } else {
      modifiedTarget = defaultTarget;
    }

    List<SwitchCase> newCaseList = new ArrayList<>();
    for (SwitchCase item : caseList) {
      if (item.target().equals(oldStartLabel)) {
        newCaseList.add(SwitchCase.of(item.caseValue(), newStartLabel));
        modified = true;
      } else {
        newCaseList.add(item);
      }
    }

    if (modified) {
      return new ModifiedSwitchInfo(modifiedTarget, newCaseList);
    } else {
      return null;
    }
  }

  /**
   * Create a new local variable whose scope is the full method.
   *
   * @param mgen describes the given method
   * @param minfo for the given method's code
   * @param localName name of new local variable
   * @param localType type of new local variable
   * @return the new local variable
   */
  protected LocalVariable createLocalWithMethodScope(
      MethodGen24 mgen, MethodGen24.MInfo24 minfo, String localName, ClassDesc localType) {
    LocalVariable newVar =
        LocalVariable.of(
            minfo.nextLocalIndex, localName, localType, minfo.startLabel, minfo.endLabel);
    mgen.localsTable.add(newVar);
    minfo.nextLocalIndex += TypeKind.from(localType).slotSize();
    mgen.setMaxLocals(minfo.nextLocalIndex);
    return newVar;
  }

  /**
   * Build a dup instruction based on item size.
   *
   * @param size size of item to be duplicated
   * @return a DUP or DUP2 instruction
   */
  protected CodeElement buildDUPInstruction(int size) {
    if (size == 1) {
      return StackInstruction.of(DUP);
    } else {
      return StackInstruction.of(DUP2);
    }
  }

  /**
   * Build a load constant instruction (LDC). Checks the offset of the constant pool element to be
   * loaded and generates a LDC or LDC_W, as needed.
   *
   * @param entry describes the constant pool element to be loaded
   * @return a LDC instruction
   */
  protected CodeElement buildLDCInstruction(LoadableConstantEntry entry) {
    if (entry.index() > 255) {
      return ConstantInstruction.ofLoad(LDC_W, entry);
    } else {
      return ConstantInstruction.ofLoad(LDC, entry);
    }
  }

  /**
   * Build a load constant instruction for values of type int, short, char, byte
   *
   * @param value to be pushed
   * @return a push instruction
   */
  protected CodeElement loadIntegerConstant(final int value) {
    return switch (value) {
      case -1 -> ConstantInstruction.ofIntrinsic(ICONST_M1);
      case 0 -> ConstantInstruction.ofIntrinsic(ICONST_0);
      case 1 -> ConstantInstruction.ofIntrinsic(ICONST_1);
      case 2 -> ConstantInstruction.ofIntrinsic(ICONST_2);
      case 3 -> ConstantInstruction.ofIntrinsic(ICONST_3);
      case 4 -> ConstantInstruction.ofIntrinsic(ICONST_4);
      case 5 -> ConstantInstruction.ofIntrinsic(ICONST_5);
      default ->
          (value >= Byte.MIN_VALUE && value <= Byte.MAX_VALUE)
              ? ConstantInstruction.ofArgument(BIPUSH, value)
              : (value >= Short.MIN_VALUE && value <= Short.MAX_VALUE)
                  ? ConstantInstruction.ofArgument(SIPUSH, value)
                  : buildLDCInstruction(poolBuilder.intEntry(value));
    };
  }
}
