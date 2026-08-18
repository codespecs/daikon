package daikon.dcomp;

import static java.lang.constant.ConstantDescs.CD_String;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassHierarchyResolver;
import java.lang.classfile.TypeKind;
import java.lang.classfile.instruction.LoadInstruction;
import java.lang.classfile.instruction.StoreInstruction;
import java.lang.constant.ClassDesc;
import org.checkerframework.checker.nullness.qual.EnsuresNonNull;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.Test;

/**
 * Tests of {@link CalcStack24}, the operand stack simulator used by {@link DCInstrument24}.
 *
 * <p>Note that this test must be located in daikon.dcomp rather than daikon.test.dcomp as it needs
 * to access protected fields of DCInstrument24.
 */
public class CalcStackTest24 {

  /** An instance of DCInstrument24 for testing. */
  @MonotonicNonNull DCInstrument24 dci;

  /**
   * Sets up the per-method simulation state that {@link CalcStack24} reads and writes, for a method
   * with {@code maxLocals} local variable slots, none of whose types are known.
   *
   * @param codeElementCount the number of code elements in the simulated method
   * @param maxLocals the number of local variable slots in the simulated method
   */
  @EnsuresNonNull("dci")
  private void initializeSimulationState(int codeElementCount, int maxLocals) {
    ClassLoader loader = DCInstrumentTest24.class.getClassLoader();
    assert loader != null : "@AssumeAssertion(nullness): this class is not in the boot classpath";
    ClassFile classFile =
        ClassFile.of(
            ClassFile.ClassHierarchyResolverOption.of(
                ClassHierarchyResolver.ofResourceParsing(loader)));
    @SuppressWarnings("nullness:argument") // Will only call CalcStack24.simulateInstruction.
    DCInstrument24 dci_tmp = new DCInstrument24(classFile, null, false);
    dci = dci_tmp;
    dci.stacks = new OperandStack24[codeElementCount];
    dci.locals = new @Nullable ClassDesc[maxLocals];
  }

  /**
   * A class compiled without {@code -g} has no {@code LocalVariableTable}, so the type of a local
   * variable other than {@code this} and the parameters is unknown. Loading such a local must not
   * put Java {@code null} on the simulated operand stack, because the callers of {@link
   * OperandStack24#peek} dereference the result.
   */
  @Test
  public void testAloadOfLocalWithUnknownType() {
    initializeSimulationState(1, 2);
    OperandStack24 stack = new OperandStack24(1);

    CalcStack24.simulateInstruction(dci, LoadInstruction.of(TypeKind.REFERENCE, 1), 0, stack);

    assertEquals(1, stack.size());
    assertEquals(CalcStack24.NULL_CD, stack.peek());
    assertFalse(stack.peek().isPrimitive());
  }

  /** An ALOAD of a local variable whose type is known pushes that type. */
  @Test
  public void testAloadOfLocalWithKnownType() {
    initializeSimulationState(1, 2);
    dci.locals[1] = CD_String;
    OperandStack24 stack = new OperandStack24(1);

    CalcStack24.simulateInstruction(dci, LoadInstruction.of(TypeKind.REFERENCE, 1), 0, stack);

    assertEquals(1, stack.size());
    assertEquals(CD_String, stack.peek());
  }

  /**
   * An ASTORE records the type of the stored value, so a later ALOAD of the same slot yields that
   * type even when there is no {@code LocalVariableTable} entry for the slot.
   */
  @Test
  public void testAloadAfterAstore() {
    initializeSimulationState(2, 2);
    OperandStack24 stack = new OperandStack24(1);
    stack.push(CD_String);

    CalcStack24.simulateInstruction(dci, StoreInstruction.of(TypeKind.REFERENCE, 1), 0, stack);
    assertEquals(0, stack.size());

    CalcStack24.simulateInstruction(dci, LoadInstruction.of(TypeKind.REFERENCE, 1), 1, stack);
    assertEquals(1, stack.size());
    assertEquals(CD_String, stack.peek());
  }
}
