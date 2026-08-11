package daikon.dcomp;

import static java.lang.constant.ConstantDescs.CD_String;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.lang.classfile.TypeKind;
import java.lang.classfile.instruction.LoadInstruction;
import java.lang.classfile.instruction.StoreInstruction;
import java.lang.constant.ClassDesc;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.Test;

/**
 * Tests of {@link CalcStack24}, the operand stack simulator used by {@link DCInstrument24}.
 *
 * Note that this test must be located in daikon.dcomp rather than daikon.test.dcomp as it
 * needs to access protected fields of DCInstrument24.
 */
public class CalcStackTest24 {

  /**
   * Sets up the per-method simulation state that {@link CalcStack24} reads and writes, for a method
   * with {@code maxLocals} local variable slots, none of whose types are known.
   *
   * @param codeElementCount the number of code elements in the simulated method
   * @param maxLocals the number of local variable slots in the simulated method
   */
  private void initializeSimulationState(int codeElementCount, int maxLocals) {
    DCInstrument24.stacks = new OperandStack24[codeElementCount];
    DCInstrument24.locals = new @Nullable ClassDesc[maxLocals];
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

    CalcStack24.simulateInstruction(LoadInstruction.of(TypeKind.REFERENCE, 1), 0, stack);

    assertEquals(1, stack.size());
    assertEquals(CalcStack24.NULL_CD, stack.peek());
    assertFalse(stack.peek().isPrimitive());
  }

  /** An ALOAD of a local variable whose type is known pushes that type. */
  @Test
  public void testAloadOfLocalWithKnownType() {
    initializeSimulationState(1, 2);
    DCInstrument24.locals[1] = CD_String;
    OperandStack24 stack = new OperandStack24(1);

    CalcStack24.simulateInstruction(LoadInstruction.of(TypeKind.REFERENCE, 1), 0, stack);

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

    CalcStack24.simulateInstruction(StoreInstruction.of(TypeKind.REFERENCE, 1), 0, stack);
    assertEquals(0, stack.size());

    CalcStack24.simulateInstruction(LoadInstruction.of(TypeKind.REFERENCE, 1), 1, stack);
    assertEquals(1, stack.size());
    assertEquals(CD_String, stack.peek());
  }
}
