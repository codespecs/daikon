package daikon.dcomp;

import daikon.chicory.MethodGen24;
import daikon.plumelib.util.ArraysPlume;
import java.lang.classfile.CodeElement;
import java.lang.classfile.TypeKind;
import java.lang.classfile.instruction.DiscontinuedInstruction;
import java.lang.classfile.instruction.IncrementInstruction;
import java.lang.classfile.instruction.LoadInstruction;
import java.lang.classfile.instruction.LocalVariable;
import java.lang.classfile.instruction.StoreInstruction;
import java.lang.constant.ClassDesc;
import java.util.List;
import java.util.ListIterator;
import org.checkerframework.checker.signature.qual.Identifier;

/**
 * This class provides static methods for manipulating bytecode structures, including operations on
 * local variables, parameter types, and instruction adjustments. It is loosely based on
 * StackMapUtils.java located in the plume-lib/bcel-util repository. It implements a very small
 * subset of the methods in StackMapUtils and does no manipulation of StackMaps at all. Its primary
 * method is {@link #addNewSpecialLocal} which is a replacement for the two methods addNewParameter
 * and create_method_scope_local in the original StackMapUtils.
 *
 * <p>StackMapUtils24 uses Java's {@code java.lang.classfile} APIs for reading and modifying .class
 * files. Those APIs were added in JDK 24. Compared to BCEL, these APIs are more complete and robust
 * and are always up to date with any .class file changes (since they are part of the JDK).
 */
public final class StackMapUtils24 {

  /** Do not instantiate. */
  private StackMapUtils24() {
    throw new Error("Do not instantiate");
  }

  /*
   * NOMENCLATURE
   *
   * 'index' is an item's subscript into a data structure.
   *
   * 'offset' is used to describe two different address types:
   *   * the offset of a byte code from the start of a method's byte codes
   *   * the offset of a variable from the start of a method's stack frame
   *
   *     The Java Virtual Machine Specification uses
   *     'index into the local variable array of the current frame'
   *     or 'slot number' to describe this second case.
   */

  /**
   * Add {@code size} (1 or 2) to the slot number of each Instruction that references a local that
   * is equal or higher in the local map than {@code offsetFirstLocalToBeMoved}. Size should be the
   * size of the new local that was just inserted at {@code offsetFirstLocalToBeMoved}.
   *
   * @param mgen MethodGen to be modified
   * @param offsetFirstLocalToBeMoved original offset of first local moved "up"
   * @param size size of new local added (1 or 2)
   */
  static void adjustCodeForLocalsChange(MethodGen24 mgen, int offsetFirstLocalToBeMoved, int size) {

    DCInstrument24.debugInstrument.log(
        "adjustCodeForLocalsChange: %d %d%n", offsetFirstLocalToBeMoved, size);
    try {
      List<CodeElement> il = mgen.getInstructionList();
      ListIterator<CodeElement> iter = il.listIterator();
      while (iter.hasNext()) {

        CodeElement inst = iter.next();
        switch (inst) {
          case DiscontinuedInstruction.RetInstruction ret -> {
            if (ret.slot() >= offsetFirstLocalToBeMoved) {
              iter.set(DiscontinuedInstruction.RetInstruction.of(ret.slot() + size));
            }
          }
          case IncrementInstruction inc -> {
            if (inc.slot() >= offsetFirstLocalToBeMoved) {
              iter.set(IncrementInstruction.of(inc.slot() + size, inc.constant()));
            }
          }
          case LoadInstruction load -> {
            if (load.slot() >= offsetFirstLocalToBeMoved) {
              iter.set(LoadInstruction.of(load.typeKind(), load.slot() + size));
            }
          }
          case StoreInstruction store -> {
            if (store.slot() >= offsetFirstLocalToBeMoved) {
              iter.set(StoreInstruction.of(store.typeKind(), store.slot() + size));
            }
          }
          default -> {} // No other instructions reference local variables.
        }
      }
    } catch (Throwable t) {
      if (DCInstrument24.debugInstrument.enabled) {
        t.printStackTrace();
      }
      throw new DynCompError("Unexpected exception encountered in adjustCodeForLocalsChange", t);
    }
  }

  /**
   * Add a new local variable to the method. This will be inserted after any parameters and before
   * any existing local variables. If there are existing local variables this will have the side
   * effect of rewritting the method byte codes to adjust the slot numbers for the existing local
   * variables - see below for details.
   *
   * <p>DCInstrument24 uses this routine for two special variables:
   *
   * <ol>
   *   <li>the dcomp marker - added as a parameter
   *   <li>the tag frame array - added as a local
   * </ol>
   *
   * <p>Must call {@link MethodGen24#fixLocals} before calling this routine.
   *
   * @param mgen MethodGen to be modified
   * @param minfo for the given method's code
   * @param varName name of new parameter
   * @param varType type of new parameter
   * @param isParam if true, the new local is a new parameter (the DCompMarker)
   * @return a LocalVariable for the new variable
   */
  public static LocalVariable addNewSpecialLocal(
      MethodGen24 mgen,
      MethodGen24.MInfo24 minfo,
      @Identifier String varName,
      ClassDesc varType,
      boolean isParam) {

    LocalVariable varNew;
    // get a copy of the locals before modification
    List<LocalVariable> locals = mgen.localsTable;
    ClassDesc[] paramTypes = mgen.getParameterTypes();
    int newIndex = 0; // index into `locals`
    int newOffset = 0; // current local slot number

    int argSize = TypeKind.from(varType).slotSize();

    if (!mgen.isStatic()) {
      // Skip the 'this' pointer.
      newIndex++;
      newOffset++; // size of 'this' is 1
    }

    if (paramTypes.length > 0) {
      LocalVariable lastArg;
      newIndex = newIndex + paramTypes.length;
      // newIndex is now positive, because paramTypes.length is
      lastArg = locals.get(newIndex - 1);
      newOffset = lastArg.slot() + TypeKind.from(lastArg.typeSymbol()).slotSize();
    }

    // Insert our new local variable into existing table at `newOffset`.
    varNew = LocalVariable.of(newOffset, varName, varType, minfo.startLabel, minfo.endLabel);
    mgen.localsTable.add(newIndex, varNew);
    minfo.nextLocalIndex += argSize;
    mgen.setMaxLocals(minfo.nextLocalIndex);

    if (isParam) {
      // Update the method's parameter information.
      paramTypes = ArraysPlume.append(paramTypes, varType);
      @Identifier String[] paramNames =
          ArraysPlume.<@Identifier String>append(mgen.getParameterNames(), varName);
      mgen.setParameterTypes(paramTypes);
      mgen.setParameterNames(paramNames);
    }

    DCInstrument24.debugInstrument.log(
        "Added a %s at slot %s.%n  name: %s type: %s size: %s%n",
        isParam ? "parameter" : "local", varNew.slot(), varNew.name(), varNew.type(), argSize);

    boolean hasCode = !mgen.getInstructionList().isEmpty();
    if (hasCode) {
      // Adjust the offset of any locals after our insertion.
      for (int i = newIndex + 1; i < locals.size(); i++) {
        LocalVariable lv = locals.get(i);
        locals.set(
            i,
            LocalVariable.of(
                lv.slot() + argSize, lv.name(), lv.type(), lv.startScope(), lv.endScope()));
      }

      // Now process the instruction list, adding one to the offset
      // within each LocalVariableInstruction that references a
      // local that is 'higher' in the local map than new local
      // we just inserted.
      adjustCodeForLocalsChange(mgen, newOffset, argSize);

      // DCInstrument24.debugInstrument.log("New LocalVariableTable:%n%s%n", mgen.localsTable);
    }
    return varNew;
  }
}
