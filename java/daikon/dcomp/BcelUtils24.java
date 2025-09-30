package daikon.dcomp;

import daikon.chicory.MethodGen24;
import daikon.plumelib.bcelutil.SimpleLog;
import java.lang.classfile.CodeElement;
import java.lang.classfile.TypeKind;
import java.lang.classfile.instruction.DiscontinuedInstruction;
import java.lang.classfile.instruction.IncrementInstruction;
import java.lang.classfile.instruction.LoadInstruction;
import java.lang.classfile.instruction.LocalVariable;
import java.lang.classfile.instruction.StoreInstruction;
import java.lang.constant.ClassDesc;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;
import org.checkerframework.checker.signature.qual.Identifier;

/**
 * This class provides utility methods for manipulating bytecode structures in the Java 24
 * instrumentation pipeline, including operations on local variables, parameter types, and
 * instruction adjustments.
 */
public final class BcelUtils24 {

  /** Don't allow others to create a new BcelUtils24 object. */
  private BcelUtils24() {}

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

  /** A log to which to print debugging information about program instrumentation. */
  private static SimpleLog debugInstrument = new SimpleLog(false);

  /**
   * Returns a copy of the given type array, with newType added to the end.
   *
   * @param types the array to extend
   * @param newType the element to add to the end of the array
   * @return a new array, with newType at the end
   */
  public static ClassDesc[] postpendToArray(ClassDesc[] types, ClassDesc newType) {
    if (types.length == Integer.MAX_VALUE) {
      throw new IllegalArgumentException(
          "array " + Arrays.toString(types) + " is too large to extend");
    }
    ClassDesc[] newTypes = new ClassDesc[types.length + 1];
    System.arraycopy(types, 0, newTypes, 0, types.length);
    newTypes[types.length] = newType;
    return newTypes;
  }

  /**
   * Returns a String array with newString added to the end of arr.
   *
   * @param arr original string array
   * @param newString string to be added
   * @return the new string array
   */
  static String[] addString(String[] arr, String newString) {
    String[] newArr = new String[arr.length + 1];
    System.arraycopy(arr, 0, newArr, 0, arr.length);
    newArr[arr.length] = newString;
    return newArr;
  }

  /**
   * Process the instruction list, adding size (1 or 2) to the slot number of each Instruction that
   * references a local that is equal or higher in the local map than offsetFirstMovedlocal. Size
   * should be the size of the new local that was just inserted at offsetFirstMovedlocal.
   *
   * @param mgen MethodGen to be modified
   * @param offsetFirstMovedLocal original offset of first local moved "up"
   * @param size size of new local added (1 or 2)
   */
  static void adjust_code_for_locals_change(MethodGen24 mgen, int offsetFirstMovedLocal, int size) {

    debugInstrument.log("adjust code: %d %d%n", offsetFirstMovedLocal, size);
    try {
      List<CodeElement> il = mgen.getInstructionList();
      ListIterator<CodeElement> iter = il.listIterator();
      while (iter.hasNext()) {

        CodeElement inst = iter.next();
        switch (inst) {
          case DiscontinuedInstruction.RetInstruction ret -> {
            if (ret.slot() >= offsetFirstMovedLocal) {
              iter.set(DiscontinuedInstruction.RetInstruction.of(ret.slot() + size));
            }
          }
          case IncrementInstruction inc -> {
            if (inc.slot() >= offsetFirstMovedLocal) {
              iter.set(IncrementInstruction.of(inc.slot() + size, inc.constant()));
            }
          }
          case LoadInstruction load -> {
            if (load.slot() >= offsetFirstMovedLocal) {
              iter.set(LoadInstruction.of(load.typeKind(), load.slot() + size));
            }
          }
          case StoreInstruction store -> {
            if (store.slot() >= offsetFirstMovedLocal) {
              iter.set(StoreInstruction.of(store.typeKind(), store.slot() + size));
            }
          }
          default -> {} // ignore all other instructions
        }
      }
    } catch (Exception e) {
      System.err.printf("Unexpected exception encountered: %s", e);
      e.printStackTrace();
    }
  }

  /**
   * Add a new local variable to the method. This will be inserted after last current parameter and
   * before the first local variable. This might have the side effect of causing us to rewrite the
   * method byte codes to adjust the slot numbers for the existing local variables - see below for
   * details.
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
   * @param argName name of new parameter
   * @param argType type of new parameter
   * @param minfo for the given method's code
   * @param isParam if true, the new local is a new parameter
   * @return a LocalVariable for the new variable
   */
  public static LocalVariable addNewSpecialLocal(
      MethodGen24 mgen,
      String argName,
      ClassDesc argType,
      MethodGen24.MInfo24 minfo,
      boolean isParam) {

    debugInstrument.enabled = daikon.dcomp.DCInstrument24.bcelDebug;

    // We add a new local variable, after any parameters and before any
    // existing local variables.  We then need to make a pass over the
    // byte codes to update the slot number of any locals we just shifted up.

    LocalVariable argNew;
    // get a copy of the locals before modification
    List<LocalVariable> locals = mgen.localsTable;
    ClassDesc[] argTypes = mgen.getParameterTypes();
    int newIndex = 0; // index into 'locals'
    int newOffset = 0; // current local slot number

    boolean hasCode = !mgen.getInstructionList().isEmpty();
    int argSize = TypeKind.from(argType).slotSize();

    if (!mgen.isStatic()) {
      // Skip the 'this' pointer.
      newIndex++;
      newOffset++; // size of 'this' is 1
    }

    if (argTypes.length > 0) {
      LocalVariable lastArg;
      newIndex = newIndex + argTypes.length;
      // newIndex is now positive, because argTypes.length is
      lastArg = locals.get(newIndex - 1);
      newOffset = lastArg.slot() + TypeKind.from(lastArg.typeSymbol()).slotSize();
    }

    // Insert our new local variable into existing table at 'newOffset'.
    argNew = LocalVariable.of(newOffset, argName, argType, minfo.startLabel, minfo.endLabel);
    mgen.localsTable.add(newIndex, argNew);
    minfo.nextLocalIndex += argSize;
    mgen.setMaxLocals(minfo.nextLocalIndex);

    if (isParam) {
      // Update the method's parameter information.
      argTypes = postpendToArray(argTypes, argType);
      @SuppressWarnings("signature:assignment") // string manipulation
      @Identifier String[] argNames = addString(mgen.getParameterNames(), argName);
      mgen.setParameterTypes(argTypes);
      mgen.setParameterNames(argNames);
    }

    String varType = isParam ? "arg" : "local";
    debugInstrument.log(
        "Added %s at %s%n",
        varType, argNew.slot() + ": " + argNew.name() + ", " + argNew.type() + ", " + argSize);

    if (hasCode) {
      // we need to adjust the offset of any locals after our insertion
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
      adjust_code_for_locals_change(mgen, newOffset, argSize);

      // debugInstrument.log("New LocalVariableTable:%n%s%n", mgen.localsTable);
    }
    return argNew;
  }
}
