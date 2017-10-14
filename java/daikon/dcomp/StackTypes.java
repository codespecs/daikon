package daikon.dcomp;

import org.apache.bcel.generic.InstructionList;
import org.apache.bcel.generic.MethodGen;
import org.apache.bcel.generic.Type;
import org.apache.bcel.verifier.structurals.Frame;
import org.apache.bcel.verifier.structurals.LocalVariables;
import org.apache.bcel.verifier.structurals.OperandStack;
import org.apache.bcel.verifier.structurals.UninitializedObjectType;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Stores the types on the stack at each instruction (identified by byte code offset) in a method.
 */
public final class StackTypes {

  boolean track_locals = true;
  OperandStack[] os_arr;
  // non-null if track_locals is true
  LocalVariables /*@Nullable*/ [] loc_arr;

  public StackTypes(MethodGen mg) {
    InstructionList il = mg.getInstructionList();
    int size = 0;
    if (il != null) size = il.getEnd().getPosition();
    os_arr = new OperandStack[size + 1];
    if (track_locals) loc_arr = new LocalVariables[size + 1];
  }

  /** Sets the stack for the instruction at the specified offset */
  public void set(int offset, Frame f) {

    OperandStack os = f.getStack();
    // logger.info ("stack[" + offset + "] = " + toString(os));

    if (track_locals) {
      assert loc_arr != null
          : "@AssumeAssertion(nullness): dependent: non-null if track_locals==true";
      loc_arr[offset] = (LocalVariables) (f.getLocals().clone());
    }

    os_arr[offset] = (OperandStack) (os.clone());
  }

  /** Returns the stack contents at the specified offset */
  public OperandStack get(int offset) {
    return os_arr[offset];
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied StackTypes this*/) {

    StringBuffer sb = new StringBuffer();

    for (int i = 0; i < os_arr.length; i++) {
      if (os_arr[i] != null) {
        sb.append(String.format("Instruction %d:\n", i));
        sb.append(String.format("  stack:  %s\n", toString(os_arr[i])));
        if (track_locals) {
          assert loc_arr != null
              : "@AssumeAssertion(nullness): dependent: non-null if track_locals==true";
          sb.append(String.format("  locals: %s\n", toString(loc_arr[i])));
        }
      }
    }

    return (sb.toString());
  }

  /*@SideEffectFree*/
  public String toString(/*>>>@GuardSatisfied StackTypes this,*/ OperandStack os) {

    String buff = "";

    for (int i = 0; i < os.size(); i++) {
      if (buff.length() > 0) buff += ", ";
      Type t = os.peek(i);
      if (t instanceof UninitializedObjectType) {
        buff += "uninitialized-object";
      } else {
        buff += t;
      }
    }

    return ("{" + buff + "}");
  }

  /*@SideEffectFree*/
  public String toString(/*>>>@GuardSatisfied StackTypes this,*/ LocalVariables lv) {

    String buff = "";

    for (int i = 0; i < lv.maxLocals(); i++) {
      if (buff.length() > 0) buff += ", ";
      Type t = lv.get(i);
      if (t instanceof UninitializedObjectType) {
        buff += "uninitialized-object";
      } else {
        buff += t;
      }
    }
    return ("{" + buff + "}");
  }
}
