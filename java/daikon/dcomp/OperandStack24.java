// This is a modified version of BCEL's OperandStack.  See below for details.

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package daikon.dcomp;

import java.lang.classfile.TypeKind;
import java.lang.constant.ClassDesc;
import java.util.ArrayList;
import org.checkerframework.checker.index.qual.NonNegative;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;

/**
 * This is a modified version of BCEL's OperandStack. We assume that the class file has been
 * previously verified, so we check very few error contitions.
 *
 * <p>This class implements a stack used for symbolic JVM stack simulation. [It's used as an operand
 * stack substitute.] Elements of this stack are {@link ClassDesc} objects.
 */
public class OperandStack24 implements Cloneable {

  /** We hold the stack information here. */
  private ArrayList<ClassDesc> stack = new ArrayList<>();

  /** The maximum number of stack slots this OperandStack instance may hold. */
  private final @NonNegative int maxStack;

  /** Creates an empty stack with a maximum of maxStack slots. */
  public OperandStack24(final @NonNegative int maxStack) {
    this.maxStack = maxStack;
  }

  /** Clears the stack. */
  public void clear() {
    stack = new ArrayList<>();
  }

  /**
   * Returns a deep copy of this object; that means, the clone operates on a new stack. However, the
   * ClassDesc objects on the stack are shared.
   */
  @Override
  public Object clone(@GuardSatisfied OperandStack24 this) {
    final OperandStack24 newstack = new OperandStack24(this.maxStack);
    @SuppressWarnings("unchecked") // OK because this.stack is the same type
    final ArrayList<ClassDesc> clone = (ArrayList<ClassDesc>) this.stack.clone();
    newstack.stack = clone;
    return newstack;
  }

  /**
   * Returns true if and only if this OperandStack equals another, meaning equal lengths and equal
   * objects on the stacks.
   */
  @Override
  public boolean equals(@GuardSatisfied OperandStack24 this, @GuardSatisfied @Nullable Object o) {
    if (!(o instanceof OperandStack24)) {
      return false;
    }
    final OperandStack24 s = (OperandStack24) o;
    return this.stack.equals(s.stack);
  }

  /**
   * Returns a (typed!) clone of this.
   *
   * @see #clone()
   */
  public OperandStack24 getClone() {
    return (OperandStack24) this.clone();
  }

  /**
   * @return a hash code value for the object.
   */
  @Override
  public int hashCode(@GuardSatisfied OperandStack24 this) {
    return stack.hashCode();
  }

  /** Returns true IFF this OperandStack is empty. */
  public boolean isEmpty() {
    return stack.isEmpty();
  }

  /** Returns the number of stack slots this stack can hold. */
  @Pure
  public @NonNegative int maxStack() {
    return this.maxStack;
  }

  /**
   * Merges another stack state into this instance's stack state. See the Java Virtual Machine
   * Specification, Second Edition, page 146: 4.9.2 for details.
   */
  public void merge(final OperandStack24 s) {
    if (slotsUsed() != s.slotsUsed() || size() != s.size()) {
      throw new Error(
          "Cannot merge stacks of different size:\nOperandStack A:\n"
              + this
              + "\nOperandStack B:\n"
              + s);
    }

    for (int i = 0; i < size(); i++) {
      if (!stack.get(i).equals(s.stack.get(i))) {
        throw new Error(
            "Cannot merge stack types don't match:\nOperandStack A:\n"
                + this
                + "\nOperandStack B:\n"
                + s);
      }
    }
  }

  /** Returns the element on top of the stack. The element is not popped off the stack! */
  @Pure
  public ClassDesc peek() {
    return peek(0);
  }

  /**
   * Returns the element that's i elements below the top element; that means, iff i==0 the top
   * element is returned. The element is not popped off the stack!
   */
  @Pure
  public ClassDesc peek(@GuardSatisfied OperandStack24 this, final int i) {
    return stack.get(size() - i - 1);
  }

  /** Returns the element on top of the stack. The element is popped off the stack. */
  public ClassDesc pop() {
    return stack.remove(size() - 1);
  }

  /**
   * Pops i elements off the stack. Always returns null.
   *
   * @return Always returns null.
   */
  public @Nullable ClassDesc pop(final int count) {
    for (int j = 0; j < count; j++) {
      pop();
    }
    return null;
  }

  /** Pushes a ClassDesc object onto the stack. */
  public void push(final ClassDesc type) {
    stack.add(type);
  }

  /** Returns the size of this OperandStack; that means, how many ClassDesc objects there are. */
  @Pure
  public @NonNegative int size(@GuardSatisfied OperandStack24 this) {
    return stack.size();
  }

  /**
   * Returns the number of stack slots used.
   *
   * @see #maxStack()
   */
  public @NonNegative int slotsUsed(@GuardSatisfied OperandStack24 this) {
    /*
     * XXX change this to a better implementation using a variable that keeps track of the actual slotsUsed()-value
     * monitoring all push()es and pop()s.
     */
    int slots = 0;
    for (int i = 0; i < stack.size(); i++) {
      slots += TypeKind.from(peek(i)).slotSize();
    }
    return slots;
  }

  /** Returns a String representation of this OperandStack instance. */
  @Override
  public String toString(@GuardSatisfied OperandStack24 this) {
    final StringBuilder sb = new StringBuilder();
    sb.append("Slots used: ");
    sb.append(slotsUsed());
    sb.append(" MaxStack: ");
    sb.append(maxStack);
    sb.append(".\n");
    for (int i = 0; i < size(); i++) {
      sb.append(peek(i));
      sb.append(" (Size: ");
      sb.append(String.valueOf(TypeKind.from(peek(i)).slotSize()));
      sb.append(")\n");
    }
    return sb.toString();
  }
}
