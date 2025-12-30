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
 * previously verified, so we check very few error conditions.
 *
 * <p>This class implements a stack used for symbolic JVM stack simulation. [It's used as an operand
 * stack substitute.] Elements of this stack are {@link ClassDesc} objects.
 */
public class OperandStack24 implements Cloneable {

  /** The underlying stack delegate. */
  private ArrayList<ClassDesc> stack = new ArrayList<>();

  /** The maximum number of stack slots this OperandStack instance may hold. */
  private final @NonNegative int maxStack;

  /**
   * Creates an empty stack with a maximum of maxStack items. Note that this might be larger than
   * necessary as a method's maxStack is the maximum number of stack slots used. This could be
   * larger than the number of stack operands if any are of type {@code long} or {@code double}.
   */
  public OperandStack24(final @NonNegative int maxStack) {
    this.maxStack = maxStack;
  }

  /** Clears the stack. */
  public void clear() {
    stack.clear();
  }

  /**
   * Returns a copy of this object. The clone contains a new stack. However, the ClassDesc objects
   * on the stack are shared.
   */
  @Override
  public OperandStack24 clone(@GuardSatisfied OperandStack24 this) {
    final OperandStack24 newstack;
    try {
      newstack = (OperandStack24) super.clone();
    } catch (CloneNotSupportedException e) {
      throw new DynCompError("Error: ", e);
    }
    newstack.stack = new ArrayList<>(this.stack);
    return newstack;
  }

  /**
   * Returns true if and only if this OperandStack equals another, meaning equal lengths and equal
   * objects on the stacks. This method is used to verify that the operand stacks match when two
   * execution paths meet. A special case is {@code null} on an operand stack, which matches
   * anything but a primitive. As we are assuming that the class file is valid, then it is okay for
   * an object to exist on one execution path and be null on the other.
   */
  @Override
  public boolean equals(@GuardSatisfied OperandStack24 this, @GuardSatisfied @Nullable Object o) {
    if (this == o) {
      return true;
    }
    if (!(o instanceof OperandStack24)) {
      return false;
    }
    final OperandStack24 other = (OperandStack24) o;
    if (this.stack.size() != other.stack.size()) {
      return false;
    }
    for (int i = 0; i < this.stack.size(); i++) {
      ClassDesc thisItem = this.stack.get(i);
      ClassDesc otherItem = other.stack.get(i);
      if (!compareOperandStackElements(thisItem, otherItem)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Returns true if and only if the two OperandStack elements are equal. An element may be one of
   * four possible items:
   *
   * <ul>
   *   <li>{@code null} a special case as it matches anything but a primitive
   *   <li>a primitive (such as {@code int}, {@code long}, etc.)
   *   <li>an array reference
   *   <li>an object reference
   * </ul>
   *
   * @param thisItem one OperandStack element
   * @param otherItem another OperandStack element
   * @return true if and only if the items match
   */
  protected boolean compareOperandStackElements(
      @GuardSatisfied OperandStack24 this,
      @Nullable ClassDesc thisItem,
      @Nullable ClassDesc otherItem) {
    if (thisItem == null) {
      if (otherItem != null && otherItem.isPrimitive()) {
        return false;
      }
      // We assume null matches any array or any class.
      return true;
    } else if (otherItem == null) {
      // we know thisItem != null
      if (thisItem.isPrimitive()) {
        return false;
      }
      // We assume null matches any array or any class.
      return true;
    } else if (thisItem.isArray()) {
      if (otherItem.isPrimitive()) {
        return false;
      }
      // We assume an array matches any array or any class as they all have Object as a superclass
      // at some point.
      return true;
    } else if (thisItem.isClassOrInterface()) {
      if (otherItem.isPrimitive()) {
        return false;
      }
      // We assume a class matches any array or any class as they all have Object as a superclass
      // at some point.
      return true;
    } else if (thisItem.isPrimitive() && !otherItem.isPrimitive()) {
      return false;
    }
    // Both operands are primitives - they better match.
    if (thisItem.equals(otherItem)) {
      return true;
    } else {
      throw new DynCompError(
          "Operand stack primitives don't match: " + thisItem + ", " + otherItem);
    }
  }

  /**
   * Returns a (typed!) clone of this.
   *
   * @see #clone()
   */
  public OperandStack24 getClone() {
    return this.clone();
  }

  /**
   * @return a hash code value for the object
   */
  @Override
  public int hashCode(@GuardSatisfied OperandStack24 this) {
    int result = 1;
    for (ClassDesc item : stack) {
      int elementHash;
      if (item != null && item.isPrimitive()) {
        // Primitives must match exactly in equals, so use their own hash code.
        elementHash = item.hashCode();
      } else {
        // `null`, arrays, and classes are all treated as equivalent in equals.
        elementHash = 2;
      }
      result = 31 * result + elementHash;
    }
    return result;
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
   * Pops {@code count} elements off the stack. Always returns {@code null}.
   *
   * @return {@code null}
   */
  public @Nullable ClassDesc pop(final int count) {
    for (int j = 0; j < count; j++) {
      pop();
    }
    return null;
  }

  /** Pushes a ClassDesc object onto the stack. */
  public void push(final ClassDesc type) {
    if (slotsUsed() + slotSize(type) > maxStack) {
      throw new DynCompError("Operand stack size exceeded: " + stack);
    }
    stack.add(type);
  }

  /**
   * Returns the size of this OperandStack; that means, how many ClassDesc objects it currently
   * contains.
   */
  @Pure
  public @NonNegative int size(@GuardSatisfied OperandStack24 this) {
    return stack.size();
  }

  /**
   * Returns the total number of stack slots used. This could be larger than the total number of
   * stack operands if any are of type {@code long} or {@code double}.
   *
   * @see #maxStack()
   */
  public @NonNegative int slotsUsed(@GuardSatisfied OperandStack24 this) {
    int slots = 0;
    for (ClassDesc item : stack) {
      slots += slotSize(item);
    }
    return slots;
  }

  /** Returns a String representation of this OperandStack instance. */
  @Override
  public String toString(@GuardSatisfied OperandStack24 this) {
    final StringBuilder sb = new StringBuilder(60);
    sb.append("Slots used: ")
        .append(slotsUsed())
        .append(" MaxStack: ")
        .append(maxStack)
        .append('\n');
    for (int i = 0; i < size(); i++) {
      sb.append(peek(i)).append(" (Size: ").append(String.valueOf(slotSize(peek(i)))).append(")\n");
    }
    return sb.toString();
  }

  /**
   * Calculate the size of an item on the operand stack. This is 2 for {@code long} or {@code
   * double}, 1 for everything else.
   *
   * @param item type to get size of
   * @return size of item
   */
  int slotSize(@GuardSatisfied OperandStack24 this, ClassDesc item) {
    if (item == null) {
      return 1;
    } else {
      return TypeKind.from(item).slotSize();
    }
  }
}
