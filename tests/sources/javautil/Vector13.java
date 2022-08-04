/*
 * @(#)Vector.java	1.39 98/07/01
 *
 * Copyright 1995-1998 by Sun Microsystems, Inc.,
 * 901 San Antonio Road, Palo Alto, California, 94303, U.S.A.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information
 * of Sun Microsystems, Inc. ("Confidential Information").  You
 * shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement
 * you entered into with Sun.
 */

package javautil;

import java.util.Enumeration;
import java.util.NoSuchElementException;


/**
 * The <code>Vector</code> class implements a growable array of
 * objects. Like an array, it contains components that can be
 * accessed using an integer index. However, the size of a
 * <code>Vector</code> can grow or shrink as needed to accommodate
 * adding and removing items after the <code>Vector</code> has been created.
 * <p>
 * Each vector tries to optimize storage management by maintaining a
 * <code>capacity</code> and a <code>capacityIncrement</code>. The
 * <code>capacity</code> is always at least as large as the vector
 * size; it is usually larger because as components are added to the
 * vector, the vector's storage increases in chunks the size of
 * <code>capacityIncrement</code>. An application can increase the
 * capacity of a vector before inserting a large number of
 * components; this reduces the amount of incremental reallocation.
 *
 * @author  Lee Boynton
 * @author  Jonathan Payne
 * @version 1.39, 07/01/98
 * @since   JDK1.0
 */
public
class Vector13 implements Cloneable, java.io.Serializable {
    /**
     * The array buffer into which the components of the vector are
     * stored. The capacity of the vector is the length of this array buffer.
     *
     * @since   JDK1.0
     */
    protected Object elementData[];

    /**
     * The number of valid components in the vector.
     *
     * @since   JDK1.0
     */
    protected int elementCount;

    /**
     * The amount by which the capacity of the vector is automatically
     * incremented when its size becomes greater than its capacity. If
     * the capacity increment is <code>0</code>, the capacity of the
     * vector is doubled each time it needs to grow.
     *
     * @since   JDK1.0
     */
    protected int capacityIncrement;

    /** use serialVersionUID from JDK 1.0.2 for interoperability */
    private static final long serialVersionUID = -2767605614048989439L;

    /**
     * Constructs an empty vector with the specified initial capacity and
     * capacity increment.
     *
     * @param   initialCapacity     the initial capacity of the vector.
     * @param   capacityIncrement   the amount by which the capacity is
     *                              increased when the vector overflows.
     * @since   JDK1.0
     */
    public Vector13(int initialCapacity, int capacityIncrement) {
	super();
	this.elementData = new Object[initialCapacity];
	this.capacityIncrement = capacityIncrement;
    }

    /**
     * Constructs an empty vector with the specified initial capacity.
     *
     * @param   initialCapacity   the initial capacity of the vector.
     * @since   JDK1.0
     */
    public Vector13(int initialCapacity) {
	this(initialCapacity, 0);
    }

    /**
     * Constructs an empty vector.
     *
     * @since   JDK1.0
     */
    public Vector13() {
	this(10);
    }

    /**
     * Copies the components of this vector into the specified array.
     * The array must be big enough to hold all the objects in this  vector.
     *
     * @param   anArray   the array into which the components get copied.
     * @since   JDK1.0
     */
    public final synchronized void copyInto(Object[] anArray) {
	int i = elementCount;
	while (i-- > 0) {
	    anArray[i] = elementData[i];
	}
    }

    /**
     * Trims the capacity of this vector to be the vector's current
     * size. An application can use this operation to minimize the
     * storage of a vector.
     *
     * @since   JDK1.0
     */
    public final synchronized void trimToSize() {
	int oldCapacity = elementData.length;
	if (elementCount < oldCapacity) {
	    Object oldData[] = elementData;
	    elementData = new Object[elementCount];
	    System.arraycopy(oldData, 0, elementData, 0, elementCount);
	}
    }

    /**
     * Increases the capacity of this vector, if necessary, to ensure
     * that it can hold at least the number of components specified by
     * the minimum capacity argument.
     *
     * @param   minCapacity   the desired minimum capacity.
     * @since   JDK1.0
     */
    public final synchronized void ensureCapacity(int minCapacity) {
	if (minCapacity > elementData.length) {
	    ensureCapacityHelper(minCapacity);
	}
    }

    /**
     * This implements the unsynchronized semantics of ensureCapacity.
     * Synchronized methods in this class can internally call this
     * method for ensuring capacity without incurring the cost of an
     * extra synchronization.
     *
     * @see java.util.Vector#ensureCapacity(int)
     */
    private void ensureCapacityHelper(int minCapacity) {
	int oldCapacity = elementData.length;
	Object oldData[] = elementData;
	int newCapacity = (capacityIncrement > 0) ?
	    (oldCapacity + capacityIncrement) : (oldCapacity * 2);
	if (newCapacity < minCapacity) {
	    newCapacity = minCapacity;
	}
	elementData = new Object[newCapacity];
	System.arraycopy(oldData, 0, elementData, 0, elementCount);
    }

    /**
     * Sets the size of this vector. If the new size is greater than the
     * current size, new <code>null</code> items are added to the end of
     * the vector. If the new size is less than the current size, all
     * components at index <code>newSize</code> and greater are discarded.
     *
     * @param   newSize   the new size of this vector.
     * @since   JDK1.0
     */
    public final synchronized void setSize(int newSize) {
	if ((newSize > elementCount) && (newSize > elementData.length)) {
	    ensureCapacityHelper(newSize);
	} else {
	    for (int i = newSize ; i < elementCount ; i++) {
		elementData[i] = null;
	    }
	}
	elementCount = newSize;
    }

    /**
     * Returns the current capacity of this vector.
     *
     * @return  the current capacity of this vector.
     * @since   JDK1.0
     */
    public final int capacity() {
	return elementData.length;
    }

    /**
     * Returns the number of components in this vector.
     *
     * @return  the number of components in this vector.
     * @since   JDK1.0
     */
    public final int size() {
	return elementCount;
    }

    /**
     * Tests if this vector has no components.
     *
     * @return  <code>true</code> if this vector has no components;
     *          <code>false</code> otherwise.
     * @since   JDK1.0
     */
    public final boolean isEmpty() {
	return elementCount == 0;
    }

    /**
     * Returns an enumeration of the components of this vector.
     *
     * @return  an enumeration of the components of this vector.
     * @see     java.util.Enumeration
     * @since   JDK1.0
     */
    public final synchronized Enumeration elements() {
	return new VectorEnumerator13(this);
    }

    /**
     * Tests if the specified object is a component in this vector.
     *
     * @param   elem   an object.
     * @return  <code>true</code> if the specified object is a component in
     *          this vector; <code>false</code> otherwise.
     * @since   JDK1.0
     */
    public final boolean contains(Object elem) {
	return indexOf(elem, 0) >= 0;
    }

    /**
     * Searches for the first occurence of the given argument, testing
     * for equality using the <code>equals</code> method.
     *
     * @param   elem   an object.
     * @return  the index of the first occurrence of the argument in this
     *          vector; returns <code>-1</code> if the object is not found.
     * @see     java.lang.Object#equals(java.lang.Object)
     * @since   JDK1.0
     */
    public final int indexOf(Object elem) {
	return indexOf(elem, 0);
    }

    /**
     * Searches for the first occurence of the given argument, beginning
     * the search at <code>index</code>, and testing for equality using
     * the <code>equals</code> method.
     *
     * @param   elem    an object.
     * @param   index   the index to start searching from.
     * @return  the index of the first occurrence of the object argument in
     *          this vector at position <code>index</code> or later in the
     *          vector; returns <code>-1</code> if the object is not found.
     * @see     java.lang.Object#equals(java.lang.Object)
     * @since   JDK1.0
     */
    public final synchronized int indexOf(Object elem, int index) {
	for (int i = index ; i < elementCount ; i++) {
	    if (elem.equals(elementData[i])) {
		return i;
	    }
	}
	return -1;
    }

    /**
     * Returns the index of the last occurrence of the specified object in
     * this vector.
     *
     * @param   elem   the desired component.
     * @return  the index of the last occurrence of the specified object in
     *          this vector; returns <code>-1</code> if the object is not found.
     * @since   JDK1.0
     */
    public final int lastIndexOf(Object elem) {
	return lastIndexOf(elem, elementCount-1);
    }

    /**
     * Searches backwards for the specified object, starting from the
     * specified index, and returns an index to it.
     *
     * @param   elem    the desired component.
     * @param   index   the index to start searching from.
     * @return  the index of the last occurrence of the specified object in this
     *          vector at position less than <code>index</code> in the vector;
     *          <code>-1</code> if the object is not found.
     * @since   JDK1.0
     */
    public final synchronized int lastIndexOf(Object elem, int index) {
	for (int i = index ; i >= 0 ; i--) {
	    if (elem.equals(elementData[i])) {
		return i;
	    }
	}
	return -1;
    }

    /**
     * Returns the component at the specified index.
     *
     * @param      index   an index into this vector.
     * @return     the component at the specified index.
     * @exception  ArrayIndexOutOfBoundsException  if an invalid index was
     *               given.
     * @since      JDK1.0
     */
    public final synchronized Object elementAt(int index) {
	if (index >= elementCount) {
	    throw new ArrayIndexOutOfBoundsException(index + " >= " + elementCount);
	}
	/* Since try/catch is free, except when the exception is thrown,
	   put in this extra try/catch to catch negative indexes and
	   display a more informative error message.  This might not
	   be appropriate, especially if we have a decent debugging
	   environment - JP. */
	try {
	    return elementData[index];
	} catch (ArrayIndexOutOfBoundsException e) {
	    throw new ArrayIndexOutOfBoundsException(index + " < 0");
	}
    }

    /**
     * Returns the first component of this vector.
     *
     * @return     the first component of this vector.
     * @exception  NoSuchElementException  if this vector has no components.
     * @since      JDK1.0
     */
    public final synchronized Object firstElement() {
	if (elementCount == 0) {
	    throw new NoSuchElementException();
	}
	return elementData[0];
    }

    /**
     * Returns the last component of the vector.
     *
     * @return  the last component of the vector, i.e., the component at index
     *          <code>size()&nbsp;-&nbsp;1</code>.
     * @exception  NoSuchElementException  if this vector is empty.
     * @since   JDK1.0
     */
    public final synchronized Object lastElement() {
	if (elementCount == 0) {
	    throw new NoSuchElementException();
	}
	return elementData[elementCount - 1];
    }

    /**
     * Sets the component at the specified <code>index</code> of this
     * vector to be the specified object. The previous component at that
     * position is discarded.
     * <p>
     * The index must be a value greater than or equal to <code>0</code>
     * and less than the current size of the vector.
     *
     * @param      obj     what the component is to be set to.
     * @param      index   the specified index.
     * @exception  ArrayIndexOutOfBoundsException  if the index was invalid.
     * @see        java.util.Vector#size()
     * @since      JDK1.0
     */
    public final synchronized void setElementAt(Object obj, int index) {
	if (index >= elementCount) {
	    throw new ArrayIndexOutOfBoundsException(index + " >= " +
						     elementCount);
	}
	elementData[index] = obj;
    }

    /**
     * Deletes the component at the specified index. Each component in
     * this vector with an index greater or equal to the specified
     * <code>index</code> is shifted downward to have an index one
     * smaller than the value it had previously.
     * <p>
     * The index must be a value greater than or equal to <code>0</code>
     * and less than the current size of the vector.
     *
     * @param      index   the index of the object to remove.
     * @exception  ArrayIndexOutOfBoundsException  if the index was invalid.
     * @see        java.util.Vector#size()
     * @since      JDK1.0
     */
    public final synchronized void removeElementAt(int index) {
	if (index >= elementCount) {
	    throw new ArrayIndexOutOfBoundsException(index + " >= " +
						     elementCount);
	}
	else if (index < 0) {
	    throw new ArrayIndexOutOfBoundsException(index);
	}
	int j = elementCount - index - 1;
	if (j > 0) {
	    System.arraycopy(elementData, index + 1, elementData, index, j);
	}
	elementCount--;
	elementData[elementCount] = null; /* to let gc do its work */
    }

    /**
     * Inserts the specified object as a component in this vector at the
     * specified <code>index</code>. Each component in this vector with
     * an index greater or equal to the specified <code>index</code> is
     * shifted upward to have an index one greater than the value it had
     * previously.
     * <p>
     * The index must be a value greater than or equal to <code>0</code>
     * and less than or equal to the current size of the vector.
     *
     * @param      obj     the component to insert.
     * @param      index   where to insert the new component.
     * @exception  ArrayIndexOutOfBoundsException  if the index was invalid.
     * @see        java.util.Vector#size()
     * @since      JDK1.0
     */
    public final synchronized void insertElementAt(Object obj, int index) {
	int newcount = elementCount + 1;
	if (index >= newcount) {
	    throw new ArrayIndexOutOfBoundsException(index
						     + " > " + elementCount);
	}
	if (newcount > elementData.length) {
	    ensureCapacityHelper(newcount);
	}
	System.arraycopy(elementData, index, elementData, index + 1, elementCount - index);
	elementData[index] = obj;
	elementCount++;
    }

    /**
     * Adds the specified component to the end of this vector,
     * increasing its size by one. The capacity of this vector is
     * increased if its size becomes greater than its capacity.
     *
     * @param   obj   the component to be added.
     * @since   JDK1.0
     */
    public final synchronized void addElement(Object obj) {
	int newcount = elementCount + 1;
	if (newcount > elementData.length) {
	    ensureCapacityHelper(newcount);
	}
	elementData[elementCount++] = obj;
    }

    /**
     * Removes the first occurrence of the argument from this vector. If
     * the object is found in this vector, each component in the vector
     * with an index greater or equal to the object's index is shifted
     * downward to have an index one smaller than the value it had previously.
     *
     * @param   obj   the component to be removed.
     * @return  <code>true</code> if the argument was a component of this
     *          vector; <code>false</code> otherwise.
     * @since   JDK1.0
     */
    public final synchronized boolean removeElement(Object obj) {
	int i = indexOf(obj);
	if (i >= 0) {
	    removeElementAt(i);
	    return true;
	}
	return false;
    }

    /**
     * Removes all components from this vector and sets its size to zero.
     *
     * @since   JDK1.0
     */
    public final synchronized void removeAllElements() {
	for (int i = 0; i < elementCount; i++) {
	    elementData[i] = null;
	}
	elementCount = 0;
    }

    /**
     * Returns a clone of this vector.
     *
     * @return  a clone of this vector.
     * @since   JDK1.0
     */
    public synchronized Object clone() {
	try {
	    Vector13 v = (Vector13)super.clone();
	    v.elementData = new Object[elementCount];
	    System.arraycopy(elementData, 0, v.elementData, 0, elementCount);
	    return v;
	} catch (CloneNotSupportedException e) {
	    // this shouldn't happen, since we are Cloneable
	    throw new InternalError();
	}
    }

    /**
     * Returns a string representation of this vector.
     *
     * @return  a string representation of this vector.
     * @since   JDK1.0
     */
    public final synchronized String toString() {
	int max = size() - 1;
	StringBuffer buf = new StringBuffer();
	Enumeration e = elements();
	buf.append("[");

	for (int i = 0 ; i <= max ; i++) {
	    buf.append(String.valueOf(e.nextElement()));
	    if (i < max) {
		buf.append(", ");
	    }
	}
	buf.append("]");
	return buf.toString();
    }
}
