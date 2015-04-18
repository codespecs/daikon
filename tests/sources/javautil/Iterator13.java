/*
 * @(#)Iterator.java	1.14 00/02/02
 *
 * Copyright 1997-2000 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */

package javautil;

/**
 * An iterator over a collection.  Iterator takes the place of Enumeration in
 * the Java collections framework.  Iterators differ from enumerations in two
 * ways: <ul>
 *	<li> Iterators allow the caller to remove elements from the
 *	     underlying collection during the iteration with well-defined
 * 	     semantics.
 *	<li> Method names have been improved.
 * </ul>
 *
 * @author  Josh Bloch
 * @version 1.14, 02/02/00
 * @see Collection
 * @see ListIterator
 * @see Enumeration
 * @since 1.2
 */
public interface Iterator13 {
    /**
     * Returns <tt>true</tt> if the iteration has more elements. (In other
     * words, returns <tt>true</tt> if <tt>next</tt> would return an element
     * rather than throwing an exception.)
     *
     * @return <tt>true</tt> if the iterator has more elements.
     */
    boolean hasNext();

    /**
     * Returns the next element in the interation.
     *
     * @return the next element in the iteration.
     * @exception NoSuchElementException iteration has no more elements.
     */
    Object next();

    /**
     * 
     * Removes from the underlying collection the last element returned by the
     * iterator (optional operation).  This method can be called only once per
     * call to <tt>next</tt>.  The behavior of an iterator is unspecified if
     * the underlying collection is modified while the iteration is in
     * progress in any way other than by calling this method.
     *
     * @exception UnsupportedOperationException if the <tt>remove</tt>
     *		  operation is not supported by this Iterator.
     
     * @exception IllegalStateException if the <tt>next</tt> method has not
     *		  yet been called, or the <tt>remove</tt> method has already
     *		  been called after the last call to the <tt>next</tt>
     *		  method.
     */
    void remove();
}
