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

// Split out from Vector.java source

final
class VectorEnumerator implements Enumeration {
    Vector vector;
    int count;

    VectorEnumerator(Vector v) {
	vector = v;
	count = 0;
    }

    public boolean hasMoreElements() {
	return count < vector.elementCount;
    }

    public Object nextElement() {
	synchronized (vector) {
	    if (count < vector.elementCount) {
		return vector.elementData[count++];
	    }
	}
	throw new NoSuchElementException("VectorEnumerator");
    }
}
