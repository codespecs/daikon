package MapQuick;

import java.util.*;

/**
 * A PriorityQueue is a collection, each of whose elements has an
 * associated priority.<p>
 *
 * @specfield elements : set                // contents of the priority queue
 * @specfield priority : Object -> double   // priority of each element
 * @endspec
 */
public class PriorityQueue
{

    private final List heap;
    // elts is redundant, and allows a fast implementation of contains()
    private final LinkedHashSet elts;

    //
    // Rep Invariant:
    //
    //
    //

    /**
     * @effects creates a PriorityQueue p, where p.elements is empty
     */
    public PriorityQueue()
    {
	heap = new ArrayList();
	elts = new LinkedHashSet();
	// the first element of heap is a dummy element, so that
	// the rest of the code can count from 1
	heap.add(new Object());
    }

    /**
     * @returns true iff elt is in this.elements
     */
    public boolean contains(Object elt)
    {
	return elts.contains(elt);
    }

    /**
     * @returns the size of this.elements
     */
    public int size()
    {
        // the heap has a dummy element
	return heap.size()-1;
    }

    /**
     * @modifies this
     *
     * @effects adds elt to this.elements and sets this.priority(elt)
     * = priority
     *
     * @throws DuplicateElementException if this.elements contains elt
     */
    public void insert(double priority, Object elt) throws DuplicateElementException
    {
	if (contains(elt)) {
	    throw new DuplicateElementException("Queue already contains "+elt);
	}
	Entry e = new Entry(priority, elt);
	heap.add(e);
	elts.add(elt);
	int i = size();
	while ((i > 1) &&
	       (heapGet((int)(i/2)).key > priority)) {
	    heap.set(i, heapGet((int)(i/2)));
	    i = (int)(i/2);
	}
	heap.set(i, e);
    }

    /**
     * Removes and returns the highest-priority element.
     *
     * @modifies this
     *
     * @returns elt such that elt is in this.elements && for all elt2
     * in this.elements, priority(elt) &lt;= priority(elt2)
     *
     * @effects removes elt from this.elements and from the domain of priority
     *
     * @throws NoSuchElementException if this.elements is empty
     */
    public Object extractMin() throws NoSuchElementException
    {
	if (heap.size() <= 1) {
	    throw new NoSuchElementException("empty PriorityQueue");
	}
	Object min = heapGet(1).value;
	elts.remove(min);
	int lastIndex = heap.size() - 1;
	heap.set(1, heapGet(lastIndex));
	heap.remove(lastIndex);
	heapify(1);
	return min;
    }

    /**
     * @requies 1 &lt;= i &lt; heap.size();
     *
     * @returns the ith element of the heap
     */
    private Entry heapGet(int i)
    {
      return (Entry) heap.get(i);
    }

    /**
     * @requires subtrees routed at 2i and 2i+1 satisfy the heap
     * property
     *
     * @modifies this
     *
     * @effects rearranges the subtree routed at i so that the min
     * heap property is satisfied
     */
    private void heapify(int i)
    {
	int smallest;
	if ((2*i < heap.size()) &&
	    (heapGet(2*i).key <
	     heapGet(i).key)) {
	    smallest = 2*i;
	}
	else {
	    smallest = i;
	}

	if ((2*i+1 < heap.size()) &&
	    (heapGet(2*i+1).key <
	     heapGet(smallest).key)) {
	    smallest = 2*i+1;
	}

	if (i != smallest) {
	    Object temp = heapGet(i);
	    heap.set(i, heapGet(smallest));
	    heap.set(smallest, temp);
	    heapify(smallest);
	}
    }

    /**
     * Entry is a record class for the storage of elements in PriorityQueue.
     */
    private class Entry
    {
	final double key;
	final Object value;

	public Entry(double key, Object value) {
	    this.key = key;
	    this.value = value;
	}
    }


    /**
     * DuplicateElementException is an unchecked exception which
     * signals that an element is already in a PriorityQueue.
     */
    public class DuplicateElementException extends RuntimeException
    {
        DuplicateElementException() { }
        DuplicateElementException(String s) { super(s); }
    }
}
