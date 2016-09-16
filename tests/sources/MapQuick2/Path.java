package MapQuick2;

import MapQuick.*;
import java.util.Iterator;

/**
 * A Path models a sequence of nodes and the cost for travelling along
 * such a sequence.<p>
 *
 * Paths are immutable. <p>
 *
 * @specfield  cost :     double          // cost of traversing this Path
 * @specfield  elements : sequence        // the nodes in this Path
 * @endspec
 * <p>
 *
 * The cost of traversing a path must not decrease as the path is
 * extended with new nodes.<p>
 */
public interface Path {

    // Producers

    /**
     * Creates an extended path by adding a new node to its end.
     * @requires n != null &&
                 n is a valid node type for this particular path 
		 implementation
     * @return a new Path p such that
     *       p.elements = this.elements + [ n ]
     *    && p.cost >= this.cost
     */
    Path extend(Object n);

    // Observers

    /**
     * @return an Iterator that produces the contents of this.elements,
     *            in order
     */
    Iterator elements();

    /** @return this.cost */
    double cost();

} // Path


