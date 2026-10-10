package MapQuick;

import java.util.Iterator;

/** A Directions is a description of how to travel from one Address to
 *   another.
 *   @specfield start      : Address // starting point
 *   @specfield end        : Address // destination
 *   @specfield length     : double  // travelling distance, in miles (not rounded)
 *   @specfield directions : sequence[String] // directions for travelling from start to end.  Each String in the sequence must be one line of directions, in the format specified in <a href="../../../ps6/ps6.html#dirs-format">PS6 text based interface</a>, <b>excluding</b> the first line which states where you start <b>and</b> the final line which gives the length of the trip.  The strings <b>must not</b> include newlines at their ends.  The order of the Strings in the sequence must correspond to the order of the lines in the directions from start to end.
 *   @endspec
 */
public interface Directions {

    // a Directions is the result of a query to a DirectionsFinder

    /** @return this.length. */
    public double getLength ();

    /** @return an Iterator of Strings over this.directions */
    public Iterator getDirections ();

    /** @return this.start */
    public Address getStart ();

    /** @return this.end */
    public Address getEnd ();
}
