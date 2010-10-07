package MapQuick1;

import MapQuick.*;
import java.util.*;

/** A RouteDirections contains directions for travelling between
 *  two points along a particular CompositeRoutePath.  It implements
 *  the Directions interface.
 *
 * @specfield segments: [StreetSegment]
 *    // ordered list of segments along this path
 * @specfield start: Address // Starting address
 * @specfield end: Address // Ending address
 * @endspec
 */
class RouteDirections implements Directions
{
    /* Representation: we store the start and end addresses and a
     * CompositeRoute for the actual route.  We also store the
     * first and last segments in the route externally to help
     * calculate directions.
     *
     * a = AF(t), where
     *   a.start = t.start
     *   a.end = t.end
     *   a.segments = AF(t.route)
     *
     * RI: no field is null.  start is on firstSeg, and end is on
     * lastSeg.  firstSeg is the first GeoSegment in route, and
     * lastSeg is the last GeoSegment.
     */
    private Address start;
    private Address end;
    private CompositeRoute route;
    private StreetSegment firstSeg, lastSeg;

    /** Construct a new RouteDirections given start and end addresses and
     *  a CompositeRoutePath.
     */
    public RouteDirections(Address start, Address end, CompositeRoutePath crp)
    {
        this.start = start;
        this.end = end;

        // Walk down the path and get the first and last segment.
        // Simultaneously build a CompositeRoute of the segments.
        Iterator iter = crp.elements();
        // Every CRP will have at least one element.
        firstSeg = (StreetSegment)iter.next();
        lastSeg = firstSeg;
        route = new CompositeRoute(firstSeg);
        while (iter.hasNext())
        {
            lastSeg = (StreetSegment)iter.next();
            route = route.addSegment(lastSeg);
        }

        // If the first and last segments are the same, we might need to
        // do some special-case testing.
        if (firstSeg == lastSeg)
        {
            // The end address must be further down the street than the
            // start segment.
            if (firstSeg.fractionDist(start.getNum()) >
                firstSeg.fractionDist(end.getNum()))
            {
                // Oops, wrong order.  Reverse the segment.
                firstSeg = (StreetSegment)firstSeg.reverse();
                lastSeg = firstSeg;
                route = new CompositeRoute(firstSeg);
            }
        }
    }

    /** @return the length of the route */
    public double getLength()
    {
        return route.length();
    }
    
    public Iterator getDirections()
    {
        // Build up a list with the actual directions.
        List directions = new LinkedList();
        
        // Get a start direction.
        double startDir = firstSeg.heading();
        // If the start address is on the left, add 90, else subtract 90.
        //         ^
        //      --+|  (From the left of the segment with heading 0, the
        //         |   initial heading is 90.)
        if (firstSeg.isOnLeft(start.getNum()))
            startDir += 90.0;
        else
            startDir -= 90.0;
        // Normalize the direction.
        if (startDir >= 360.0)
            startDir -= 360.0;
        if (startDir < 0.0)
            startDir += 360.0;

        // Get actual directions from the CompositeRoute.
        ElementaryRoute[] routes = route.elementaryRoutes();
        for (int i = 0; i < routes.length; i++)
        {
            directions.add(routes[i].directions(startDir).trim());
            startDir = routes[i].endHeading();
        }

        // State which side the destination is on.
        if (lastSeg.isOnLeft(end.getNum()))
            directions.add(end + " is on your left");
        else
            directions.add(end + " is on your right");

        // Return an iterator over the results.
        return Collections.unmodifiableList(directions).iterator();
    }
    
    /** @return the starting address of the route */
    public Address getStart()
    {
        return start;
    }
    
    /** @return the ending address of the route */
    public Address getEnd()
    {
        return end;
    }
}
