package MapQuick2;

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
public class RouteDirections implements Directions
{
    /*@ invariant this.start != null; */
    /**@ invariant this.start.streetNum >= 1; */
    /**@ invariant this.start.streetName != null; */
    /**@ invariant this.start.zipcode != null; */
    /*@ invariant this.end != null; */
    /**@ invariant this.end.streetNum >= 1; */
    /**@ invariant this.end.streetName != null; */
    /**@ invariant this.end.zipcode != null; */
    /*@ invariant this.route != null; */
    /**@ invariant this.route.elements != null; */
    /*@ invariant this.firstSeg != null; */
    /*@ invariant this.lastSeg != null; */
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
    /*@ spec_public */ private Address start;
    /*@ spec_public */ private Address end;
    /*@ spec_public */ private CompositeRoute route;
    /*@ spec_public */ private StreetSegment firstSeg, lastSeg;

    /*@ requires start != null; */
    /**@ requires start.streetNum >= 1; */
    /**@ requires start.streetName != null; */
    /**@ requires start.zipcode != null; */
    /*@ requires end != null; */
    /**@ requires end.streetNum >= 1; */
    /**@ requires end.streetName != null; */
    /**@ requires end.zipcode != null; */
    /*@ requires crp != null; */
    /*@ ensures start == this.start; */
    /**@ ensures start.streetNum == this.start.streetNum; */
    /**@ ensures start.streetName == this.start.streetName; */
    /**@ ensures start.zipcode == this.start.zipcode; */
    /*@ ensures end == this.end; */
    /**@ ensures end.streetNum == this.end.streetNum; */
    /**@ ensures end.streetName == this.end.streetName; */
    /**@ ensures end.zipcode == this.end.zipcode; */
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
        firstSeg = (StreetSegment)iter.next(); //@ nowarn Cast // incompleteness (global analysis)
        lastSeg = firstSeg;
        route = new CompositeRoute(firstSeg);
        while (iter.hasNext())
        {
            lastSeg = (StreetSegment)iter.next(); //@ nowarn Cast // incompleteness (global analysis)
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

    /*@ also_requires this.start.streetNum != this.end.streetNum; */
    /** @return the length of the route */
    public double getLength()
    {
        return route.length();
    }

    /*@ also_requires this.start.streetNum != this.end.streetNum; */
    /*@ also_ensures \result != null; */
    public Iterator getDirections()
    {
        // Build up a list with the actual directions.
        List directions = new LinkedList();
	//@ set directions.elementType = \type(Object) // dumb

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
        return directions.iterator(); // Collections.unmodifiableList(directions).iterator();
    }

    /*@ also_requires this.start.streetNum != this.end.streetNum; */
    /*@ also_ensures \result == this.start; */
    /**@ also_ensures \result == \old(this.start); */
    /**@ also_ensures \result.streetNum == this.start.streetNum; */
    /**@ also_ensures \result.streetNum == \old(this.start.streetNum); */
    /**@ also_ensures \result.streetName == this.start.streetName; */
    /**@ also_ensures \result.streetName == \old(this.start.streetName); */
    /**@ also_ensures \result.zipcode == this.start.zipcode; */
    /**@ also_ensures \result.zipcode == \old(this.start.zipcode); */
    /** @return the starting address of the route */
    public Address getStart()
    {
        return start;
    }

    /*@ also_requires this.start.streetNum != this.end.streetNum; */
    /*@ also_ensures \result == this.end; */
    /**@ also_ensures \result == \old(this.end); */
    /**@ also_ensures \result.streetNum == this.end.streetNum; */
    /**@ also_ensures \result.streetNum == \old(this.end.streetNum); */
    /**@ also_ensures \result.streetName == this.end.streetName; */
    /**@ also_ensures \result.streetName == \old(this.end.streetName); */
    /**@ also_ensures \result.zipcode == this.end.zipcode; */
    /**@ also_ensures \result.zipcode == \old(this.end.zipcode); */
    /**@ also_ensures \result.streetNum != this.start.streetNum; */
    /** @return the ending address of the route */
    public Address getEnd()
    {
        return end;
    }
}
