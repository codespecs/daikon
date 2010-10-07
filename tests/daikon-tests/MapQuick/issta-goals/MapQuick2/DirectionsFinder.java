package MapQuick2;

import MapQuick.*;
import java.util.*;
import junit.framework.Assert;

/** A DirectionsFinder produces directions for traveling from one
 *   address to another in a particular geographical area.
 *   <p>
 *   @specfield segments : set[StreetSegment]
 *      // the set of StreetSegments which represents the geographical
 *         area for which this DirectionsFinder produces directions
 *   @endspec
 */
public class DirectionsFinder
{
    /*
     * Private state
     * -------------
     *
     * Beyond construction, the only operation on a DirectionsFinder
     * is represented by the getDirections method.  For this, we need
     * to be able to pull out a StreetSegment from the database based
     * on its name and ZIP code.  This suggests that a useful
     * representation is a mapping from ZIP code to a mapping of names
     * to sets of street segments.
     *
     * (That is: segments{"02139"} is a Map;
     *           segments{"02139"}{"Vassar St."} is a Set of StreetSeg)
     */
    private Map segments;

    /* We also need to be able to trace paths through our list of
     * segments.  The Graph abstraction is useful for this.  Here
     * the street segments are nodes, and intersections are
     * represented by edges between adjacent nodes.
     */
    private Graph graph;


    // Used for debugging.  If set, segments whose names start with
    // this are printed to stderr
    private String dumpPrefix;

    /* AF(t) = { seg | exists key in t.segments s.t.
     *                 seg is in t.segments[key] }
     * RI: segments is non-null.  The keys of segments are non-null and
     *     are Strings.  The values are non-null and are Maps.  In the
     *     inner Maps, the keys are non-null and are Strings.  The values
     *     are non-null and are Sets; these Sets are non-empty, and the
     *     contents are non-null and are StreetSegments.
     *
     * RI: graph is non-null.  Every node in graph is a StreetSegment,
     *     and is contained in segments.  There is an edge from node
     *     s1 to node s2 if and only if the endpoint of s1 is the
     *     startpoint of s2.
     */

    /* @effects: throws an exception if the rep invariant does
     *           not hold.  No effects otherwise. */
    public void checkRep()
    {
        /* CHECK: segments is non-null. */
        Assert.assert(segments != null);

        /* Get an iterator over the set of keys. */
        Iterator zipIter = segments.keySet().iterator();

        /* Go over each key in turn. */
        while (zipIter.hasNext())
        {
            Object zip = zipIter.next();

            /* CHECK: zip is non-null and is a String. */
            Assert.assert(zip != null);
            Assert.assert(zip instanceof String);

            /* CHECK: segments[zip] is non-null and is a Map. */
            Object byName = segments.get(zip);
            Assert.assert(byName != null);
            Assert.assert(byName instanceof Map);
            Map nameMap = (Map)byName;

            /* Now iterate through this Map. */
            Iterator nameIter = nameMap.keySet().iterator();
            while (nameIter.hasNext())
            {
                Object key = nameIter.next();

                /* CHECK: key is non-null and is a String. */
                Assert.assert(key != null);
                Assert.assert(key instanceof String);

                /* CHECK: nameMap[key] is non-null and is a Set. */
                Object value = nameMap.get(key);
                Assert.assert(value != null);
                Assert.assert(value instanceof Set);

                /* CHECK: nameMap[key] is non-empty. */
                Set valSet = (Set)value;
                Assert.assert(!valSet.isEmpty());

                /* Now iterate over each of the things in this set. */
                Iterator segIter = valSet.iterator();
                while (segIter.hasNext())
                {
                    Object seg = segIter.next();

                    /* CHECK: seg is non-null and is a StreetSegment. */
                    Assert.assert(seg != null);
                    Assert.assert(seg instanceof StreetSegment);
                }
            }
        }

        /* TODO: Add rep invariant checks for the rest of this. */
    }

    /** Produces a DirectionsFinder for a given database.
     * @requires zf is null or is a Collection of String
     * @return A DirectionsFinder whose set of segments is
     *  { s | s in (new StreetSegReader(databaseName)).streetSegs () &&
     *        (s.leftZip in zf || s.rightZip in zf ||
     *         (s.leftZip.trim () == s.rightZip.trim () == "") ||
     *         zf == null) }
     * @throws InvalidDatabaseException if databaseName does not refer to a
     *  valid tiger database.
     */
    public static DirectionsFinder getDirectionsFinder(String databaseName,
                                                       Collection zf)
        throws InvalidDatabaseException
    {
      return getDirectionsFinder(databaseName, zf, null);
    }

    //@ ensures \result != null // code not instrumented
    // package vis.
    static DirectionsFinder getDirectionsFinder(String databaseName,
						Collection zf,
						String dumpPrefix)
        throws InvalidDatabaseException
    {
        StreetSegReader ssr;

	System.err.println("Making DF for " + databaseName);
	System.err.flush();

        try
        {
            ssr = new StreetSegReader(databaseName);
        }
        catch (StreetSegReader.InvalidSourceException e)
        {
            throw new InvalidDatabaseException("Invalid Tiger database " +
                                               databaseName);
        }

        return new DirectionsFinder(ssr.streetSegments(), zf, dumpPrefix);
    }

    /** Creates a new DirectionsFinder from an Iterator of StreetSegments.
     *
     * @requires zf is null or is a Collection of String; (dumpPrefix may be null)
     * @modifies ssIter
     * @effects Populates this with every StreetSegment in ssIter whose
     *   left or right zip code is in zf.  If zf is null or empty, use every
     *   segment.  Leaves ssIter empty.
     */
    public DirectionsFinder(Iterator ssIter, Collection zf, String dumpPrefix)
    {
        // Initialize instance variables.
        segments = new HashMap();
        graph = new Graph();
	this.dumpPrefix = dumpPrefix;

        // Also keep two maps locally for quickly finding things to
        // attach edges to.
        Map mapByStart = new HashMap();
        Map mapByEnd = new HashMap();

        // Walk through each segment we get back.
        while (ssIter.hasNext())
        {
            StreetSegment seg = (StreetSegment)ssIter.next();

	    if ((dumpPrefix != null) && seg.name().startsWith(dumpPrefix)) {
	        System.err.println(seg.toString());
	    }

            // If the segment is eligible, add it to both our index
            // by name and to the graph.
            if (isSegmentEligible(seg, zf))
            {
                addToSegments(seg);
                addToGraph(seg, mapByStart, mapByEnd);
                seg = (StreetSegment)seg.reverse();
                addToSegments(seg);
                addToGraph(seg, mapByStart, mapByEnd);
            }
        }

        // All done.  Check that the rep invariant holds.
        checkRep();
    }

    /**

    /** Determines whether or not a given segment should be part of
     * our map.
     *
     * @requires zf is null or is a Collection of String
     * @return true if:
     *   the left ZIP code of seg is in zf;
     *   the right ZIP code of seg is in zf;
     *   the left and right ZIP codes are both "";
     *   zf is empty; or
     *   zf is null.
     * @return false otherwise.
     */
    private boolean isSegmentEligible(StreetSegment seg, Collection zf)
    {
        // If the collection is null or empty, any segment is eligible.
        if (zf == null)
            return true;
        if (zf.isEmpty())
            return true;

        // If both side ZIP codes are empty, the segment is eligible.
        if (seg.leftZip().trim().equals("") &&
            seg.rightZip().trim().equals(""))
            return true;

        // Otherwise, check both the left and right zip codes.
        if (zf.contains(seg.leftZip()))
            return true;
        if (zf.contains(seg.rightZip()))
            return true;

        // Otherwise, the segment is ineligible.
        return false;
    }

    /** Generic helper function to get a Map of street names to Sets of
     * StreetSegments, given a ZIP code.
     *
     * @requires zip is non-null
     * @modifies segments
     * @effects If zip is in segments, return the map that it corresponds
     *   to.  Otherwise, if create is true, create a new HashMap, add
     *   it to segments under zip, and return the map.  If zip isn't already
     *   in segments and create is false, return null.
     */
    private Map findZipMap(String zip, boolean create)
    {
        // Just go off and get it.
        Map theMap = (Map)segments.get(zip);

        // If we didn't find it, create one if necessary.
        if (theMap == null && create)
        {
            theMap = new HashMap();
            segments.put(zip, theMap);
        }

        return theMap;
    }

    /** Generic helper function to add a value to a set.
     *
     * @param value: Value to add to the set
     * @param key: Key to add the value under
     * @param map: Map indexing keys to Sets of values
     * @modifies map
     * @effects If map contains a Set for key, adds value to that
     *   set.  Otherwise, creates a Set containing value and adds
     *   it to map under key.
     */
    private static void addToSet(Object value, Object key, Map map)
    {
        // Pull the appropriate set out of the map.
        Set theSet = (Set)map.get(key);

        // If we lost on finding the set, create one now.  Use a
        // HashSet under the assumption that most things will have
        // hashCode()s and that we'll rarely be adding things.
        if (theSet == null)
        {
            theSet = new HashSet();
            map.put(key, theSet);
        }

        // Add the value to the set.
        theSet.add(value);
    }

    /** Adds a segment to the map of names to segments.
     *
     * @requires seg is not already in segments
     * @modifies segments
     * @effects Adds seg to segments
     */
    private void addToSegments(StreetSegment seg)
    {
        Map zipMap;

        // Add to the set indexed by the left ZIP code.
        zipMap = findZipMap(seg.leftZip(), true);
        addToSet(seg, seg.name(), zipMap);

        // If the left and right ZIP codes are different, do the same
        // for the right ZIP code.
        if (!seg.leftZip().equals(seg.rightZip()))
        {
            zipMap = findZipMap(seg.rightZip(), true);
            addToSet(seg, seg.name(), zipMap);
        }
    }

    /** Adds a segment to the graph.
     *
     * @param seg: The StreetSegment to be added
     * @param mapByStart: A Map of Sets of StreetSegments, keyed on the
     *   starting GeoPoint of the segment
     * @param mapByEnd: A Map of Sets of StreetSegments, keyed on the
     *   ending GeoPoint of the segment
     * @requires: no parameter is null, the map parameters have type
     *   signatures as described above
     * @modifies graph, mapByStart, mapByEnd
     * @effects Adds seg to graph, and adds appropriate edges between
     *   segments in the graph and the new segment.  Adds seg as
     *   to mapByStart and mapByEnd with appropriate keys,
     */
    private void addToGraph(StreetSegment seg, Map mapByStart, Map mapByEnd)
    {
        Iterator iter;
        Set segs;

        // 1. Just go off and add the node.
        try {
            graph.addNode(seg);
        }
        catch (Exception e)
        {
            return;
        }

        // 2. Add edges from this node.
        segs = (Set)mapByStart.get(seg.p2());
        // Do nothing if there are no such segments.
        if (segs != null)
        {
            iter = segs.iterator();
            while (iter.hasNext())
            {
                StreetSegment other = (StreetSegment)iter.next();
                graph.addEdge(seg, other);
            }
        }

        // 3. Add edges to this node.
        segs = (Set)mapByEnd.get(seg.p1());
        if (segs != null)
        {
            iter = segs.iterator();
            while (iter.hasNext())
            {
                StreetSegment other = (StreetSegment)iter.next();
                graph.addEdge(other, seg);
            }
        }

        // 4. Add this node to both maps.
        addToSet(seg, seg.p1(), mapByStart);
        addToSet(seg, seg.p2(), mapByEnd);
    }

    /** Gets directions between two addresses.
     *
     * @return A String providing directions of travel from the address
     *  specified by the first three string arguments to the address
     *  specified by the last three string arguments. The result string
     *  may indicate an invalid address or that there is no path between
     * the two addresses. The result string should be exactly what
     *  would be printed by the <a href="../ps6.html#text">PS6 text-based
     * interface</a>, including any trailing newlines.
     */
    public String getDirections (String startNumber, String startStreet,
                                 String startZip, String destinationNumber,
                                 String destinationStreet,
                                 String destinationZip)
    {
        checkRep();

        // Parse the start and end addresses into Addresses.
        Address start = makeAddress(startNumber, startStreet, startZip);
        Address end = makeAddress(destinationNumber, destinationStreet,
                                  destinationZip);

        if (start == null) {
	  return "Bad address: " +
	    startNumber + " " +
	    startStreet + " " +
	    startZip +
	    "\n";
	}
        if (end == null) {
	  return "Bad address: " +
	    destinationNumber + " " +
	    destinationStreet + " " +
	    destinationZip +
	    "\n";
	}

        String result = "";
        try
        {
            // Actually get directions.
            Directions dir = getDirections(start, end);

            // Build up a list of result directions.
            result = "Start at " + start + "\n";
            Iterator iter = dir.getDirections();
            while (iter.hasNext())
            {
                String theDir = (String)iter.next();
                result = result + theDir + "\n";
            }
            double l = dir.getLength() * 10;
            l = Math.round(l);
            l = l / 10;
            result = result + "Trip length: " + l + " miles\n";
        }
        catch (InvalidAddressException e)
        {
	    result = e.getMessage() + "\n";
        }
        catch (NoPathException e)
        {
	    result = e.getMessage() + "\n";
        }

        checkRep();
        return result;
    }

    /** Makes an Address from a set of three strings.
     *
     * @return A new Address corresponding to the provided parameters,
     *   or null if the address is not parseable.
     */
    private Address makeAddress(String num, String street, String zip)
    {
        int theNum;
        try
        {
            theNum = Integer.parseInt(num);
        }
        catch (NumberFormatException e)
        {
            return null;
        }

        return new Address(theNum, street, zip);
    }

    /** Gets directions from start to end.
     *
     * @throws InvalidAddressException if start or end is not in the
     *  geographical area covered by this DirectionsFinder.
     * @throws NoPathException if no path could be found from start to end.
     * @return A Directions providing directions of travel from start to end.
     */
    public Directions getDirections (Address start, Address end)
        throws InvalidAddressException, NoPathException
    {
        checkRep();

        // Find start and end segments.  These may throw
        // InvalidAddressExceptions; let those just be passed on.
        Set startSegSet = findSegments(start);
        Set endSegSet = findSegments(end);

        // Turn these into appropriate parameters for PathFinder.
        // In particular, the start set needs to be a Set of
        // one-element Paths.
        Set startPathSet = new HashSet();
        Iterator segIter = startSegSet.iterator();
        CompositeRoutePath crp;
        while (segIter.hasNext())
        {
            StreetSegment seg = (StreetSegment)segIter.next();
            crp = new CompositeRoutePath(seg);
            startPathSet.add(crp);
        }

        // Okay, find the shortest path.
        try
        {
            Path result = PathFinder.findPath(graph, startPathSet, endSegSet);
            crp = (CompositeRoutePath)result;
        }
        catch (PathFinder.NoPathException e)
        {
            throw new NoPathException("No path from " + start + " to " + end);
        }

        // Create and return a RouteDirections object.
        return new RouteDirections(start, end, crp);
    }

    /** Returns a Set of all StreetSegments containing a given address.
     *
     * @requires addr is not null
     * @return A non-empty Set of StreetSegments containing addr.
     * @throws InvalidAddressException if addr doesn't match any
     *   segments
     */
    private Set findSegments(Address addr)
        throws InvalidAddressException
    {
        // 1. Look up the ZIP code to get a map of street names to segments.
        Map byName = (Map)segments.get(addr.getZipcode());
        if (byName == null)
            throw new InvalidAddressException("No such zipcode: " + addr);

        // 2. Look up the street by name to get a Set of segments.
        Set segSet = (Set)byName.get(addr.getName());
        if (segSet == null)
            throw new InvalidAddressException("No such street: " + addr);

        // 3. Trim this down to a Set of segments containing our
        // number.
        Set theSet = new HashSet();
        Iterator segIter = segSet.iterator();
        while (segIter.hasNext())
        {
            StreetSegment seg = (StreetSegment)segIter.next();
            if (seg.contains(addr.getNum()))
                theSet.add(seg);
        }

        // 4. If we found nothing, throw an exception.
        if (theSet.isEmpty())
            throw new InvalidAddressException("No such number: " + addr);

        // 5. All done!
        return theSet;
    }

}
