package MapQuick;

import MapQuick2.*;
import java.io.File;
import java.util.*;

// PAG Test adds value coverge to DirectionsFinderTest
public class PAGTest {

  public static void main(String[] args) throws Exception {
    // Cover Address.equals with false
    for (int i = 1; i <= 5; i++) {
      (new Address(((i*3)%10), i+" Rd.", "0000"+i))
	.equals((Object) (new Address(i, i+"St.", "0000"+i)));
    }
    Address add = new Address(1, "Street", "12345");
    add.equals((Object) new Address(1, new String("Street"), new String("12345")));
    add.equals((Object) null); add.equals((Address) null);

    // Cover Graph with a different type, and false containsNode
    Graph g = new Graph();
    g.addNode(g); g.containsNode(g); g.addNode(add);
    g.containsNode(new Graph()); g.containsNode(new Object());
    g.addEdge(g, add); g.childrenOf(g);

    // Cover PriorityQueue with a different type
    PriorityQueue q = new PriorityQueue();
    q.insert(1.0, q); q.contains(q); q.extractMin();

    // Cover GeoSegment hashCode crap
    GeoPoint gp00 = new GeoPoint(0,0), gp11 = new GeoPoint(1, 1);
    GeoSegment gs00 = new GeoSegment("", gp00, gp00); gs00.hashCode();

    // Cover varied database
    String zip = "00000";
    StreetNumberSet sns = new StreetNumberSet("1-3"), sns2 = new StreetNumberSet("9-11");
    GeoPoint[] gp = new GeoPoint[] {
      new GeoPoint(-100,  100), new GeoPoint( 100,  100),
      new GeoPoint(-100, -100), new GeoPoint( 100, -100),
      new GeoPoint(-100, -200), new GeoPoint(   0,    0),
      new GeoPoint(-200, -300),
    };
    List ss = Arrays.asList(new StreetSegment[] {
      new StreetSegment(gp[0], gp[1], "foo1", sns, sns, zip, zip, StreetClassification.UNKNOWN, true),
      new StreetSegment(gp[1], gp[3], "foo2", sns, sns, zip, zip, StreetClassification.UNKNOWN, true),
      new StreetSegment(gp[3], gp[2], "foo3", sns, sns, zip, zip, StreetClassification.UNKNOWN, true),
      new StreetSegment(gp[2], gp[0], "foo4", sns, sns, zip, zip, StreetClassification.UNKNOWN, true),
      new StreetSegment(gp[4], gp[3], "foo5", sns, sns, zip, zip, StreetClassification.UNKNOWN, true),
      new StreetSegment(gp[5], gp[0], "foo6", sns, sns, zip, zip, StreetClassification.UNKNOWN, true),
      new StreetSegment(gp[4], gp[6], new String("foo5"), sns2, sns2, zip, zip, StreetClassification.UNKNOWN, true),
    });
    Collection zf = Collections.singleton("00000");
    DirectionsFinder df = new DirectionsFinder(ss.iterator(), zf, null);
    df.getDirections("1", "foo1", zip, "3", "foo2", zip);
    df.getDirections("1", "foo1", zip, "3", "foo5", zip);
    df.getDirections("3", "foo6", zip, "1", "foo3", zip);
    df.getDirections("3", "foo2", zip, "9", "foo5", zip);

    // Cover RouteDirections justification
    for (int i=0; i<100; i++) { new RouteDirections(add, add, new CompositeRoutePath((StreetSegment) ss.get(0))); }

    // Cover KillfileReader
    for (int i=0; i<100; i++) { KillfileReader.fromDir(new File(TextUITest.tinyPath)); }

    // Run this last, since it exits explicitly
    DirectionsFinderTest.main(new String[0]);
  }

}
