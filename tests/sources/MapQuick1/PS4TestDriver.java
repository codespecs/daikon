package MapQuick1;

import MapQuick.*;
import java.io.*;
import java.util.*;
import junit.framework.Assert;

/**
 * This class implements a testing driver which reads test scripts...
 */

public class PS4TestDriver extends PS3TestDriver
{

  private final static boolean debug = false;

  public static void main(String args[]) throws Exception {
    if (args.length == 1) {
      runDirectly(new File(args[0]));
    } else {
      System.out.println(args.length + " arguments supplied, so running as pipe");
      runAsPipe();
    }
  }

  public static class CookieMonster extends OutputStream
  {
    public void write(int b) { }
    public void write(byte[] b) { }
    public void write(byte[] b, int off, int len) { }
  }

  public static void runDirectly(File testDir) throws Exception {
    // get the inputs
    File[] inputs = testDir.listFiles(new FilenameFilter() {
	public boolean accept(File file, String name) {
	  return name.endsWith(".test");
	}
      });

    if ((inputs == null) || (inputs.length == 0)) {
      throw new RuntimeException("No test input files found");
    }

    Arrays.sort(inputs);

    for (int i = 0; i < inputs.length; i++) {
      File input = inputs[i];
      System.err.println("Running test: " + input);
      PS4TestDriver drv =
	new PS4TestDriver(new InputStreamReader(new FileInputStream(input)),
			  new OutputStreamWriter(new CookieMonster()));
      drv.runTests();
    }
  }

  public static void runAsPipe() {
    try {
      PS4TestDriver td = new PS4TestDriver(new InputStreamReader(System.in),
					   new OutputStreamWriter(System.out));
      td.runTests();
    } catch (IOException e) {
      System.err.println(e.toString());
      e.printStackTrace(System.err);
    }
  }

  /**
   * @requies r != null && w != null
   *
   * @effects Creates a new PS4TestDriver which reads command from
   * <tt>r</tt> and writes results to <tt>w</tt>.
   */
  public PS4TestDriver(Reader r, Writer w)
  {
    super(r, w);
  }

  protected void executeCommand(String command, List arguments)
  {
    try {

      if (command.equals("CreateGeoNode")) {
        createGeoNode(arguments);
      } else if (command.equals("FindGeoPath")) {
        findGeoPath(arguments);
      } else {
        super.executeCommand(command, arguments);
      }

    } catch (Exception e) {
      output.println("Exception: " + e.toString());
      e.printStackTrace();
    }
  }

  private void createGeoNode(List arguments)
  {
    Assert.assertTrue(arguments.size() == 6);

    int arg = 0;
    String nodeName = (String) arguments.get(arg++);
    int lat1 = Integer.parseInt((String) arguments.get(arg++));
    int long1 = Integer.parseInt((String) arguments.get(arg++));
    int lat2 = Integer.parseInt((String) arguments.get(arg++));
    int long2 = Integer.parseInt((String) arguments.get(arg++));
    String segName = (String) arguments.get(arg++);

    createGeoNode(nodeName, lat1, long1, lat2, long2, segName);
  }

  private void createGeoNode(String nodeName, int lat1, int long1, int lat2, int long2, String segName)
  {
    GeoPoint p1 = new GeoPoint(lat1, long1);
    GeoPoint p2 = new GeoPoint(lat2, long2);
    GeoSegment node = new GeoSegment(segName, p1, p2);

    nodes.put(nodeName, node);

    output.println("created node " + nodeName + ": " +
		   lat1 + " " + long1 + " " + lat2 + " " + long2 + " " +
		   segName);
  }

  private void findGeoPath(List arguments)
  {
    Assert.assertTrue(arguments.size() >= 1);

    String graphName;
    List sourceArgs = new ArrayList();
    List destArgs = new ArrayList();

    Iterator iter = arguments.iterator();

    graphName = (String) iter.next();

    while (iter.hasNext()) {
      String s = (String) iter.next();
      if (s.equals("->")) {
        break;
      }
      sourceArgs.add(s);
    }
    while (iter.hasNext()) {
      destArgs.add(iter.next());
    }

    Assert.assertTrue(sourceArgs.size() >= 1);
    Assert.assertTrue(destArgs.size() >= 1);

    findGeoPath(graphName, sourceArgs, destArgs);
  }

  private void findGeoPath(String graphName, List sourceArgs, List destArgs)
  {
    // get the objects
    Graph g = (Graph) graphs.get(graphName);
    Assert.assertNotNull(g);

    Set goals = new LinkedHashSet();
    for (int i=0; i<destArgs.size(); i++) {
      String nodeName = (String) destArgs.get(i);
      GeoSegment n = (GeoSegment) nodes.get(nodeName);
      Assert.assertNotNull(n);
      goals.add(n);
    }

    Set starts = new LinkedHashSet();
    for (int i=0; i<sourceArgs.size(); i++) {
      String nodeName = (String) sourceArgs.get(i);
      GeoSegment n = (GeoSegment) nodes.get(nodeName);
      Assert.assertNotNull(n);
      CompositeRoutePath crp = new CompositeRoutePath(n, goals);
      starts.add(crp);
    }

    // output the result
    output.println("shortest path in " + graphName);
    try {

      CompositeRoutePath path = (CompositeRoutePath) PathFinder.findPath(g, starts, goals);
      output.print(path.route().directions(0.0));

    } catch (PathFinder.NoPathException e) {
      output.println("no path found");
    }

  }
}
