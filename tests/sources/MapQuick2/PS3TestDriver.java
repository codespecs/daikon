package MapQuick2;

import MapQuick.*;
import java.io.*;
import java.util.*;
import junit.framework.Assert;

/**
 * This class implements a testing driver which reads test scripts
 * from files for testing Graph and PathFinder.
 */

public class PS3TestDriver {

  public static void main(String args[]) {
    try {

      PS3TestDriver td = new PS3TestDriver(new InputStreamReader(System.in),
					   new OutputStreamWriter(System.out));

      td.runTests();

    } catch (IOException e) {
      System.err.println(e.toString());
      e.printStackTrace(System.err);
    }
  }

  /** String -> Graph: maps the names of graphs to the actual graph */
  protected final LinkedHashMap graphs = new LinkedHashMap();
  /** String -> Object: maps the names of nodes to the actual node */
  protected final LinkedHashMap nodes = new LinkedHashMap();
  protected final PrintWriter output;
  protected final BufferedReader input;

  /**
   * @requies r != null && w != null
   *
   * @effects Creates a new PS3TestDriver which reads command from
   * <tt>r</tt> and writes results to <tt>w</tt>.
   */
  public PS3TestDriver(Reader r, Writer w) {
    input = new BufferedReader(r);
    output = new PrintWriter(w);
  }

  /**
   * @effects Executes the commands read from the input and writes results to the output
   * @throws IOException if the input or output sources encounter an IOException
   */
  public void runTests()
    throws IOException
  {
    String inputLine;
    while ((inputLine = input.readLine()) != null) {
      if (inputLine.trim().length() == 0 ||
          inputLine.charAt(0) == '#') {
        // echo blank and comment lines
        output.println(inputLine);
	output.flush();
        continue;
      }

      // separate the input line on white space
      StringTokenizer st = new StringTokenizer(inputLine);
      if (st.hasMoreTokens()) {
        String command = st.nextToken();

        List arguments = new ArrayList();
        while (st.hasMoreTokens()) {
          arguments.add(st.nextToken());
        }
        executeCommand(command, arguments);
	output.flush();
      }

    }
  }

  protected void executeCommand(String command, List arguments) {
    //    System.err.println("PS3TestDriver.execute");
    try {
      if (command.equals("CreateGraph")) {
        createGraph(arguments);
      } else if (command.equals("CreateNode")) {
        createNode(arguments);
      } else if (command.equals("AddNode")) {
        addNode(arguments);
      } else if (command.equals("AddEdge")) {
        addEdge(arguments);
      } else if (command.equals("ListNodes")) {
        listNodes(arguments);
      } else if (command.equals("ListChildren")) {
        listChildren(arguments);
      } else if (command.equals("FindPath")) {
        findPath(arguments);
      } else {
        output.println("Unrecognized command: " + command);
      }
    } catch (Exception e) {
      output.println("Exception: " + e.toString());
      e.printStackTrace();
    }
  }

  private void createGraph(List arguments) {
    if (arguments.size() != 1) {
      throw new CommandException("Bad arguments to CreateGraph: " + arguments);
    }

    String graphName = (String) arguments.get(0);
    createGraph(graphName);
  }

  private void createGraph(String graphName) {
    // Insert your code here.

    graphs.put(graphName, new Graph());

    output.println("created graph " + graphName);
  }

  private void createNode(List arguments) {
    if (arguments.size() != 2) {
      throw new CommandException("Bad arguments to createNode: " + arguments);
    }

    String nodeName = (String) arguments.get(0);
    String cost = (String) arguments.get(1);

    createNode(nodeName, cost);
  }

  private void createNode(String nodeName, String cost) {
    int ncost = Integer.parseInt(cost);

    WeightedNode n = new WeightedNode(nodeName, ncost);
    nodes.put(nodeName, n);

    output.println("created node " + nodeName + " with cost " + cost);

  }

  private void addNode(List arguments) {
    if (arguments.size() != 2) {
      throw new CommandException("Bad arguments to addNode: " + arguments);
    }

    String graphName = (String) arguments.get(0);
    String nodeName = (String) arguments.get(1);

    addNode(graphName, nodeName);
  }

  private void addNode(String graphName, String nodeName) {
    Object node = nodes.get(nodeName);
    Graph graph = (Graph) graphs.get(graphName);

    Assert.assertNotNull(node);
    Assert.assertNotNull(graph);

    graph.addNode(node);

    output.println("added node " + nodeName + " to " +
		   graphName);
  }

  private void addEdge(List arguments) {
    if (arguments.size() != 3) {
      throw new CommandException("Bad arguments to addEdge: " + arguments);
    }

    String graphName = (String) arguments.get(0);
    String parentName = (String) arguments.get(1);
    String childName = (String) arguments.get(2);

    addEdge(graphName, parentName, childName);
  }

  private void addEdge(String graphName, String parentName, String childName) {
    Graph graph = (Graph) graphs.get(graphName);
    Object parent = nodes.get(parentName);
    Object child = nodes.get(childName);

    Assert.assertNotNull(graph);
    Assert.assertNotNull(parent);
    Assert.assertNotNull(child);

    graph.addEdge(parent, child);

    output.println("added edge from " + parentName + " to " + childName + " in " + graphName);
  }


  private void listNodes(List arguments) {
    if (arguments.size() != 1) {
      throw new CommandException("Bad arguments to listNodes: " + arguments);
    }

    String graphName = (String) arguments.get(0);
    listNodes(graphName);
  }

  private void listNodes(String graphName) {
    Graph g = (Graph) graphs.get(graphName);
    Assert.assertNotNull(g);

    ArrayList nodes = new ArrayList(g.nodeSet());
    Collections.sort(nodes);

    // print the required output
    output.print(graphName + " contains:");
    for (Iterator i = nodes.iterator(); i.hasNext() ;) {
      output.print(" " + ((WeightedNode) i.next()).name);
    }
    output.print("\n");
  }

  private void listChildren(List arguments) {
    if (arguments.size() != 2) {
      throw new CommandException("Bad arguments to listChildren: " + arguments);
    }

    String graphName = (String) arguments.get(0);
    String parentName = (String) arguments.get(1);
    listChildren(graphName, parentName);
  }

  private void listChildren(String graphName, String parentName) {
    Graph g = (Graph) graphs.get(graphName);
    Object parent = nodes.get(parentName);
    Assert.assertNotNull(g);
    Assert.assertNotNull(parent);

    ArrayList children = new ArrayList(g.childrenOf(parent));
    Collections.sort(children);

    // print the required output
    output.print("the children of " + parentName + " in " + graphName + " are:");
    for (Iterator i = children.iterator(); i.hasNext() ;) {
      output.print(" " + ((WeightedNode) i.next()).name);
    }
    output.print("\n");
  }

  private void findPath(List arguments) {
    String graphName;
    List sourceArgs = new ArrayList();
    List destArgs = new ArrayList();

    if (arguments.size() < 1) {
      throw new CommandException("Bad arguments to FindPath: " + arguments);
    }

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

    if (sourceArgs.size() < 1) {
      throw new CommandException("Too few source args for FindPath");
    }
    if (destArgs.size() < 1) {
      throw new CommandException("Too few dest args for FindPath");
    }

    findPath(graphName, sourceArgs, destArgs);
  }

  private void findPath(String graphName, List sourceArgs, List destArgs) {
    // get the objects
    Graph g = (Graph) graphs.get(graphName);
    Assert.assertNotNull(g);

    Set starts = new HashSet();
    for (int i=0; i<sourceArgs.size(); i++) {
      String nodeName = (String) sourceArgs.get(i);
      WeightedNode n = (WeightedNode) nodes.get(nodeName);
      Assert.assertNotNull(n);
      starts.add(new WeightedNodePath(n));
    }

    Set goals = new HashSet();
    for (int i=0; i<destArgs.size(); i++) {
      String nodeName = (String) destArgs.get(i);
      WeightedNode n = (WeightedNode) nodes.get(nodeName);
      Assert.assertNotNull(n);
      goals.add(n);
    }

    // output the result
    try {

      Path path = PathFinder.findPath(g, starts, goals);
      output.print("shortest path in " + graphName + ":");
      Iterator steps = path.elements();
      while (steps.hasNext()) {
	String theName =  ((WeightedNode)steps.next()).name;
	output.print(" " + theName);
      }
      output.print("\n");

    } catch (PathFinder.NoPathException e) {
      output.println("No Path in " + graphName);
    }
  }
}

/**
 * This exception results when the input file cannot be parsed properly
 */
class CommandException extends RuntimeException {

  public CommandException() {
    super();
  }
  public CommandException(String s) {
    super(s);
  }
}
