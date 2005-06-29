package daikon.split;

import java.io.*;
import java.util.*;
import daikon.split.misc.*;
import utilMDE.*;
import java.util.logging.Logger;

/**
 * This factory creates Splitters from map files.  The splitters
 * partition the data based upon the the caller (i.e., which static
 * callgraph edge was taken).
 **/
public class ContextSplitterFactory
{
  /** Debug tracer. **/
  public static final Logger debug = Logger.getLogger("daikon.split.ContextSplitterFactory");

  /** Callsite granularity at the line level. */
  public static final int GRAIN_LINE = 0;
  /** Callsite granularity at the method level. */
  public static final int GRAIN_METHOD = 1;
  /** Callsite granularity at the class level. */
  public static final int GRAIN_CLASS = 2;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Enumeration (integer).  Specifies the granularity to use for
   * callsite splitter processing.  0 is line-level granularity; 1 is
   * method-level granularity; 2 is class-level granularity.
   **/
  public static int dkconfig_granularity = GRAIN_METHOD;

  /**
   * @param files set of File objects to read from
   * @param grain one ofthe GRAIN constants defined in this class
   *
   * Read all the map files in the given collection, create callsite
   * splitters from them, and put the splitters into SplitterList.
   **/
  public static void load_mapfiles_into_splitterlist(Collection files, // [File]
                                                     int grain
                                                     ) {
    for (Iterator i = files.iterator(); i.hasNext(); ) {
      File file = (File)i.next();
      String filename = file.getName();

      System.out.print(".");  // show progress
      debug.fine ("Reading mapfile " + filename);

      PptNameAndSplitters[] splitters;
      try {
        MapfileEntry[] entries = parse_mapfile(file);
        splitters = make_context_splitters(entries, grain);
      } catch (IOException e) {
        throw new Error(e.toString());
      }

      for (int j=0; j < splitters.length; j++) {
        PptNameAndSplitters nas = splitters[j];
        SplitterList.put(nas.ppt_name, nas.splitters);
      }
    }
  }

  /**
   * Simple record type to store a map file entry.
   **/
  public static final class MapfileEntry
  {
    public final long id;
    public final String fromclass;
    public final String frommeth;
    public final String fromfile;
    public final long fromline;
    public final long fromcol;
    public final String toexpr;
    public final String toargs;
    public final String toclass;
    public final String tometh;

    public MapfileEntry(long id,
                        String fromclass,
                        String frommeth,
                        String fromfile,
                        long fromline,
                        long fromcol,
                        String toexpr,
                        String toargs,
                        String toclass,
                        String tometh)
    {
      this.id = id;
      this.fromclass = fromclass;
      this.frommeth = frommeth;
      this.fromfile = fromfile;
      this.fromline = fromline;
      this.fromcol = fromcol;
      this.toexpr = toexpr;
      this.toargs = toargs;
      this.toclass = toclass;
      this.tometh = tometh;
    }
  }

  /**
   * Read and parse a map file.
   **/
  public static MapfileEntry[] parse_mapfile(File mapfile)
    throws IOException
  {
    ArrayList result = new ArrayList();

    try {
      LineNumberReader reader = UtilMDE.lineNumberFileReader(mapfile.toString());
      String reader_line;
      while ((reader_line = reader.readLine()) != null) {
        String line = reader_line;
        // Remove comments, skip blank lines
        {
          int hash = line.indexOf('#');
          if (hash >= 0) {
            line = line.substring(0, hash);
          }
          line = line.trim();
          if (line.length() == 0) {
            continue;
          }
        }

        // Example line:
        //   0x85c2e8c PC.RPStack get [PC/RPStack.java:156:29] -> "getCons" [(I)LPC/Cons;] PC.RP meth
        // where this ^ is a tab and the rest are single spaces
        long id;
        String fromclass, frommeth, fromfile; long fromline, fromcol;
        String toexpr, toargs, toclass, tometh;

        int tab = line.indexOf('\t');
        int arrow = line.indexOf(" -> ");
        Assert.assertTrue(tab >= 0);
        Assert.assertTrue(arrow >= tab);

        id = Long.decode(line.substring(0, tab)).longValue();

        // parse "called from" data
        {
          StringTokenizer tok = new StringTokenizer(line.substring(tab+1,arrow));
          fromclass = tok.nextToken();
          frommeth = tok.nextToken();
          String temp = tok.nextToken();
          Assert.assertTrue(temp.startsWith("["));
          Assert.assertTrue(temp.endsWith("]"));
          temp = temp.substring(1, temp.length()-1);
          int one = temp.indexOf(':');
          int two = temp.lastIndexOf(':');
          fromfile = temp.substring(0, one);
          fromline = Integer.parseInt(temp.substring(one+1,two));
          fromcol = Integer.parseInt(temp.substring(two+1));
          Assert.assertTrue(! tok.hasMoreTokens());
        }

        // parse "call into" data
        {
          String to = line.substring(arrow + 4); // 4: " -> "
          Assert.assertTrue(to.startsWith("\""), to);
          int endquote = to.indexOf("\" ", 1);
          toexpr = line.substring(1, endquote);
          StringTokenizer tok = new StringTokenizer(to.substring(endquote+1));
          toargs = tok.nextToken();
          toclass = tok.nextToken();
          tometh = tok.nextToken();
          Assert.assertTrue(! tok.hasMoreTokens());
        }

        // As we add better error messages, use this:
        // "Malformed line " + reader.getLineNumber()
        // + " (\"" + reader_line + "\")"
        // + " in " + mapfile;

        MapfileEntry entry = new MapfileEntry
          (id, fromclass, frommeth, fromfile, fromline, fromcol,
           toexpr, toargs, toclass, tometh);

        result.add(entry);
      }
    } catch (NumberFormatException e) {
      throw new IOException("Malformed number: " + e);
    }

    return (MapfileEntry[]) result.toArray(new MapfileEntry[result.size()]);
  }

  /**
   * @param grain one of the GRAIN constants defined in this class
   *
   * Given map file data, create splitters given the requested
   * granularity.
   **/
  public static PptNameAndSplitters[] make_context_splitters(MapfileEntry[] entries,
                                                             int grain) {
    // Use a 2-deep map structure.  First key is an identifier
    // (~pptname) for the callee.  Second key is an idenfier for the
    // caller (based on granularity).  The value is a set of Integers
    // giving the ids that are associated with that callgraph edge.
    Map callee2caller2ids = new HashMap();

    // For each entry
    for (int i=0; i < entries.length; i++) {
      MapfileEntry entry = entries[i];
      String callee_ppt_name = entry.toclass + "." + entry.tometh;

      // Compute the caller based on granularity
      String caller_condition;
      switch (grain) {
      case GRAIN_LINE:
        caller_condition = "<Called from "
          + entry.fromclass + "." + entry.frommeth
          + ":" + entry.fromline + ":" + entry.fromcol + ">";
        break;
      case GRAIN_METHOD:
        caller_condition = "<Called from "
          + entry.fromclass + "." + entry.frommeth + ">";
        break;
      case GRAIN_CLASS:
        caller_condition = "<Called from "
          + entry.fromclass + ">";
        break;
      default:
        throw new UnsupportedOperationException("Unknown grain " + grain);
      }

      // Place the ID into the mapping
      Map caller2ids = (Map) callee2caller2ids.get(callee_ppt_name);
      if (caller2ids == null) {
        caller2ids = new HashMap();
        callee2caller2ids.put(callee_ppt_name, caller2ids);
      }
      Set ids = (Set) caller2ids.get(caller_condition);
      if (ids == null) {
        ids = new TreeSet();
        caller2ids.put(caller_condition, ids);
      }
      ids.add(new Long(entry.id));
    } // for all entries

    ArrayList result = new ArrayList(); // [PptNameAndSplitters]

    // For each callee
    for (Iterator i = callee2caller2ids.entrySet().iterator(); i.hasNext(); ) {
      Map.Entry ipair = (Map.Entry) i.next();
      String callee_ppt_name = (String) ipair.getKey();
      Map caller2ids = (Map) ipair.getValue();

      // 'splitters' collects all splitters for one callee_ppt_name
      Collection splitters = new ArrayList(); // [Splitter]

      // For each caller of that callee
      for (Iterator j = caller2ids.entrySet().iterator(); j.hasNext(); ) {
        Map.Entry jpair = (Map.Entry) j.next();
        String caller_condition = (String) jpair.getKey();
        List ids = new ArrayList((Set) jpair.getValue());

        // Make a splitter
        long[] ids_array = new long[ids.size()];
        for (int k=0; k < ids_array.length; k++) {
          ids_array[k] = ((Long) ids.get(k)).longValue();
        }

        debug.fine ("Creating splitter for " + callee_ppt_name
                    + " with ids " +  ids
                    + " named " + caller_condition);

        Splitter splitter = new CallerContextSplitter(ids_array, caller_condition);
        splitters.add(splitter);
      }

      // Collect all splitters for one callee_ppt_name
      Splitter[] splitters_array = (Splitter[])
        splitters.toArray(new Splitter[splitters.size()]);
      result.add(new PptNameAndSplitters(callee_ppt_name, splitters_array));
    }

    return (PptNameAndSplitters[])
      result.toArray(new PptNameAndSplitters[result.size()]);
  }

  /**
   * Simple record type to store a PptName and Splitter array.
   **/
  public static final class PptNameAndSplitters
  {
    public final String ppt_name; // really more like a regexp
    public final Splitter[] splitters;

    public PptNameAndSplitters(String ppt_name, Splitter[] splitters) {
      this.ppt_name = ppt_name;
      this.splitters = splitters;
    }

    public String toString() {
      return "PptNameAndSplitters<" + ppt_name + ","
        + Arrays.asList(splitters).toString() + ">";
    }

  }

}
