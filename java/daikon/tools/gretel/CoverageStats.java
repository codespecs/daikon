package daikon.tools.gretel;

import java.lang.reflect.Method;
import java.text.DecimalFormat;
import java.util.*;

// see /g4/projects/invariants/tools/Gretel/current/lib/residue

/**
 * Given a .gretel file and a list of source file names, produce a
 * report of the line coverage on those files.
 **/
public class CoverageStats
{

  public static void main(String[] args)
    throws Exception
  {
    String gretelFile = args[0];
    List relevant = Arrays.asList(args).subList(1, args.length);

    // We use reflection for Gretel calls so that people don't have to
    // download Gretel to build Daikon.
    Class clazzListHits = Class.forName("residue.ListHits");

    // setConfigFile(gretelFile);
    {
      Method m = clazzListHits.getMethod("setConfigFile", new Class[] { String.class });
      m.invoke(null, new Object[] { gretelFile });
    }

    Map hits, misses; // [StringFilename -> Set[Integer]]
    // hits = getHits(); misses = getMisses();
    {
      Method m;
      m = clazzListHits.getMethod("getHits", null);
      hits = (Map) m.invoke(null, null);
      m = clazzListHits.getMethod("getMisses", null);
      misses = (Map) m.invoke(null, null);
    }

    // Compute coverage
    Map all = new HashMap();
    Map covered = new HashMap();
    for (Iterator i = hits.keySet().iterator(); i.hasNext(); ) {
      String file = (String) i.next();
      Set lines = (Set) hits.get(file);
      if (! relevant.contains(file)) continue;
      if (! all.containsKey(file)) all.put(file, new HashSet());
      ((Set) all.get(file)).addAll(lines);
      if (! covered.containsKey(file)) covered.put(file, new HashSet());
      ((Set) covered.get(file)).addAll(lines);
    }
    for (Iterator i = misses.keySet().iterator(); i.hasNext(); ) {
      String file = (String) i.next();
      Set lines = (Set) misses.get(file);
      if (! relevant.contains(file)) continue;
      if (! all.containsKey(file)) all.put(file, new HashSet());
      ((Set) all.get(file)).addAll(lines);
      // We choose to count partially-covered lines as fully covered,
      // instead of uncovered.  Uncomment below code to not count them.
      // if (! covered.containsKey(file)) covered.put(file, new HashSet(0));
      // ((Set) covered.get(file)).removeAll(lines);
    }

    // Display results
    int nall_sum = 0;
    int ncov_sum = 0;
    for (int i = 0; i < relevant.size(); i++) {
      String file = (String) relevant.get(i);
      if (all.get(file) == null) {
	System.out.println(file + " not instrumented (?)");
	continue;
      }
      int nall = ((Set) all.get(file)).size();
      int ncov = (covered.get(file) == null) ? 0 : ((Set) covered.get(file)).size();
      System.out.println(file + " covered on " + ncov + " of " + nall + " lines");
      nall_sum += nall;
      ncov_sum += ncov;
    }
    double pct = ((double) ncov_sum) / nall_sum;
    System.out.println("Total coverage "
		       + ncov_sum + " of " + nall_sum + " lines ==> "
		       + (new DecimalFormat("0.00").format(pct)) + " %");
  }

}
