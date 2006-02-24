package daikon.tools;

import java.util.*;
import java.util.regex.*;
import java.io.*;

import daikon.chicory.DTraceReader;
import daikon.chicory.DeclReader;
import static daikon.chicory.DeclReader.*;
import utilMDE.*;

/**
 * Reads multiple dtrace files from web services and looks for fields
 * that match
 */
public class WSMatch {

  /**
   * Used to compare to doubles.  The parameter is how close (ratio) the two
   * doubles must be in order to be considered equal.  Larger ratioes will
   * match more which may have unintended effects on matching columns (since
   * multiple matches in a column result in no match)
   */
  static FuzzyFloat fuzzy = new FuzzyFloat (0.01);

  @Option ("print progress information")
  public static boolean verbose = false;

  @Option ("consider only variables that match the regular expression")
  public static Pattern var_match = null;

  @Option ("minimum rate for a substitution match")
  public static double min_substitution_match = 0.60;

  @Option ("minimum rate for a substitution cross check")
  public static double min_substitution_cross_check = 0.90;

  @Option ("minimum rate for a composable match")
  public static double min_composable_match = 0.60;

  public static class MatchInfo implements Comparable<MatchInfo> {
    DeclPpt ppt1;
    VarInfo var1;
    DeclPpt ppt2;
    VarInfo var2;
    double perc_match;
    List<RowMatch> matching_rows = new ArrayList<RowMatch>();

    public MatchInfo (DeclPpt ppt1, VarInfo var1, DeclPpt ppt2, VarInfo var2,
                      double perc_match) {

      this.ppt1 = ppt1;
      this.var1 = var1;
      this.ppt2 = ppt2;
      this.var2 = var2;
      this.perc_match = perc_match;
    }

    /** Sort based on percentage of matches **/
    public int compareTo (MatchInfo m1) {
      if (this.perc_match == m1.perc_match)
        return 0;
      else if (this.perc_match < m1.perc_match)
        return -1;
      else
        return 1;
    }

    public String toString() {
      return String.format ("%5.2f  %s.%s  %s.%s", perc_match, ppt1.name,
                            var1.name, ppt2.name, var2.name);
    }
  }

  /** Tracks the sample numbers in two matching samples **/
  public static class RowMatch {
    int index1;
    int index2;
    RowMatch (int index1, int index2) {
      this.index1 = index1; this.index2 = index2;
    }
    public String toString () {
      return (index1 + "-" + index2);
    }
  }

  public static void main (String[] args) {

    Options options = new Options ("WSMatch [options] dtrace-files...",
                                   WSMatch.class);
    String[] files = options.parse_and_usage (args);

    // Read in all of the files
    List<DTraceReader> traces = new ArrayList<DTraceReader>();
    for (String file : files) {
      if (verbose)
        System.out.printf ("Processing file %s%n", file);
      DTraceReader trace = new DTraceReader();
      trace.read (new File (file));
      traces.add (trace);
    }

    List<MatchInfo> substitute_matches = new ArrayList<MatchInfo>();
    List<MatchInfo> compose_matches = new ArrayList<MatchInfo>();

    // Look for any matches between each pair of columns
    for (int ii = 0; ii < traces.size(); ii++) {
      for (int jj = ii+1; jj < traces.size(); jj++) {
        List<MatchInfo> results
          = compare_services (traces.get(ii), traces.get(jj));
        if (results.size() > 0) {
          List<MatchInfo> substitute_results = new ArrayList<MatchInfo>();
          List<MatchInfo> compose_results = new ArrayList<MatchInfo>();
          split_results (results, substitute_results, compose_results);
          if (substitute_results.size() > 0)
            substitute_matches.add (Collections.max (substitute_results));
          if (compose_results.size() > 0)
            compose_matches.add (Collections.max (compose_results));
        }
      }
    }

    // For each substitution match, look for corresponding matches in the
    // other fields
    for (MatchInfo primary_match : substitute_matches) {
      if (primary_match.perc_match < min_substitution_match)
        continue;
      List<MatchInfo> subs = find_substitutes (primary_match);
      System.out.printf ("%nSubstitution matches for primary %s%n",
                         primary_match);
      if (verbose)
        System.out.printf ("  matching rows = %s%n",
                           primary_match.matching_rows);
      for (MatchInfo sub : subs) {
        if (sub.perc_match < min_substitution_cross_check)
          continue;
        System.out.printf ("  %s%n", sub);
      }
    }

    System.out.printf ("%nComposition Matches:%n");
    for (MatchInfo m : compose_matches) {
      if (m.perc_match < min_composable_match)
        continue;
      System.out.printf ("%5.2f %s.%s  %s.%s%n", m.perc_match,
                         m.ppt1, m.var1.name, m.ppt2, m.var2.name);
      if (verbose)
        System.out.printf ("  matching rows = %s%n", m.matching_rows);
    }
  }

  /**
   * Compares each field from two services and returns how often they
   * match up
   */
  public static List<MatchInfo> compare_services (DTraceReader trace1,
                                                  DTraceReader trace2) {

    List<MatchInfo> results = new ArrayList<MatchInfo>();

    DeclPpt ppt1 = trace1.get_all_ppts().get(0);
    DeclPpt ppt2 = trace2.get_all_ppts().get(0);

    for (VarInfo var1 : ppt1.get_all_vars()) {
      if (!include_var (var1))
        continue;
      for (VarInfo var2 : ppt2.get_all_vars()) {
        if (!include_var (var2))
          continue;
        results.add (compare_var (ppt1, var1,  ppt2, var2));
      }
    }
    return (results);
  }

  /**
   * Compares all of the values on the specified variables.  Returns the
   * percentage of rows that match exactly once
   */
  public static MatchInfo compare_var (DeclPpt ppt1, VarInfo var1,
                                       DeclPpt ppt2, VarInfo var2) {

    //System.out.printf ("%s index = %d, %s index = %d\n", var1, var1.index,
    //                   var2, var2.index);

    List<List<Object>> data1 = ppt1.get_var_data();
    List<List<Object>> data2 = ppt2.get_var_data();

    MatchInfo m = new MatchInfo (ppt1, var1, ppt2, var2, 0.0);

    int possible_matches = Math.min (data1.size(), data2.size());
    int match_cnt = 0;
    for (int ii = 0; ii < data1.size(); ii++) {
      List<Object> var_data1 = data1.get(ii);
      int mcnt1 = 0;
      int row = -1;
      for (int jj = 0; jj < data2.size(); jj++) {
        List<Object> var_data2 = data2.get(jj);
        boolean match = compare_val (var1, var_data1.get(var1.index),
                                     var2, var_data2.get(var2.index));
        if (match) {
          mcnt1++;
          row = jj;
        }
      }
      if (mcnt1 == 1) {
        match_cnt++;
        m.matching_rows.add (new RowMatch (ii, row));
      } else if (mcnt1 > 1) {
        // System.out.printf ("var1 %s value %s matches %s %d times%n", var1,
        //                   var_data1.get(var1.index), var2, mcnt1);
      }
    }

    m.perc_match = ((double) match_cnt) / possible_matches;
    if ((m.perc_match > 0) && (verbose))
      System.out.printf ("%5.2f  %s.%s %s.%s%n", m.perc_match, ppt1.name,
                         var1.name, ppt2.name, var2.name);
    return (m);
  }

  /**
   * Returns whether or not the two values are at least approximately
   * the same.  Nonsensical values are always different.
   */
  public static boolean compare_val (VarInfo var1, Object data1,
                                     VarInfo var2, Object data2) {

    // System.out.printf ("Comparing %s = %s against %s = %s\n", var1, data1,
    //                   var2, data2);

    // Nonsensical values are never equal
    if ((data1 == null) || (data2 == null))
      return (false);

    if (var1.is_int() && var2.is_int()) {
      int i1 = (Integer) data1;
      int i2 = (Integer) data2;
      return (i1 == i2);
    } else if (var1.is_double() && var2.is_double()) {
      double d1 = (Double) data1;
      double d2 = (Double) data2;
      return fuzzy.eq (d1, d2);
    } else if (var1.is_string() && var2.is_string()) {
      return data1.equals (data2);
    } else { // non-matching types
      return (false);
    }
  }

  /**
   * Finds substitutes given a match.  Only compare input to input and
   * output to output.
   */
  public static List<MatchInfo> find_substitutes (MatchInfo match) {

    List<MatchInfo> results = new ArrayList<MatchInfo>();

    for (VarInfo var1 : match.ppt1.get_all_vars()) {
      if (!include_var (var1))
        continue;
      for (VarInfo var2 : match.ppt2.get_all_vars()) {
        if (!include_var (var2))
          continue;
        if (is_input(var1) != is_input(var2))
          continue;
        results.add (compare_var (match, match.ppt1, var1,  match.ppt2, var2));
      }
    }
    return (results);
  }

  /**
   * Compares the values using the matching rows specified in match.
   * Returns a MatchInfo with perc_match filled in accordingly.  Does not
   * fill in matching_rows.
   */
  public static MatchInfo compare_var (MatchInfo match, DeclPpt ppt1,
                                  VarInfo var1, DeclPpt ppt2, VarInfo var2) {

    List<List<Object>> data1 = ppt1.get_var_data();
    List<List<Object>> data2 = ppt2.get_var_data();

    MatchInfo result = new MatchInfo (ppt1, var1, ppt2, var2, 0.0);

    int match_cnt = 0;
    for (RowMatch row : match.matching_rows) {
      List<Object> var_data1 = data1.get(row.index1);
      List<Object> var_data2 = data2.get(row.index2);
      boolean val_match = compare_val (var1, var_data1.get(var1.index),
                                       var2, var_data2.get(var2.index));
      if (val_match) {
        match_cnt++;
      }
    }

    int possible_matches = match.matching_rows.size();
    result.perc_match = ((double) match_cnt) / possible_matches;
    return (result);
  }

  /**
   * Prints the results of comparing two services
   */
  public static void print_results (List<MatchInfo> results) {

    if (results.size() == 0)
      return;

    Collections.sort (results, Collections.reverseOrder());
    MatchInfo m = results.get(0);
    System.out.printf ("%5.2f %s.%s  %s.%s%n", m.perc_match,
                       m.ppt1, m.var1.name, m.ppt2, m.var2.name);
  }

  /**
   * Takes all of the matches and splits them into those between the
   * same type of parameters (in-in/out-out (substitute_results)) and
   * those between different types of parements (in-out/out-in
   * (compose_results)).
   */
  public static void split_results (List<MatchInfo> all_results,
        List<MatchInfo> substitute_results, List<MatchInfo> compose_results) {

    for (MatchInfo m : all_results) {
      if (is_input (m.var1) == is_input (m.var2))
        substitute_results.add (m);
      else
        compose_results.add (m);
    }
  }

  public static boolean is_input (VarInfo v) {
    return v.name.startsWith ("input");
  }

  /** Returns whether or not to consider this variable **/
  public static boolean include_var (VarInfo var) {

    if (var_match == null)
      return true;

    Matcher m = var_match.matcher (var.name);
    return (m.find());
  }
}
