package daikon.split;

import daikon.FileIO;
import daikon.Global;
import java.util.Arrays;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.StringJoiner;
import java.util.logging.Level;
import jtb.ParseException;
import org.checkerframework.checker.nullness.qual.Nullable;

// SplitterList maps from a program point name to an array of Splitter
// objects that should be used when splitting that program point.
// Invariant:  each of those splitters should be non-instantiated (each is
// a factory, not an instantiated splitter).
// It's a shame to have to hard-code for each program point name.

public abstract class SplitterList {
  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  // "@ref{}" produces a cross-reference in the printed manual.  It must
  // *not* come at the beginning of a line, or Javadoc will get confused.
  /**
   * Boolean. Enables indiscriminate splitting (see Daikon manual, @ref{Indiscriminate splitting},
   * for an explanation of this technique).
   */
  public static boolean dkconfig_all_splitters = true;

  private static final HashMap<String, Splitter[]> ppt_splitters = new LinkedHashMap<>();

  /**
   * Maps a splitter to its condition, with the REPLACE statements of its {@code .spinfo} file
   * applied. {@link #get} and {@link #get_all} use this to determine whether two splitters are
   * duplicates. For a splitter that is not a key, the expanded condition is its condition.
   */
  private static final IdentityHashMap<Splitter, String> expanded_conditions =
      new IdentityHashMap<>();

  /**
   * Removes the splitters associated with the given name, which is a name on a PPT_NAME line of a
   * {@code .spinfo} file.
   *
   * @param pptname a name on a PPT_NAME line of a {@code .spinfo} file
   */
  public static void remove(String pptname) {
    Splitter[] splits = ppt_splitters.remove(pptname);
    if (splits != null) {
      for (Splitter splitter : splits) {
        expanded_conditions.remove(splitter);
      }
    }
  }

  /**
   * Associate an array of splitters with the program point pptname.
   *
   * @param pptname a name on a PPT_NAME line of a {@code .spinfo} file
   * @param splits the splitters
   * @param replacer the REPLACE statements of the {@code .spinfo} file that contains the splitters
   */
  static void put(String pptname, Splitter[] splits, StatementReplacer replacer) {
    for (Splitter splitter : splits) {
      String condition = splitter.condition().trim();
      try {
        expanded_conditions.put(splitter, replacer.makeReplacements(condition).trim());
      } catch (ParseException e) {
        // The splitter's Java source was created from the same expansion, so this does not happen.
        // If it does, identify the splitter by its unexpanded condition.
      }
    }
    put(pptname, splits);
  }

  /** Associate an array of splitters with the program point pptname. */
  public static void put(String pptname, Splitter[] splits) {
    // for (int i = 0; i<splits.length; i++) {
    //   assert splits[i].instantiated() == false;
    // }

    if ((Global.debugSplit != null) && Global.debugSplit.isLoggable(Level.FINE)) {
      String[] splits_strings = new String[splits.length];
      for (int i = 0; i < splits.length; i++) {
        splits_strings[i] = splits[i].condition();
      }
      Global.debugSplit.fine(
          "Registering splitters for " + pptname + ":" + Arrays.toString(splits_strings));
    }

    if (ppt_splitters.containsKey(pptname)) {
      Splitter[] old = ppt_splitters.get(pptname);
      Splitter[] new_splits = new Splitter[old.length + splits.length];
      System.arraycopy(old, 0, new_splits, 0, old.length);
      System.arraycopy(splits, 0, new_splits, old.length, splits.length);
      ppt_splitters.put(pptname, new_splits);
    } else {
      assert !ppt_splitters.containsKey(pptname);
      // assert ! ppt_splitters.containsKey(pptname)
      //               : "SplitterList already contains " + pptname
      //               + " which maps to" + lineSep + " " + Arrays.toString(get_raw(pptname))
      //               + lineSep + " which is " + formatSplitters(get_raw(pptname));
      ppt_splitters.put(pptname, splits);
    }
  }

  // This is only used by the debugging output in SplitterList.put().
  public static String formatSplitters(Splitter[] splits) {
    if (splits == null) {
      return "null";
    }
    StringJoiner sj = new StringJoiner(", ", "[", "]");
    for (Splitter split : splits) {
      sj.add("\"" + split.condition() + "\"");
    }
    return sj.toString();
  }

  public static Splitter @Nullable [] get_raw(String pptname) {
    return ppt_splitters.get(pptname);
  }

  //   // This returns a list of all the splitters that are applicable to the
  //   // program point named "name".  The list is constructed by looking up
  //   // various parts of "name" in the SplitterList hashtable.
  //
  //   // This routine tries the name first, then the base of the name, then the
  //   // class, then the empty string.  For instance, if the program point name is
  //   // "Foo.bar(IZ)V:::EXIT2", then it tries, in order:
  //   //   "Foo.bar(IZ)V:::EXIT2"
  //   //   "Foo.bar(IZ)V"
  //   //   "Foo.bar"
  //   //   "Foo"
  //   //   ""
  //
  //   public static Splitter[] get(String pptName) {
  //     String pptName_ = pptName;        // debugging
  //     Splitter[] result;
  //     ArrayList splitterArrays = new ArrayList();
  //     ArrayList splitters = new ArrayList();
  //
  //     result = get_raw(pptName);
  //     if (result != null)
  //       splitterArrays.addElement(result);
  //
  //     {
  //       int tag_index = pptName.indexOf(FileIO.ppt_tag_separator);
  //       if (tag_index != -1) {
  //         pptName = pptName.substring(0, tag_index);
  //         result = get_raw(pptName);
  //         if (result != null)
  //           splitterArrays.addElement(result);
  //       }
  //     }
  //
  //     int lparen_index = pptName.indexOf('(');
  //     {
  //       if (lparen_index != -1) {
  //         pptName = pptName.substring(0, lparen_index);
  //         result = get_raw(pptName);
  //         if (result != null)
  //           splitterArrays.addElement(result);
  //       }
  //     }
  //     {
  //       // The class pptName runs up to the last dot before any open parenthesis.
  //       int dot_limit = (lparen_index == -1) ? pptName.length() : lparen_index;
  //       int dot_index = pptName.lastIndexOf('.', dot_limit - 1);
  //       if (dot_index != -1) {
  //         pptName = pptName.substring(0, dot_index);
  //         result = get_raw(pptName);
  //         if (result != null)
  //           splitterArrays.addElement(result);
  //       }
  //     }
  //
  //     // Empty string means always applicable.
  //     result = get_raw("");
  //     if (result != null)
  //       splitterArrays.addElement(result);
  //
  //     if (splitterArrays.isEmpty()) {
  //         Global.debugSplit.fine("SplitterList.get found no splitters for " + pptName);
  //         return null;
  //     } else {
  //       int counter = 0;
  //       for (int i = 0; i < splitterArrays.size(); i++) {
  //         Splitter[] tempsplitters = (Splitter[])splitterArrays.get(i);
  //         for (int j = 0; j < tempsplitters.length; j++) {
  //           splitters.addElement(tempsplitters[j]);
  //           counter++;
  //         }
  //       }
  //       Global.debugSplit.fine("SplitterList.get found " + counter + " splitters for " +
  //                              pptName);
  //     }
  //     return (Splitter[])splitters.toArray(new Splitter[0]);
  //   }
  // //////////////////////

  /**
   * Returns true if the name on a PPT_NAME line of a {@code .spinfo} file designates the given
   * program point.
   *
   * <p>A name that contains ":::", such as "pkg.Foo.bar(int):::EXIT1", is a complete program point
   * name. It designates only the program point of that name, except that a name ending with
   * ":::EXIT" also designates the method's numbered exit points, such as
   * "pkg.Foo.bar(int):::EXIT12".
   *
   * <p>Any other name, such as "Foo.bar", designates every program point whose name contains it.
   *
   * @param spinfoPptName a name on a PPT_NAME line of a {@code .spinfo} file
   * @param pptName the name of a program point
   * @return true if {@code spinfoPptName} designates the program point named {@code pptName}
   */
  public static boolean matches(String spinfoPptName, String pptName) {
    if (!isComplete(spinfoPptName)) {
      return pptName.contains(spinfoPptName);
    }
    if (pptName.equals(spinfoPptName)) {
      return true;
    }
    if (spinfoPptName.endsWith(FileIO.exit_tag) && pptName.startsWith(spinfoPptName)) {
      String exitNumber = pptName.substring(spinfoPptName.length());
      return !exitNumber.isEmpty() && exitNumber.chars().allMatch(c -> c >= '0' && c <= '9');
    }
    return false;
  }

  /**
   * Returns true if the name on a PPT_NAME line of a {@code .spinfo} file is a complete program
   * point name, which designates only specific program points; see {@link #matches}.
   *
   * @param spinfoPptName a name on a PPT_NAME line of a {@code .spinfo} file
   * @return true if {@code spinfoPptName} is a complete program point name
   */
  static boolean isComplete(String spinfoPptName) {
    return spinfoPptName.contains(FileIO.ppt_tag_separator);
  }

  /**
   * Returns the splitters associated with this program point name (or null). The resulting
   * splitters are factories, not instantiated splitters. The result contains no two duplicate
   * splitters (see {@link #addUnlessDuplicate}), even if several PPT_NAME lines match the program
   * point.
   *
   * <p>An OBJECT program point also uses every splitter whose PPT_NAME is not a complete program
   * point name, if any such PPT_NAME contains "OBJECT".
   *
   * @param pptName the name of a program point
   * @return an array of splitters
   */
  public static Splitter @Nullable [] get(String pptName) {
    boolean useAllIncomplete = false;
    if (pptName.contains("OBJECT")) {
      for (String name : ppt_splitters.keySet()) {
        if (!isComplete(name) && name.contains("OBJECT")) {
          useAllIncomplete = true;
          break;
        }
      }
    }

    // Maps a duplicate key to its splitter.  A LinkedHashMap, for deterministic output.
    Map<String, Splitter> splitters = new LinkedHashMap<>();
    for (Map.Entry<String, Splitter[]> entry : ppt_splitters.entrySet()) {
      String name = entry.getKey();
      if (matches(name, pptName) || (useAllIncomplete && !isComplete(name))) {
        addUnlessDuplicate(splitters, entry.getValue());
      }
    }

    if (splitters.isEmpty()) {
      Global.debugSplit.fine("SplitterList.get found no splitters for " + pptName);
      return null;
    } else {
      Global.debugSplit.fine(
          "SplitterList.get found " + splitters.size() + " splitters for " + pptName);
      return splitters.values().toArray(new Splitter[0]);
    }
  }

  /**
   * Adds each splitter to the map, unless the map already contains a duplicate of it. Two splitters
   * are duplicates if their conditions are the same, both as written and with the REPLACE
   * statements of their {@code .spinfo} files applied.
   *
   * <p>Two splitters whose conditions differ as written are not duplicates, even if the conditions
   * are the same after REPLACE statements are applied, such as "isEmpty()" and "size == 0". A
   * splitter's variables are those of the program point for which it was created, so two such
   * splitters may test different values.
   *
   * @param splitters maps a key returned by {@link #duplicateKey} to its splitter; side-effected by
   *     this method
   * @param toAdd the splitters to add
   */
  private static void addUnlessDuplicate(Map<String, Splitter> splitters, Splitter[] toAdd) {
    for (Splitter splitter : toAdd) {
      splitters.putIfAbsent(duplicateKey(splitter), splitter);
    }
  }

  /**
   * Returns a string that is equal for two splitters if and only if they are duplicates; see {@link
   * #addUnlessDuplicate}.
   *
   * @param splitter a splitter
   * @return a key that identifies the splitter's duplicates
   */
  private static String duplicateKey(Splitter splitter) {
    String condition = splitter.condition().trim();
    String expanded = expanded_conditions.get(splitter);
    if (expanded == null) {
      expanded = condition;
    }
    // A newline cannot appear in a condition, which is one line of a .spinfo file.
    return condition + "\n" + expanded;
  }

  /**
   * Returns all the splitters in this program. The resulting splitters are factories, not
   * instantiated splitters. The result contains no two duplicate splitters (see {@link
   * #addUnlessDuplicate}).
   *
   * @return an array of splitters
   */
  public static Splitter[] get_all() {
    // Maps a duplicate key to its splitter.  A LinkedHashMap, for deterministic output.
    Map<String, Splitter> splitters = new LinkedHashMap<>();
    for (Splitter[] splitter_array : ppt_splitters.values()) {
      addUnlessDuplicate(splitters, splitter_array);
    }
    return splitters.values().toArray(new Splitter[0]);
  }
}
