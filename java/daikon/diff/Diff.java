package daikon.diff;

import static java.util.logging.Level.FINE;
import static java.util.logging.Level.INFO;

import daikon.Daikon;
import daikon.FileIO;
import daikon.Ppt;
import daikon.PptConditional;
import daikon.PptMap;
import daikon.PptTopLevel;
import daikon.inv.Invariant;
import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OptionalDataException;
import java.io.StreamCorruptedException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.NavigableSet;
import java.util.TreeSet;
import java.util.logging.Logger;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.nullness.qual.EnsuresNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.plumelib.util.CollectionsPlume;
import org.plumelib.util.FilesPlume;
import org.plumelib.util.MPair;
import org.plumelib.util.OrderedPairIterator;
import org.plumelib.util.StringsPlume;

/**
 * Diff is the main class for the invariant diff program. The invariant diff program outputs the
 * differences between two sets of invariants.
 *
 * <p>The following is a high-level description of the program. Each input file contains a
 * serialized PptMap or InvMap. PptMap and InvMap are similar structures, in that they both map
 * program points to invariants. However, PptMaps are much more complicated than InvMaps. PptMaps
 * are output by Daikon, and InvMaps are output by this program.
 *
 * <p>First, if either input is a PptMap, it is converted to an InvMap. Next, the two InvMaps are
 * combined to form a tree. The tree is exactly three levels deep. The first level contains the
 * root, which holds no data. Each node in the second level is a pair of Ppts, and each node in the
 * third level is a pair of Invariants. The tree is constructed by pairing the corresponding Ppts
 * and Invariants in the two PptMaps. Finally, the tree is traversed via the Visitor pattern to
 * produce output. The Visitor pattern makes it easy to extend the program, simply by writing a new
 * Visitor.
 */
public final class Diff {

  /** Debug logger. */
  public static final Logger debug = Logger.getLogger("daikon.diff.Diff");

  /** The usage message for this program. */
  private static String usage =
      StringsPlume.joinLines(
          "Usage:",
          "    java daikon.diff.Diff [flags...] file1 [file2]",
          "  file1 and file2 are serialized invariants produced by Daikon.",
          "  If file2 is not specified, file1 is compared with the empty set.",
          "  For a list of flags, see the Daikon manual, which appears in the ",
          "  Daikon distribution and also at http://plse.cs.washington.edu/daikon/.");

  /** The long command line options. */
  private static final String HELP_SWITCH = "help";

  private static final String INV_SORT_COMPARATOR1_SWITCH = "invSortComparator1";
  private static final String INV_SORT_COMPARATOR2_SWITCH = "invSortComparator2";
  private static final String INV_PAIR_COMPARATOR_SWITCH = "invPairComparator";
  private static final String IGNORE_UNJUSTIFIED_SWITCH = "ignore_unjustified";
  private static final String IGNORE_NUMBERED_EXITS_SWITCH = "ignore_exitNN";

  /** Determine which ppts should be paired together in the tree. */
  private static final Comparator<PptTopLevel> PPT_COMPARATOR = new Ppt.NameComparator();

  /**
   * Comparators to sort the sets of invs, and to combine the two sets into the pair tree. Can be
   * overriden by command-line options.
   */
  private Comparator<Invariant> invSortComparator1;

  private Comparator<Invariant> invSortComparator2;
  private Comparator<Invariant> invPairComparator;

  private boolean examineAllPpts;
  private boolean ignoreNumberedExits;

  public Diff() {
    this(false, false);
  }

  public Diff(boolean examineAllPpts) {
    this(examineAllPpts, false);
  }

  public Diff(boolean examineAllPpts, Comparator<Invariant> c) {
    this(examineAllPpts, false);
    setAllInvComparators(c);
  }

  public Diff(boolean examineAllPpts, boolean ignoreNumberedExits) {
    this.examineAllPpts = examineAllPpts;
    this.ignoreNumberedExits = ignoreNumberedExits;
    setAllInvComparators(new Invariant.ClassVarnameComparator());
  }

  public Diff(
      boolean examineAllPpts,
      boolean ignoreNumberedExits,
      @Nullable @ClassGetName String invSortComparator1Classname,
      @Nullable @ClassGetName String invSortComparator2Classname,
      @Nullable @ClassGetName String invPairComparatorClassname,
      Comparator<Invariant> defaultComparator)
      throws ClassNotFoundException,
          IllegalAccessException,
          InstantiationException,
          InvocationTargetException,
          NoSuchMethodException {
    this.examineAllPpts = examineAllPpts;
    this.ignoreNumberedExits = ignoreNumberedExits;
    this.invSortComparator1 = selectComparator(invSortComparator1Classname, defaultComparator);
    this.invSortComparator2 = selectComparator(invSortComparator2Classname, defaultComparator);
    this.invPairComparator = selectComparator(invPairComparatorClassname, defaultComparator);
  }

  /**
   * Read two PptMap or InvMap objects from their respective files. Convert the PptMaps to InvMaps
   * as necessary, and diff the InvMaps.
   */
  public static void main(String[] args)
      throws FileNotFoundException,
          StreamCorruptedException,
          OptionalDataException,
          IOException,
          ClassNotFoundException,
          IllegalAccessException,
          InstantiationException,
          InvocationTargetException,
          NoSuchMethodException {
    try {
      mainHelper(args);
    } catch (Daikon.DaikonTerminationException e) {
      daikon.Daikon.handleDaikonTerminationException(e);
    }
  }

  /**
   * This does the work of {@link #main(String[])}, but it never calls System.exit, so it is
   * appropriate to be called progrmmatically.
   */
  public static void mainHelper(final String[] args)
      throws FileNotFoundException,
          StreamCorruptedException,
          OptionalDataException,
          IOException,
          ClassNotFoundException,
          InstantiationException,
          IllegalAccessException,
          InvocationTargetException,
          NoSuchMethodException {
    daikon.LogHelper.setupLogs(INFO);

    boolean printDiff = false;
    boolean printAll = false;
    boolean includeUnjustified = true;
    boolean stats = false;
    boolean tabSeparatedStats = false;
    boolean minus = false;
    boolean xor = false;
    boolean union = false;
    boolean examineAllPpts = false;
    boolean ignoreNumberedExits = false;
    boolean printEmptyPpts = false;
    boolean verbose = false;
    boolean continuousJustification = false;
    boolean logging = false;
    File outputFile = null;
    @ClassGetName String invSortComparator1Classname = null;
    @ClassGetName String invSortComparator2Classname = null;
    @ClassGetName String invPairComparatorClassname = null;

    boolean optionSelected = false;

    daikon.LogHelper.setupLogs(INFO);
    //     daikon.LogHelper.setLevel ("daikon.diff", FINE);

    LongOpt[] longOpts =
        new LongOpt[] {
          new LongOpt(HELP_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(INV_SORT_COMPARATOR1_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(INV_SORT_COMPARATOR2_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(INV_PAIR_COMPARATOR_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(IGNORE_UNJUSTIFIED_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(IGNORE_NUMBERED_EXITS_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
        };

    Getopt g =
        new Getopt(
            "daikon.diff.Diff", args,
            "Hhyduastmxno:jzpevl", longOpts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch (c) {
        case 0:
          // got a long option
          String optionName = longOpts[g.getLongind()].getName();
          if (Daikon.help_SWITCH.equals(optionName)) {
            System.out.println(usage);
            throw new Daikon.NormalTermination();
          } else if (INV_SORT_COMPARATOR1_SWITCH.equals(optionName)) {
            if (invSortComparator1Classname != null) {
              throw new Error(
                  "multiple --"
                      + INV_SORT_COMPARATOR1_SWITCH
                      + " classnames supplied on command line");
            }
            @SuppressWarnings("signature") // user input, should be checked
            @ClassGetName String cgn = Daikon.getOptarg(g);
            invSortComparator1Classname = cgn;
          } else if (INV_SORT_COMPARATOR2_SWITCH.equals(optionName)) {
            if (invSortComparator2Classname != null) {
              throw new Error(
                  "multiple --"
                      + INV_SORT_COMPARATOR2_SWITCH
                      + " classnames supplied on command line");
            }
            @SuppressWarnings("signature") // user input, should be checked
            @ClassGetName String cgn = Daikon.getOptarg(g);
            invSortComparator2Classname = cgn;
          } else if (INV_PAIR_COMPARATOR_SWITCH.equals(optionName)) {
            if (invPairComparatorClassname != null) {
              throw new Error(
                  "multiple --"
                      + INV_PAIR_COMPARATOR_SWITCH
                      + " classnames supplied on command line");
            }
            @SuppressWarnings("signature") // user input, should be checked
            @ClassGetName String cgn = Daikon.getOptarg(g);
            invPairComparatorClassname = cgn;
          } else if (IGNORE_UNJUSTIFIED_SWITCH.equals(optionName)) {
            optionSelected = true;
            includeUnjustified = false;
            break;
          } else if (IGNORE_NUMBERED_EXITS_SWITCH.equals(optionName)) {
            ignoreNumberedExits = true;
            break;
          } else {
            throw new RuntimeException("Unknown long option received: " + optionName);
          }
          break;
        case 'h':
          System.out.println(usage);
          throw new Daikon.NormalTermination();
        case 'H':
          PrintAllVisitor.HUMAN_OUTPUT = true;
          break;
        case 'y': // included for legacy code
          optionSelected = true;
          includeUnjustified = false;
          break;
        case 'd':
          optionSelected = true;
          printDiff = true;
          break;
        case 'a':
          optionSelected = true;
          printAll = true;
          break;
        case 's':
          optionSelected = true;
          stats = true;
          break;
        case 't':
          optionSelected = true;
          tabSeparatedStats = true;
          break;
        case 'm':
          optionSelected = true;
          minus = true;
          break;
        case 'x':
          optionSelected = true;
          xor = true;
          break;
        case 'n':
          optionSelected = true;
          union = true;
          break;
        case 'o':
          if (outputFile != null) {
            throw new Error("multiple output files supplied on command line");
          }
          String outputFilename = Daikon.getOptarg(g);
          outputFile = new File(outputFilename);
          if (!FilesPlume.canCreateAndWrite(outputFile)) {
            throw new Error("Cannot write to file " + outputFile);
          }
          break;
        case 'j':
          continuousJustification = true;
          break;
        case 'p':
          examineAllPpts = true;
          break;
        case 'e':
          printEmptyPpts = true;
          break;
        case 'v':
          verbose = true;
          break;
        case 'l':
          logging = true;
          break;
        case '?':
          // getopt() already printed an error
          System.out.println(usage);
          throw new Daikon.UserError("Bad argument");
        default:
          System.out.println("getopt() returned " + c);
          break;
      }
    }

    // Turn on the defaults
    if (!optionSelected) {
      printDiff = true;
    }

    if (logging) {
      System.err.println("Invariant Diff: Starting Log");
    }

    if (logging) {
      System.err.println("Invariant Diff: Creating Diff Object");
    }

    Comparator<Invariant> defaultComparator;
    if (minus || xor || union) {
      defaultComparator = new Invariant.ClassVarnameFormulaComparator();
    } else {
      defaultComparator = new Invariant.ClassVarnameComparator();
    }

    // Set the comparators based on the command-line options

    Diff diff =
        new Diff(
            examineAllPpts,
            ignoreNumberedExits,
            invSortComparator1Classname,
            invSortComparator2Classname,
            invPairComparatorClassname,
            defaultComparator);

    if (!diff.invSortComparator1
            .getClass()
            .toString()
            .equals(diff.invSortComparator2.getClass().toString())
        || !diff.invSortComparator1
            .getClass()
            .toString()
            .equals(diff.invPairComparator.getClass().toString())) {
      System.out.println("You are using different comparators to sort or pair up invariants.");
      System.out.println("This may cause misalignment of invariants and may cause Diff to");
      System.out.println("work incorectly.  Make sure you know what you are doing!");
    }

    // The index of the first non-option argument -- the name of the
    // first file
    int firstFileIndex = g.getOptind();
    int numFiles = args.length - firstFileIndex;

    InvMap invMap1 = null;
    InvMap invMap2 = null;

    if (logging) {
      System.err.println("Invariant Diff: Reading Files");
    }

    if (numFiles == 1) {
      String filename1 = args[firstFileIndex];
      invMap1 = diff.readInvMap(new File(filename1));
      invMap2 = new InvMap();
    } else if (numFiles == 2) {
      String filename1 = args[firstFileIndex];
      String filename2 = args[firstFileIndex + 1];
      invMap1 = diff.readInvMap(new File(filename1));
      invMap2 = diff.readInvMap(new File(filename2));
    } else if (numFiles > 2) {

      // The new stuff that allows multiple files -LL

      PptMap[] mapAr = new PptMap[numFiles];
      int j = 0;
      for (int i = firstFileIndex; i < args.length; i++) {
        String fileName = args[i];
        mapAr[j++] = FileIO.read_serialized_pptmap(new File(fileName), false);
      }

      // Cascade a lot of the different invariants into one map,
      // and then put them into map1, map2

      // Initialize it all
      MultiDiffVisitor v1 = new MultiDiffVisitor(mapAr[0]);

      for (int i = 1; i < mapAr.length; i++) {
        RootNode root = diff.diffPptMap(mapAr[i], v1.currMap, includeUnjustified);
        root.accept(v1);
      }

      // now take the final result for the MultiDiffVisitor
      // and use it along side a null empty map
      // PptMap map1 = v1.currMap;

      v1.printAll();
      return;
    } else {
      System.out.println(usage);
      throw new Daikon.NormalTermination();
    }

    if (logging) {
      System.err.println("Invariant Diff: Creating Tree");
    }

    if (logging) {
      System.err.println("Invariant Diff: Visiting Tree");
    }

    RootNode root = diff.diffInvMap(invMap1, invMap2, includeUnjustified);

    if (stats) {
      DetailedStatisticsVisitor v = new DetailedStatisticsVisitor(continuousJustification);
      root.accept(v);
      System.out.print(v.format());
    }

    if (tabSeparatedStats) {
      DetailedStatisticsVisitor v = new DetailedStatisticsVisitor(continuousJustification);
      root.accept(v);
      System.out.print(v.repr());
    }

    if (printDiff) {
      PrintDifferingInvariantsVisitor v =
          new PrintDifferingInvariantsVisitor(System.out, verbose, printEmptyPpts);
      root.accept(v);
    }

    if (printAll) {
      PrintAllVisitor v = new PrintAllVisitor(System.out, verbose, printEmptyPpts);
      root.accept(v);
    }

    if (minus) {
      if (outputFile != null) {
        MinusVisitor v = new MinusVisitor();
        root.accept(v);
        FilesPlume.writeObject(v.getResult(), outputFile);
        // System.out.println("Output written to: " + outputFile);
      } else {
        throw new Error("no output file specified on command line");
      }
    }

    if (xor) {
      if (outputFile != null) {
        XorVisitor v = new XorVisitor();
        root.accept(v);
        InvMap resultMap = v.getResult();
        FilesPlume.writeObject(resultMap, outputFile);
        if (debug.isLoggable(FINE)) {
          debug.fine("Result: " + resultMap.toString());
        }

        // System.out.println("Output written to: " + outputFile);
      } else {
        throw new Error("no output file specified on command line");
      }
    }

    if (union) {
      if (outputFile != null) {
        UnionVisitor v = new UnionVisitor();
        root.accept(v);
        FilesPlume.writeObject(v.getResult(), outputFile);
        // System.out.println("Output written to: " + outputFile);
      } else {
        throw new Error("no output file specified on command line");
      }
    }

    if (logging) {
      System.err.println("Invariant Diff: Ending Log");
    }

    // finished; return (and end program)
  }

  /**
   * Reads an InvMap from a file that contains a serialized InvMap or PptMap.
   *
   * @param file a file
   * @return an InvMap read from the file
   * @throws IOException if there is trouble reading the file
   * @throws ClassNotFoundException if an object in the serialized file has an unloadable class
   */
  private InvMap readInvMap(File file) throws IOException, ClassNotFoundException {
    Object o = FilesPlume.readObject(file);
    if (o instanceof InvMap) {
      return (InvMap) o;
    } else {
      PptMap pptMap = FileIO.read_serialized_pptmap(file, false);
      return convertToInvMap(pptMap);
    }
  }

  /**
   * Extracts the PptTopLevel and Invariants out of a pptMap, and places them into an InvMap. Maps
   * PptTopLevel to a List of Invariants. The InvMap is a cleaner representation than the PptMap,
   * and it allows us to more easily manipulate the contents. The InvMap contains exactly the
   * elements contained in the PptMap. Conditional program points are also added as keys. Filtering
   * is done when creating the pair tree. The ppts in the InvMap must be sorted, but the invariants
   * need not be sorted.
   */
  public InvMap convertToInvMap(PptMap pptMap) {
    InvMap map = new InvMap();

    // Created sorted set of top level ppts, possibly including
    // conditional ppts
    NavigableSet<PptTopLevel> ppts = new TreeSet<>(PPT_COMPARATOR);
    ppts.addAll(pptMap.asCollection());

    for (PptTopLevel ppt : ppts) {
      if (ignoreNumberedExits && ppt.ppt_name.isNumberedExitPoint()) {
        continue;
      }

      // List<Invariant> invs = ppt.getInvariants();
      List<Invariant> invs = CollectionsPlume.sortList(ppt.getInvariants(), PptTopLevel.icfp);
      map.put(ppt, invs);
      if (examineAllPpts) {
        // Add conditional ppts
        for (PptConditional pptCond : ppt.cond_iterable()) {
          List<Invariant> invsCond =
              CollectionsPlume.sortList(pptCond.getInvariants(), PptTopLevel.icfp);
          // List<Invariant> invsCond = pptCond.getInvariants();
          map.put(pptCond, invsCond);
        }
      }
    }
    return map;
  }

  /**
   * Returns a pair tree of corresponding program points, and corresponding invariants at each
   * program point. This tree can be walked to determine differences between the sets of invariants.
   * Calls diffInvMap and asks to include all justified invariants.
   */
  public RootNode diffInvMap(InvMap map1, InvMap map2) {
    return diffInvMap(map1, map2, true);
  }

  /**
   * Returns a pair tree of corresponding program points, and corresponding invariants at each
   * program point. This tree can be walked to determine differences between the sets of invariants.
   * The tree consists of the invariants in map1 and map2. If includeUnjustified is true, the
   * unjustified invariants are included.
   */
  public RootNode diffInvMap(InvMap map1, InvMap map2, boolean includeUnjustified) {
    RootNode root = new RootNode();

    Iterator<MPair<@Nullable PptTopLevel, @Nullable PptTopLevel>> opi =
        new OrderedPairIterator<PptTopLevel>(
            map1.pptSortedIterator(PPT_COMPARATOR),
            map2.pptSortedIterator(PPT_COMPARATOR),
            PPT_COMPARATOR);
    while (opi.hasNext()) {
      MPair<@Nullable PptTopLevel, @Nullable PptTopLevel> ppts = opi.next();
      PptTopLevel ppt1 = ppts.first;
      PptTopLevel ppt2 = ppts.second;
      if (shouldAdd(ppt1) || shouldAdd(ppt2)) {
        PptNode node = diffPptTopLevel(ppt1, ppt2, map1, map2, includeUnjustified);
        root.add(node);
      }
    }

    return root;
  }

  /**
   * Diffs two PptMaps by converting them to InvMaps. Provided for compatibiliy with legacy code.
   * Calls diffPptMap and asks to include all invariants.
   */
  public RootNode diffPptMap(PptMap pptMap1, PptMap pptMap2) {
    return diffPptMap(pptMap1, pptMap2, true);
  }

  /**
   * Diffs two PptMaps by converting them to InvMaps. Provided for compatibiliy with legacy code. If
   * includeUnjustified is true, the unjustified invariants are included.
   */
  public RootNode diffPptMap(PptMap pptMap1, PptMap pptMap2, boolean includeUnjustified) {
    InvMap map1 = convertToInvMap(pptMap1);
    InvMap map2 = convertToInvMap(pptMap2);
    return diffInvMap(map1, map2, includeUnjustified);
  }

  /** Returns true if the program point should be added to the tree, false otherwise. */
  private boolean shouldAdd(@Nullable PptTopLevel ppt) {
    if (examineAllPpts) {
      return true;
    } else {
      if (ppt == null) {
        return false;
      } else if (ppt.ppt_name.isEnterPoint()) {
        return true;
      } else if (ppt.ppt_name.isCombinedExitPoint()) {
        return true;
      } else {
        return false;
      }
    }
  }

  /**
   * Takes a pair of corresponding top-level program points and maps, and returns a tree of the
   * corresponding invariants. Either of the program points may be null. If includeUnjustied is
   * true, the unjustified invariants are included.
   */
  private PptNode diffPptTopLevel(
      @Nullable PptTopLevel ppt1,
      @Nullable PptTopLevel ppt2,
      InvMap map1,
      InvMap map2,
      boolean includeUnjustified) {
    PptNode pptNode = new PptNode(ppt1, ppt2);

    assert ppt1 == null || ppt2 == null || PPT_COMPARATOR.compare(ppt1, ppt2) == 0
        : "Program points do not correspond";

    List<Invariant> invs1;
    if (ppt1 != null) {
      invs1 = map1.get(ppt1);
      Collections.sort(invs1, invSortComparator1);
    } else {
      invs1 = new ArrayList<Invariant>();
    }

    List<Invariant> invs2;
    if (ppt2 != null) {
      invs2 = map2.get(ppt2);
      Collections.sort(invs2, invSortComparator2);
    } else {
      invs2 = new ArrayList<Invariant>();
    }

    Iterator<MPair<@Nullable Invariant, @Nullable Invariant>> opi =
        new OrderedPairIterator<Invariant>(invs1.iterator(), invs2.iterator(), invPairComparator);
    while (opi.hasNext()) {
      MPair<@Nullable Invariant, @Nullable Invariant> invariants = opi.next();
      Invariant inv1 = invariants.first;
      Invariant inv2 = invariants.second;
      if (!includeUnjustified) {
        if ((inv1 != null) && !inv1.justified()) {
          inv1 = null;
        }
        if ((inv2 != null) && !inv2.justified()) {
          inv2 = null;
        }
      }
      if ((inv1 != null) || (inv2 != null)) {
        InvNode invNode = new InvNode(inv1, inv2);
        pptNode.add(invNode);
      }
    }

    return pptNode;
  }

  /** Use the comparator for sorting both sets and creating the pair tree. */
  @EnsuresNonNull({"invSortComparator1", "invSortComparator2", "invPairComparator"})
  public void setAllInvComparators(@UnknownInitialization Diff this, Comparator<Invariant> c) {
    setInvSortComparator1(c);
    setInvSortComparator2(c);
    setInvPairComparator(c);
  }

  /**
   * If the classname is non-null, returns the comparator named by the classname. Else, returns the
   * default.
   */
  private static Comparator<Invariant> selectComparator(
      @Nullable @ClassGetName String classname, Comparator<Invariant> defaultComparator)
      throws ClassNotFoundException,
          IllegalAccessException,
          InstantiationException,
          InvocationTargetException,
          NoSuchMethodException {

    if (classname != null) {
      Class<?> cls = Class.forName(classname);
      @SuppressWarnings("unchecked")
      Comparator<Invariant> cmp =
          (Comparator<Invariant>) cls.getDeclaredConstructor().newInstance();
      return cmp;
    } else {
      return defaultComparator;
    }
  }

  /** Use the comparator for sorting the first set. */
  @EnsuresNonNull("invSortComparator1")
  public void setInvSortComparator1(@UnknownInitialization Diff this, Comparator<Invariant> c) {
    invSortComparator1 = c;
  }

  /** Use the comparator for sorting the second set. */
  @EnsuresNonNull("invSortComparator2")
  public void setInvSortComparator2(@UnknownInitialization Diff this, Comparator<Invariant> c) {
    invSortComparator2 = c;
  }

  /** Use the comparator for creating the pair tree. */
  @EnsuresNonNull("invPairComparator")
  public void setInvPairComparator(@UnknownInitialization Diff this, Comparator<Invariant> c) {
    invPairComparator = c;
  }
}
