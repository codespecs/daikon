package daikon;

import static java.util.logging.Level.FINE;
import static java.util.logging.Level.INFO;

import daikon.split.PptSplitter;
import daikon.suppress.NIS;
import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OptionalDataException;
import java.io.StreamCorruptedException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.plumelib.util.FilesPlume;
import org.plumelib.util.StringsPlume;

/**
 * Merges invariants from multiple invariant files into a single invariant file. It does this by
 * forming a hierarchy over the ppts from each invariant file and using the normal hierarchy merging
 * code to merge the invariants.
 *
 * <p>The ppts from each invariant file are merged to create a single ppt map that contains the ppts
 * from all of the files. At each leaf of the merged map, a hierarchy is formed to the ppts from
 * each of the input files.
 */
public final class MergeInvariants {
  private MergeInvariants() {
    throw new Error("do not instantiate");
  }

  /** Debug logger. */
  public static final Logger debug = Logger.getLogger("daikon.MergeInvariants");

  /** Progress logger. */
  public static final Logger debugProgress = Logger.getLogger("daikon.MergeInvariants.progress");

  /** The file in which to produce output; if null, the results are printed. */
  public static @Nullable File output_inv_file;

  /** The usage message for this program. */
  private static String usage =
      StringsPlume.joinLines(
          "Usage: java daikon.MergeInvariants [OPTION]... FILE",
          "  -h, --" + Daikon.help_SWITCH,
          "      Display this usage message",
          "  --" + Daikon.config_option_SWITCH,
          "      Specify a configuration option ",
          "  --" + Daikon.debug_SWITCH,
          "      Specify a logger to enable",
          "  --" + Daikon.track_SWITCH,
          "      Specify a class, varinfos, and ppt to debug track.  Format"
              + "is class<var1,var2,var3>@ppt",
          "   -o ",
          "      Specify an output inv file.  If not specified, the results are printed");

  public static void main(final String[] args)
      throws FileNotFoundException,
          StreamCorruptedException,
          OptionalDataException,
          IOException,
          ClassNotFoundException {
    try {
      mainHelper(args);
    } catch (Daikon.DaikonTerminationException e) {
      Daikon.handleDaikonTerminationException(e);
    }
  }

  /**
   * This does the work of {@link #main(String[])}, but it never calls System.exit, so it is
   * appropriate to be called progrmmatically.
   *
   * @param args the command-line arguments
   * @throws FileNotFoundException if a file cannot be found
   * @throws StreamCorruptedException if a stream is corrupted
   * @throws OptionalDataException if there is a serialization problem
   * @throws IOException if there is trouble with I/O
   * @throws ClassNotFoundException if a class cannot be found
   */
  @SuppressWarnings("nullness:contracts.precondition") // private field
  public static void mainHelper(String[] args)
      throws FileNotFoundException,
          StreamCorruptedException,
          OptionalDataException,
          IOException,
          ClassNotFoundException {

    daikon.LogHelper.setupLogs(INFO);

    LongOpt[] longopts =
        new LongOpt[] {
          new LongOpt(Daikon.help_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(Daikon.config_option_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(Daikon.track_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
        };

    Getopt g = new Getopt("daikon.MergeInvariants", args, "ho:", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch (c) {

          // long option
        case 0:
          String option_name = longopts[g.getLongind()].getName();
          if (Daikon.help_SWITCH.equals(option_name)) {
            System.out.println(usage);
            throw new Daikon.NormalTermination();
          } else if (Daikon.config_option_SWITCH.equals(option_name)) {
            String item = Daikon.getOptarg(g);
            daikon.config.Configuration.getInstance().apply(item);
            break;

          } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
            Global.debugAll = true;

          } else if (Daikon.debug_SWITCH.equals(option_name)) {
            LogHelper.setLevel(Daikon.getOptarg(g), FINE);
          } else if (Daikon.track_SWITCH.equals(option_name)) {
            LogHelper.setLevel("daikon.Debug", FINE);
            String error = Debug.add_track(Daikon.getOptarg(g));
            if (error != null) {
              throw new Daikon.UserError(
                  "Error parsing track argument '" + Daikon.getOptarg(g) + "' - " + error);
            }
          } else {
            throw new Daikon.UserError("Unknown long option received: " + option_name);
          }
          break;

        case 'h':
          System.out.println(usage);
          throw new Daikon.NormalTermination();

        case 'o':
          String output_inv_filename = Daikon.getOptarg(g);

          if (output_inv_file != null) {
            throw new Daikon.UserError(
                "multiple serialization output files supplied on command line: "
                    + output_inv_file
                    + " "
                    + output_inv_filename);
          }

          output_inv_file = new File(output_inv_filename);

          if (!FilesPlume.canCreateAndWrite(output_inv_file)) {
            throw new Daikon.UserError(
                "Cannot write to serialization output file " + output_inv_file);
          }
          break;

        case '?':
          break; // getopt() already printed an error

        default:
          System.out.println("getopt() returned " + c);
          break;
      }
    }

    daikon.LogHelper.setupLogs(Global.debugAll ? FINE : INFO);

    List<File> inv_files = new ArrayList<>();
    File decl_file = null;
    Set<File> splitter_files = new TreeSet<>();

    // Get each file specified
    for (int i = g.getOptind(); i < args.length; i++) {
      File file = new File(args[i]);
      if (!file.exists()) {
        throw new Daikon.UserError("File " + file + " not found.");
      }
      if (file.toString().indexOf(".inv") != -1) {
        inv_files.add(file);
      } else if (file.toString().indexOf(".decls") != -1) {
        if (decl_file != null) {
          throw new Daikon.UserError("Only one decl file may be specified");
        }
        decl_file = file;
      } else if (file.toString().indexOf(".spinfo") != -1) {
        splitter_files.add(file);
      } else {
        throw new Daikon.UserError("Unrecognized file type: " + file);
      }
    }

    // Make sure at least two files were specified
    if (inv_files.size() < 2) {
      throw new Daikon.UserError(
          "Provided "
              + StringsPlume.nplural(inv_files.size(), "inv file")
              + " but needs at least two");
    }

    // Setup the default for guarding
    PrintInvariants.validateGuardNulls();

    // Initialize the prototype invariants and NI suppressions
    Daikon.setup_proto_invs();
    NIS.init_ni_suppression();

    // Read in each of the specified maps
    List<PptMap> pptmaps = new ArrayList<>();
    for (File file : inv_files) {
      debugProgress.fine("Processing " + file);
      PptMap ppts = FileIO.read_serialized_pptmap(file, true);
      ppts.repCheck();
      pptmaps.add(ppts);
      Debug.check(ppts, "After initial reading of " + file);
    }

    // Merged ppt map (result of merging each specified inv file)
    PptMap merge_ppts = null;

    // if no decls file was specified
    if (decl_file == null) {
      if (splitter_files.size() > 0) {
        throw new Daikon.UserError(".spinfo files may only be specified along with a .decls file");
      }

      // Read in each of the maps again to build a template that contains all
      // of the program points from each map.
      for (File file : inv_files) {
        debugProgress.fine("Reading " + file + " as merge template");
        if (merge_ppts == null) {
          merge_ppts = FileIO.read_serialized_pptmap(file, true);
        } else {
          PptMap pmap = FileIO.read_serialized_pptmap(file, true);
          for (PptTopLevel ppt : pmap.pptIterable()) {
            if (merge_ppts.containsName(ppt.name())) {
              // System.out.printf("Not adding ppt %s from %s%n", ppt, file);
              continue;
            }
            merge_ppts.add(ppt);
            // System.out.printf("Adding ppt %s from %s%n", ppt, file);

            // Make sure that the parents of this ppt are already in
            // the map.  This will be true if all possible children of
            // any ppt are always included in the same invariant file.
            // For example, all possible enter/exit points should be
            // included with each object point.  This is true for Chicory
            // as long as ppt filtering didn't remove some ppts.
            for (PptRelation rel : ppt.parents) {
              assert merge_ppts.get(rel.parent.name()) == rel.parent : ppt + " - " + rel;
            }
          }
        }
      }
      assert merge_ppts != null
          : "@AssumeAssertion(nullness): inv_files is non-empty, so for-loop body executed";

      // Remove all of the slices, equality sets, to start
      debugProgress.fine("Cleaning ppt map in preparation for merge");
      for (PptTopLevel ppt : merge_ppts.ppt_all_iterable()) {
        ppt.clean_for_merge();
      }

    } else {

      // Build the result pptmap from the specific decls file
      debugProgress.fine("Building result ppt map from decls file");
      Daikon.create_splitters(splitter_files);
      List<File> decl_files = new ArrayList<>();
      decl_files.add(decl_file);
      merge_ppts = FileIO.read_declaration_files(decl_files);
      merge_ppts.trimToSize();
      PptRelation.init_hierarchy(merge_ppts);
    }

    // Create a hierarchy between the merge exitNN points and the
    // corresponding points in each of the specified maps.  This
    // should only be created at the exitNN points (i.e., the leaves)
    // so that the normal processing will create the invariants at
    // upper points.
    debugProgress.fine("Building hierarchy between leaves of the maps");
    for (PptTopLevel ppt : merge_ppts.pptIterable()) {

      // Skip everything that is not a final exit point
      if (!ppt.ppt_name.isExitPoint()) {
        assert ppt.children.size() > 0 : ppt;
        continue;
      }
      if (ppt.ppt_name.isCombinedExitPoint()) {
        assert ppt.children.size() > 0 : ppt;
        continue;
      }

      // System.out.printf("Including ppt %s, %d children%n", ppt,
      //                   ppt.children.size());

      // Splitters should not have any children to begin with
      if (ppt.has_splitters()) {
        assert ppt.splitters != null; // because ppt.has_splitters() = true
        for (PptSplitter ppt_split : ppt.splitters) {
          for (PptTopLevel p : ppt_split.ppts) {
            assert p.children.size() == 0 : p;
          }
        }
      }

      // Loop over each of the input ppt maps, looking for the same ppt
      for (int j = 0; j < pptmaps.size(); j++) {
        PptMap pmap = pptmaps.get(j);
        PptTopLevel child = pmap.get(ppt.name());
        // System.out.printf("found child %s from pmap %d%n", child, j);
        if (child == null) {
          continue;
        }
        if (child.equality_view == null) {
          System.out.println(
              "equality_view == null in child ppt: "
                  + child.name()
                  + " ("
                  + inv_files.get(j)
                  + ")");
        } else if (child.equality_view.invs == null) {
          System.out.println(
              "equality_view.invs == null in child ppt: "
                  + child.name()
                  + " ("
                  + inv_files.get(j)
                  + ")"
                  + " samples = "
                  + child.num_samples());
        }

        // Remove the equality invariants added during equality post
        // processing.  These are not over leaders and will cause problems
        // in the merge
        child.remove_equality_invariants();
        child.in_merge = false;

        // Remove implications, they don't merge correctly
        child.remove_implications();

        // If the ppt has splitters, attach the child's splitters to the
        // splitters.  Don't attach the ppt itself, as its invariants can
        // be built from the invariants in the splitters.
        if (ppt.has_splitters()) {
          assert ppt.splitters != null; // because ppt.has_splitters() = true
          setup_conditional_merge(ppt, child);
        } else {
          PptRelation.newMergeChildRel(ppt, child);
        }
      }

      // Make sure at least one child was found
      assert ppt.children.size() > 0 : ppt;
      if (ppt.has_splitters()) {
        assert ppt.splitters != null; // because ppt.has_splitters() = true
        for (PptSplitter ppt_split : ppt.splitters) {
          for (PptTopLevel p : ppt_split.ppts) {
            assert p.children.size() > 0 : p;
          }
        }
      }
    }

    // Check the resulting PptMap for consistency
    merge_ppts.repCheck();

    // Debug print the hierarchy is a more readable manner
    if (debug.isLoggable(FINE)) {
      debug.fine("PPT Hierarchy");
      for (PptTopLevel ppt : merge_ppts.pptIterable()) {
        if (ppt.parents.size() == 0) {
          ppt.debug_print_tree(debug, 0, null);
        }
      }
    }

    // Merge the invariants
    debugProgress.fine("Merging invariants");
    Daikon.createUpperPpts(merge_ppts);

    // Equality post processing
    debugProgress.fine("Equality Post Processing");
    for (PptTopLevel ppt : merge_ppts.ppt_all_iterable()) {
      ppt.postProcessEquality();
    }

    // Implications
    long startTime = System.nanoTime();
    // System.out.println("Creating implications ");
    debugProgress.fine("Adding Implications ... ");
    for (PptTopLevel ppt : merge_ppts.pptIterable()) {
      if (ppt.num_samples() > 0) {
        ppt.addImplications();
      }
    }
    long duration = System.nanoTime() - startTime;
    debugProgress.fine("Time spent in implications: " + TimeUnit.NANOSECONDS.toSeconds(duration));

    // Remove the PptRelation links so that when the file is written
    // out it only includes the new information
    for (PptTopLevel ppt : merge_ppts.pptIterable()) {
      if (!ppt.ppt_name.isExitPoint()) {
        continue;
      }
      if (ppt.ppt_name.isCombinedExitPoint()) {
        continue;
      }
      ppt.children.clear();
      for (PptConditional cond : ppt.cond_iterable()) {
        cond.children.clear();
      }
    }

    // Write serialized output
    debugProgress.fine("Writing Output");
    if (output_inv_file != null) {
      try {
        FileIO.write_serialized_pptmap(merge_ppts, output_inv_file);
      } catch (IOException e) {
        throw new RuntimeException(
            "Error while writing .inv file '" + output_inv_file + "': " + e.toString());
      }
    } else {
      // Print the invariants
      PrintInvariants.print_invariants(merge_ppts);
    }
  }

  /**
   * Ses up the specified relation beteween each of the conditionals in ppt and the matching
   * conditionals in child. Each must have the same number of splitters setup in the same order. The
   * splitter match can't be checked because splitters can't be read back in.
   */
  private static void setup_conditional_merge(PptTopLevel ppt, PptTopLevel child) {

    // Both ppt and child should have splitters
    if (ppt.has_splitters() != child.has_splitters()) {
      throw new Error(
          "Merge ppt "
              + ppt.name
              + (ppt.has_splitters() ? " has " : "doesn't have ")
              + "splitters, but child ppt "
              + child.name
              + (child.has_splitters() ? " does" : " doesn't"));
    }

    // Nothing to do if there are no splitters here
    if (!ppt.has_splitters()) {
      return;
    }

    assert child.splitters != null
        : "@AssumeAssertion(nullness): correlated: ppt.has_splitters() == child.has_splitters(),"
            + " and ppt.has_splitters() == true";

    // Both ppt and child should have the same number of splitters
    if (ppt.splitters.size() != child.splitters.size()) {
      throw new Error(
          "Merge ppt "
              + ppt.name
              + " has "
              + ((ppt.splitters.size() > child.splitters.size()) ? "more" : "fewer")
              + " splitters ("
              + ppt.splitters.size()
              + ") than child ppt "
              + child.name
              + " ("
              + child.splitters.size()
              + ")");
    }

    // Create a relation from each conditional ppt to its corresponding
    // conditional ppt in the child.
    for (int ii = 0; ii < ppt.splitters.size(); ii++) {
      PptSplitter ppt_split = ppt.splitters.get(ii);
      PptSplitter child_split = child.splitters.get(ii);
      for (int jj = 0; jj < ppt_split.ppts.length; jj++) {
        child_split.ppts[jj].remove_equality_invariants();
        child_split.ppts[jj].in_merge = false;
        PptRelation.newMergeChildRel(ppt_split.ppts[jj], child_split.ppts[jj]);
      }
    }
  }
}
