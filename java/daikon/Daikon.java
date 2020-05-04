// Main routine for Daikon invariant detector
// For documentation, see file doc/daikon.html in the distribution.

package daikon;

import daikon.config.Configuration;
import daikon.derive.Derivation;
import daikon.inv.Equality;
import daikon.inv.Invariant;
import daikon.inv.OutputFormat;
import daikon.inv.binary.sequenceScalar.Member;
import daikon.inv.binary.sequenceScalar.MemberFloat;
import daikon.inv.binary.sequenceScalar.SeqFloatEqual;
import daikon.inv.binary.sequenceScalar.SeqFloatGreaterEqual;
import daikon.inv.binary.sequenceScalar.SeqFloatGreaterThan;
import daikon.inv.binary.sequenceScalar.SeqFloatLessEqual;
import daikon.inv.binary.sequenceScalar.SeqFloatLessThan;
import daikon.inv.binary.sequenceScalar.SeqIntEqual;
import daikon.inv.binary.sequenceScalar.SeqIntGreaterEqual;
import daikon.inv.binary.sequenceScalar.SeqIntGreaterThan;
import daikon.inv.binary.sequenceScalar.SeqIntLessEqual;
import daikon.inv.binary.sequenceScalar.SeqIntLessThan;
import daikon.inv.binary.sequenceString.MemberString;
import daikon.inv.binary.twoScalar.FloatEqual;
import daikon.inv.binary.twoScalar.FloatGreaterEqual;
import daikon.inv.binary.twoScalar.FloatGreaterThan;
import daikon.inv.binary.twoScalar.FloatLessEqual;
import daikon.inv.binary.twoScalar.FloatLessThan;
import daikon.inv.binary.twoScalar.FloatNonEqual;
import daikon.inv.binary.twoScalar.IntEqual;
import daikon.inv.binary.twoScalar.IntGreaterEqual;
import daikon.inv.binary.twoScalar.IntGreaterThan;
import daikon.inv.binary.twoScalar.IntLessEqual;
import daikon.inv.binary.twoScalar.IntLessThan;
import daikon.inv.binary.twoScalar.IntNonEqual;
import daikon.inv.binary.twoScalar.LinearBinary;
import daikon.inv.binary.twoScalar.LinearBinaryFloat;
import daikon.inv.binary.twoScalar.NumericFloat;
import daikon.inv.binary.twoScalar.NumericInt;
import daikon.inv.binary.twoSequence.PairwiseFloatEqual;
import daikon.inv.binary.twoSequence.PairwiseFloatGreaterEqual;
import daikon.inv.binary.twoSequence.PairwiseFloatGreaterThan;
import daikon.inv.binary.twoSequence.PairwiseFloatLessEqual;
import daikon.inv.binary.twoSequence.PairwiseFloatLessThan;
import daikon.inv.binary.twoSequence.PairwiseIntEqual;
import daikon.inv.binary.twoSequence.PairwiseIntGreaterEqual;
import daikon.inv.binary.twoSequence.PairwiseIntGreaterThan;
import daikon.inv.binary.twoSequence.PairwiseIntLessEqual;
import daikon.inv.binary.twoSequence.PairwiseIntLessThan;
import daikon.inv.binary.twoSequence.PairwiseLinearBinary;
import daikon.inv.binary.twoSequence.PairwiseLinearBinaryFloat;
import daikon.inv.binary.twoSequence.PairwiseNumericFloat;
import daikon.inv.binary.twoSequence.PairwiseNumericInt;
import daikon.inv.binary.twoSequence.PairwiseString;
import daikon.inv.binary.twoSequence.PairwiseStringEqual;
import daikon.inv.binary.twoSequence.PairwiseStringGreaterEqual;
import daikon.inv.binary.twoSequence.PairwiseStringGreaterThan;
import daikon.inv.binary.twoSequence.PairwiseStringLessEqual;
import daikon.inv.binary.twoSequence.PairwiseStringLessThan;
import daikon.inv.binary.twoSequence.Reverse;
import daikon.inv.binary.twoSequence.ReverseFloat;
import daikon.inv.binary.twoSequence.SeqSeqFloatEqual;
import daikon.inv.binary.twoSequence.SeqSeqFloatGreaterEqual;
import daikon.inv.binary.twoSequence.SeqSeqFloatGreaterThan;
import daikon.inv.binary.twoSequence.SeqSeqFloatLessEqual;
import daikon.inv.binary.twoSequence.SeqSeqFloatLessThan;
import daikon.inv.binary.twoSequence.SeqSeqIntEqual;
import daikon.inv.binary.twoSequence.SeqSeqIntGreaterEqual;
import daikon.inv.binary.twoSequence.SeqSeqIntGreaterThan;
import daikon.inv.binary.twoSequence.SeqSeqIntLessEqual;
import daikon.inv.binary.twoSequence.SeqSeqIntLessThan;
import daikon.inv.binary.twoSequence.SeqSeqStringEqual;
import daikon.inv.binary.twoSequence.SeqSeqStringGreaterEqual;
import daikon.inv.binary.twoSequence.SeqSeqStringGreaterThan;
import daikon.inv.binary.twoSequence.SeqSeqStringLessEqual;
import daikon.inv.binary.twoSequence.SeqSeqStringLessThan;
import daikon.inv.binary.twoSequence.SubSequence;
import daikon.inv.binary.twoSequence.SubSequenceFloat;
import daikon.inv.binary.twoSequence.SubSet;
import daikon.inv.binary.twoSequence.SubSetFloat;
import daikon.inv.binary.twoSequence.SuperSequence;
import daikon.inv.binary.twoSequence.SuperSequenceFloat;
import daikon.inv.binary.twoSequence.SuperSet;
import daikon.inv.binary.twoSequence.SuperSetFloat;
import daikon.inv.binary.twoString.StdString;
import daikon.inv.binary.twoString.StringEqual;
import daikon.inv.binary.twoString.StringGreaterEqual;
import daikon.inv.binary.twoString.StringGreaterThan;
import daikon.inv.binary.twoString.StringLessEqual;
import daikon.inv.binary.twoString.StringLessThan;
import daikon.inv.binary.twoString.StringNonEqual;
import daikon.inv.ternary.threeScalar.FunctionBinary;
import daikon.inv.ternary.threeScalar.FunctionBinaryFloat;
import daikon.inv.ternary.threeScalar.LinearTernary;
import daikon.inv.ternary.threeScalar.LinearTernaryFloat;
import daikon.inv.unary.scalar.CompleteOneOfScalar;
import daikon.inv.unary.scalar.IsPointer;
import daikon.inv.unary.scalar.LowerBound;
import daikon.inv.unary.scalar.LowerBoundFloat;
import daikon.inv.unary.scalar.Modulus;
import daikon.inv.unary.scalar.NonModulus;
import daikon.inv.unary.scalar.NonZero;
import daikon.inv.unary.scalar.NonZeroFloat;
import daikon.inv.unary.scalar.OneOfFloat;
import daikon.inv.unary.scalar.OneOfScalar;
import daikon.inv.unary.scalar.RangeFloat;
import daikon.inv.unary.scalar.RangeInt;
import daikon.inv.unary.scalar.UpperBound;
import daikon.inv.unary.scalar.UpperBoundFloat;
import daikon.inv.unary.sequence.CommonFloatSequence;
import daikon.inv.unary.sequence.CommonSequence;
import daikon.inv.unary.sequence.EltLowerBound;
import daikon.inv.unary.sequence.EltLowerBoundFloat;
import daikon.inv.unary.sequence.EltNonZero;
import daikon.inv.unary.sequence.EltNonZeroFloat;
import daikon.inv.unary.sequence.EltOneOf;
import daikon.inv.unary.sequence.EltOneOfFloat;
import daikon.inv.unary.sequence.EltRangeFloat;
import daikon.inv.unary.sequence.EltRangeInt;
import daikon.inv.unary.sequence.EltUpperBound;
import daikon.inv.unary.sequence.EltUpperBoundFloat;
import daikon.inv.unary.sequence.EltwiseFloatEqual;
import daikon.inv.unary.sequence.EltwiseFloatGreaterEqual;
import daikon.inv.unary.sequence.EltwiseFloatGreaterThan;
import daikon.inv.unary.sequence.EltwiseFloatLessEqual;
import daikon.inv.unary.sequence.EltwiseFloatLessThan;
import daikon.inv.unary.sequence.EltwiseIntEqual;
import daikon.inv.unary.sequence.EltwiseIntGreaterEqual;
import daikon.inv.unary.sequence.EltwiseIntGreaterThan;
import daikon.inv.unary.sequence.EltwiseIntLessEqual;
import daikon.inv.unary.sequence.EltwiseIntLessThan;
import daikon.inv.unary.sequence.NoDuplicates;
import daikon.inv.unary.sequence.NoDuplicatesFloat;
import daikon.inv.unary.sequence.OneOfFloatSequence;
import daikon.inv.unary.sequence.OneOfSequence;
import daikon.inv.unary.sequence.SeqIndexFloatEqual;
import daikon.inv.unary.sequence.SeqIndexFloatGreaterEqual;
import daikon.inv.unary.sequence.SeqIndexFloatGreaterThan;
import daikon.inv.unary.sequence.SeqIndexFloatLessEqual;
import daikon.inv.unary.sequence.SeqIndexFloatLessThan;
import daikon.inv.unary.sequence.SeqIndexFloatNonEqual;
import daikon.inv.unary.sequence.SeqIndexIntEqual;
import daikon.inv.unary.sequence.SeqIndexIntGreaterEqual;
import daikon.inv.unary.sequence.SeqIndexIntGreaterThan;
import daikon.inv.unary.sequence.SeqIndexIntLessEqual;
import daikon.inv.unary.sequence.SeqIndexIntLessThan;
import daikon.inv.unary.sequence.SeqIndexIntNonEqual;
import daikon.inv.unary.string.CompleteOneOfString;
import daikon.inv.unary.string.OneOfString;
import daikon.inv.unary.string.PrintableString;
import daikon.inv.unary.stringsequence.CommonStringSequence;
import daikon.inv.unary.stringsequence.EltOneOfString;
import daikon.inv.unary.stringsequence.OneOfStringSequence;
import daikon.split.ContextSplitterFactory;
import daikon.split.PptSplitter;
import daikon.split.SpinfoFile;
import daikon.split.Splitter;
import daikon.split.SplitterFactory;
import daikon.split.SplitterList;
import daikon.suppress.NIS;
import daikon.suppress.NIS.SuppressionProcessor;
import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.LineNumberReader;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.plumelib.util.EntryReader;
import org.plumelib.util.RegexUtil;
import org.plumelib.util.UtilPlume;
import typequals.prototype.qual.Prototype;

/**
 * The {@link #main} method is the main entry point for the Daikon invariant detector. The {@link
 * #mainHelper} method is the entry point, when called programmatically.
 */
@SuppressWarnings(
    "nullness:initialization.static.fields.uninitialized") // field all_ppts; deal with it later
public final class Daikon {

  private Daikon() {
    throw new Error("do not instantiate");
  }

  /**
   * The amount of time to wait between updates of the progress display, measured in milliseconds. A
   * value of -1 means do not print the progress display at all.
   */
  public static int dkconfig_progress_delay = 1000;

  /** The current version of Daikon. */
  public static final String release_version = "5.8.3";
  /** The date for the current version of Daikon. */
  public static final String release_date = "May 4, 2020";
  /** A description of the Daikon release (version number, date, and URL). */
  public static final String release_string =
      "Daikon version "
          + release_version
          + ", released "
          + release_date
          + "; http://plse.cs.washington.edu/daikon.";

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. Controls whether conditional program points are displayed. */
  public static boolean dkconfig_output_conditionals = true;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. Just print the total number of possible invariants and exit. */
  public static boolean dkconfig_calc_possible_invs;

  /**
   * Integer. Percentage of program points to process. All program points are sorted by name, and
   * all samples for the first {@code ppt_perc} program points are processed. A percentage of 100
   * matches all program points.
   */
  public static int dkconfig_ppt_perc = 100;

  /**
   * Boolean. Controls whether or not the total samples read and processed are printed at the end of
   * processing.
   */
  public static boolean dkconfig_print_sample_totals = false;

  // All these variables really need to be organized better.

  public static final String lineSep = Global.lineSep;

  /**
   * Boolean. Controls whether or not processing information is printed out. Setting this variable
   * to true also automatically sets {@code progress_delay} to -1.
   */
  public static boolean dkconfig_quiet = false;

  // Change this at your peril; high costs in time and space for "false",
  // because so many more invariants get instantiated.
  public static final boolean check_program_types = true;

  // Problem with setting this to true:
  //  get no invariants over any value that can ever be missing
  // Problem with setting this to false:
  //  due to different number of samples, isEqualityComparison is
  //  non-transitive (that is specially handled in the code)
  public static final boolean invariants_check_canBeMissing = false;

  // Specialized version for array elements; only examined if
  // invariants_check_canBeMissing is false
  public static final boolean invariants_check_canBeMissing_arrayelt = true;

  public static final boolean disable_modbit_check_message = false;
  // Not a good idea to set this to true, as it is too easy to ignore the
  // warnings and the modbit problem can cause an error later.
  public static final boolean disable_modbit_check_error = false;

  // When true, don't print textual output.
  public static boolean no_text_output = false;

  // When true, show how much time each program point took.
  // Has no effect unless no_text_output is true.
  public static boolean show_progress = false;

  /**
   * Whether to use the "new" equality set mechanism for handling equality, using canonicals to have
   * instantiation of invariants only over equality sets.
   */
  public static boolean use_equality_optimization = true;

  /**
   * Boolean. Controls whether the Daikon optimizations (equality sets, suppressions) are undone at
   * the end to create a more complete set of invariants. Output does not include conditional
   * program points, implications, reflexive and partially reflexive invariants.
   */
  public static boolean dkconfig_undo_opts = false;

  /**
   * Boolean. Indicates to Daikon classes and methods that the methods calls should be compatible to
   * DaikonSimple because Daikon and DaikonSimple share methods. Default value is 'false'.
   */
  public static boolean using_DaikonSimple = false;

  /**
   * If "always", then invariants are always guarded. If "never", then invariants are never guarded.
   * If "missing", then invariants are guarded only for variables that were missing ("can be
   * missing") in the dtrace (the observed executions). If "default", then use "missing" mode for
   * Java output and "never" mode otherwise.
   *
   * <p>Guarding means adding predicates that ensure that variables can be dereferenced. For
   * instance, if {@code a} can be null --- that is, if {@code a.b} can be nonsensical --- then the
   * guarded version of
   *
   * <pre>a.b == 5</pre>
   *
   * is
   *
   * <pre>(a != null) &rArr; (a.b == 5)</pre>
   *
   * .
   *
   * <p>(To do: Some configuration option (maybe this one) should add guards for other reasons that
   * lead to nonsensical values (@pxref{Variable names}).) <br>
   * &#64;cindex nonsensical values for variables, guarding
   */
  // Perhaps a better default would be "missing".
  public static @Interned String dkconfig_guardNulls = "default";

  /**
   * Whether to associate the program points in a dataflow hierarchy, as via Nimmer's thesis.
   * Deactivate only for languages and analyses where flow relation is nonsensical.
   */
  public static boolean use_dataflow_hierarchy = true;

  /**
   * Whether to use the bottom up implementation of the dataflow hierarchy. This mechanism builds
   * invariants initially only at the leaves of the partial order. Upper points are calculated by
   * joining the invariants from each of their children points.
   */
  // public static boolean dkconfig_df_bottom_up = true;

  // When true, use the Simplify theorem prover (not part of Daikon)
  // to locate logically redundant invariants, and flag them as
  // redundant, so that they are removed from the printed output.
  public static boolean suppress_redundant_invariants_with_simplify = false;

  public static OutputFormat output_format = OutputFormat.DAIKON;

  // When true, output numbers of values and samples (also names of variables)
  public static boolean output_num_samples = false;

  public static boolean ignore_comparability = false;

  // Controls which program points/variables are used/ignored.
  public static @Nullable Pattern ppt_regexp;
  public static @Nullable Pattern ppt_omit_regexp;
  public static @Nullable Pattern var_regexp;
  public static @Nullable Pattern var_omit_regexp;

  /**
   * If set, only ppts less than ppt_max_name are included. Used by the configuration option
   * dkconfig_ppt_percent to only work on a specified percent of the ppts.
   */
  public static @Nullable String ppt_max_name = null;

  // The invariants detected will be serialized and written to this
  // file.
  public static @Nullable File inv_file;

  // Whether we want the memory monitor activated
  private static boolean use_mem_monitor = false;

  /** Whether Daikon should print its version number and date. */
  public static boolean noversion_output = false;

  /** Whether Daikon is in its inferencing loop. Used only for assertion checks. */
  public static boolean isInferencing = false;

  /**
   * When true, omit certain invariants from the output {@code .inv} file. Generally these are
   * invariants that wouldn't be printed in any case; but by default, they're retained in the {@code
   * .inv} file in case they would be useful for later processing. (For instance, we might at some
   * point in the future support resuming processing with more data from an {@code .inv} file).
   * These invariants can increase the size of the {@code .inv} file, though, so when only limited
   * further processing is needed, it can save space to omit them.
   */
  public static boolean omit_from_output = false;

  /**
   * An array of flags, indexed by characters, in which a true entry means that invariants of that
   * sort should be omitted from the output {@code .inv} file.
   */
  public static boolean[] omit_types = new boolean[256];

  // Command-line options / command-line arguments
  // These variables are public so other programs can reuse the same
  // command-line options.
  // Please use these switches in the same order in all places where they
  // appear (in the code and in the documentation); it makes the code
  // easier to read and the documentation easier to keep up to date.

  public static final String help_SWITCH = "help";
  // "-o" switch: file to which serialized output is written
  public static final String no_text_output_SWITCH = "no_text_output";
  public static final String format_SWITCH = "format";
  public static final String show_progress_SWITCH = "show_progress";
  public static final String no_show_progress_SWITCH = "no_show_progress";
  public static final String noversion_SWITCH = "noversion";
  public static final String output_num_samples_SWITCH = "output_num_samples";
  public static final String files_from_SWITCH = "files_from";
  public static final String omit_from_output_SWITCH = "omit_from_output";
  // Control invariant detection
  public static final String conf_limit_SWITCH = "conf_limit";
  public static final String list_type_SWITCH = "list_type";
  public static final String user_defined_invariant_SWITCH = "user-defined-invariant";
  public static final String disable_all_invariants_SWITCH = "disable-all-invariants";
  public static final String no_dataflow_hierarchy_SWITCH = "nohierarchy";
  public static final String suppress_redundant_SWITCH = "suppress_redundant";
  // Process only part of the trace file
  public static final String ppt_regexp_SWITCH = "ppt-select-pattern";
  public static final String ppt_omit_regexp_SWITCH = "ppt-omit-pattern";
  public static final String var_regexp_SWITCH = "var-select-pattern";
  public static final String var_omit_regexp_SWITCH = "var-omit-pattern";
  // Configuration options
  public static final String server_SWITCH =
      "server"; // YOAV: server mode for Daikon: reads dtrace files as they appear
  public static final String config_SWITCH = "config";
  public static final String config_option_SWITCH = "config_option";
  // Debugging
  public static final String debugAll_SWITCH = "debug";
  public static final String debug_SWITCH = "dbg";
  public static final String track_SWITCH = "track";
  public static final String disc_reason_SWITCH = "disc_reason";
  public static final String mem_stat_SWITCH = "mem_stat";
  public static final String wrap_xml_SWITCH = "wrap_xml";

  /**
   * Regular expression that matches class names in the format expected by {@link
   * Class#getName(String)}.
   */
  // This regular expression is taken from
  // checker-framework/checker/src/org/checkerframework/checker/signature/qual/ClassGetName.java
  // .  It's a bit too lenient since we don't need to permit arrays and
  // primitives.
  private static final String classGetNameRegex =
      "(^[A-Za-z_][A-Za-z_0-9]*(\\.[A-Za-z_][A-Za-z_0-9]*)*(\\$[A-Za-z_0-9]+)*$)|^\\[+([BCDFIJSZ]|L[A-Za-z_][A-Za-z_0-9]*(\\.[A-Za-z_][A-Za-z_0-9]*)*(\\$[A-Za-z_0-9]+)*;)$";

  private static final Pattern classGetNamePattern;

  static {
    try {
      classGetNamePattern = Pattern.compile(classGetNameRegex);
    } catch (PatternSyntaxException e) {
      // This shouldn't happen because classGetNameRegex is a legal regex
      throw new Error(e);
    }
  }

  public static @MonotonicNonNull File server_dir =
      null; // YOAV: the directory from which we read the dtrace files

  // A PptMap (mapping String -> PptTopLevel) that contains all the program points.
  // Set in mainHelper().
  public static PptMap all_ppts;

  /** current invariant (used for debugging) */
  public static @Nullable Invariant current_inv = null;

  /* List of prototype invariants (one for each type of invariant) */
  public static ArrayList<@Prototype Invariant> proto_invs = new ArrayList<>();

  /** Debug tracer. */
  public static final Logger debugTrace = Logger.getLogger("daikon.Daikon");

  public static final Logger debugProgress = Logger.getLogger("daikon.Progress");

  public static final Logger debugEquality = Logger.getLogger("daikon.Equality");

  /** Debug tracer for ppt initialization. */
  public static final Logger debugInit = Logger.getLogger("daikon.init");

  /** Prints out statistics concerning equality sets, suppressions, etc. */
  public static final Logger debugStats = Logger.getLogger("daikon.stats");

  // Avoid problems if daikon.Runtime is loaded at analysis (rather than
  // test-run) time.  This might have to change when JTrace is used.
  static {
    daikon.Runtime.no_dtrace = true;
  }

  static String usage =
      UtilPlume.joinLines(
          release_string,
          "Daikon invariant detector, copyright 1998-2018",
          // " by Michael Ernst <mernst@cs.washington.edu>",
          "Uses the Java port of GNU getopt, copyright (c) 1998 Aaron M. Renn",
          // "For licensing information, see the License section of the manual.",
          "Usage:",
          "    java daikon.Daikon [flags...] files...",
          "  Each file is a declaration file or a data trace file; the file type",
          "  is determined by the file name (containing \".decls\" or \".dtrace\").",
          "  For a list of flags, see the Daikon manual, which appears in the ",
          "  Daikon distribution and also at http://plse.cs.washington.edu/daikon/.");

  /** Indicates the need for Daikon to terminate. */
  public abstract static class DaikonTerminationException extends RuntimeException {
    static final long serialVersionUID = 20180729L;

    public DaikonTerminationException() {
      super();
    }

    public DaikonTerminationException(String message) {
      super(message);
    }

    public DaikonTerminationException(Throwable e) {
      super(e);
    }

    public DaikonTerminationException(String message, Throwable e) {
      super(message, e);
    }
  }

  /** Indicates that Daikon has terminated normally. */
  public static class NormalTermination extends DaikonTerminationException {
    static final long serialVersionUID = 20180729L;

    public NormalTermination(String message) {
      super(message);
    }

    public NormalTermination() {
      super();
    }
  }

  /**
   * Indicates that Daikon has terminated abnormally, indicating a bug in Daikon. Also used to
   * indicate a situation that should not happen.
   */
  public static class BugInDaikon extends DaikonTerminationException {
    static final long serialVersionUID = 20180729L;

    public BugInDaikon(String message) {
      super(message);
    }

    public BugInDaikon(Throwable e) {
      super(e);
    }

    public BugInDaikon(String message, Throwable e) {
      super(message, e);
    }
  }

  /**
   * Indicates a user error. Thrown to indicate that main should not print a stack trace, but only
   * print the message itself to the user.
   *
   * <p>Code in Daikon should throw other exceptions such as BugInDaikon in cases of a Daikon bug or
   * a system problem (like unpredictable IOExceptions). If the string is null, then this is normal
   * termination, not an error; no message is printed.
   */
  public static class UserError extends DaikonTerminationException {
    static final long serialVersionUID = 20050923L;

    public static String error_at_line_file(LineNumberReader reader, String filename, Throwable e) {
      String msg = e.getMessage();
      if (msg == null) {
        msg = " of type " + e.getClass() + " with no detail message";
      }
      return error_at_line_file(reader, filename, msg);
    }

    public static String error_at_line_file(LineNumberReader reader, String filename, String msg) {
      if (msg == null) {
        throw new Error("Null message supplied to error_at_line_file()");
      }
      return "Error at line " + reader.getLineNumber() + " in file " + filename + ": " + msg;
    }

    /// Constructors that take a Throwable

    // Requires that e.getMessage() != null.
    public UserError(Throwable e) {
      super(e.getMessage() == null ? e.toString() : e.getMessage(), e);
    }

    public UserError(Throwable e, String msg) {
      super(
          ((e.getMessage() != null && msg.contains(e.getMessage()))
              ? msg
              : msg + ": " + e.getMessage()),
          e);
    }

    public UserError(Throwable e, FileIO.ParseState state) {
      this(e, error_at_line_file(state.reader, state.filename, e));
    }

    public UserError(Throwable e, LineNumberReader reader, String filename) {
      this(e, error_at_line_file(reader, filename, e));
    }

    public UserError(Throwable e, String msg, LineNumberReader reader, String filename) {
      this(e, error_at_line_file(reader, filename, msg));
    }

    /// Constructors that do not take a Throwable

    public UserError() {
      super("");
    }

    public UserError(String s) {
      super(s);
    }

    public UserError(String msg, FileIO.ParseState state) {
      super(error_at_line_file(state.reader, state.filename, msg));
    }

    public UserError(String msg, LineNumberReader reader, String filename) {
      super(error_at_line_file(reader, filename, msg));
    }
    // This constructor is too error-prone:  it leads to throwing away
    // subsequent args if there are not enough % directives in the string.
    // public UserError(String format, @Nullable Object... args) {
    //   super (String.format (format, args));
    // }
  }

  /** A parser error that should be reported, with better context, by the caller. */
  public static class ParseError extends Exception {
    static final long serialVersionUID = 20181021L;

    ParseError(String s) {
      super(s);
    }
  }

  /**
   * The arguments to daikon.Daikon are file names. Declaration file names end in ".decls", and data
   * trace file names end in ".dtrace".
   */
  public static void main(final String[] args) {
    try {
      mainHelper(args);
    } catch (DaikonTerminationException e) {
      handleDaikonTerminationException(e);
    }
  }

  /**
   * Handle DaikonTerminationExceptions. Others are left to be handled by the default handler, which
   * prints a more informative stack trace.
   */
  public static void handleDaikonTerminationException(DaikonTerminationException e) {
    if (e instanceof NormalTermination) {
      System.out.println();
      if (e.getMessage() != null) {
        System.out.println(e.getMessage());
      }
      System.exit(0);
    } else if (e instanceof UserError) {
      System.err.println();
      System.err.println(e.getMessage());
      if (Debug.dkconfig_show_stack_trace) {
        e.printStackTrace();
      }
      System.exit(1);
    } else if (e instanceof BugInDaikon) {
      System.err.println();
      System.err.println("Bug in Daikon.  Please report.");
      e.printStackTrace(System.err);
      System.err.println("Bug in Daikon.  Please report.");
      System.exit(2);
    } else {
      // This caes should never be executed.
      System.err.println();
      System.err.println("Bug in Daikon.  Please report.");
      e.printStackTrace(System.err);
      System.err.println("Bug in Daikon.  Please report.");
      System.exit(2);
    }
  }

  /**
   * This does the work of {@link #main}, but it never calls System.exit, so it is appropriate to be
   * called progrmmatically.
   */
  @SuppressWarnings("nullness:contracts.precondition.not.satisfied") // private field
  public static void mainHelper(final String[] args) {
    // Cleanup from any previous runs
    cleanup();

    // Read command line options
    FileOptions files = read_options(args, usage);
    Set<File> decls_files = files.decls;
    Set<String> dtrace_files = files.dtrace;
    Set<File> spinfo_files = files.spinfo;
    Set<File> map_files = files.map;
    if (server_dir == null && (decls_files.size() == 0) && (dtrace_files.size() == 0)) {
      System.out.println("No .decls or .dtrace files specified");
      throw new Daikon.UserError("No .decls or .dtrace files specified");
    }

    // Never disable splitting for csharp format.
    if (Daikon.dkconfig_undo_opts && Daikon.output_format != OutputFormat.CSHARPCONTRACT) {
      PptSplitter.dkconfig_disable_splitting = true;
    }

    if (Daikon.dkconfig_quiet) {
      Daikon.dkconfig_progress_delay = -1;
    }
    if (System.console() == null) {
      // not connected to a terminal
      Daikon.dkconfig_progress_delay = -1;
    }

    // Set up debug traces; note this comes after reading command line options.
    LogHelper.setupLogs(Global.debugAll ? LogHelper.FINE : LogHelper.INFO);

    if (!noversion_output) {
      if (!Daikon.dkconfig_quiet) System.out.println(release_string);
    }

    // figure out which algorithm to use in NIS to process suppressions
    if (NIS.dkconfig_suppression_processor == SuppressionProcessor.HYBRID) {
      NIS.hybrid_method = true;
    } else {
      if (NIS.dkconfig_suppression_processor == SuppressionProcessor.ANTECEDENT) {
        NIS.antecedent_method = true;
        NIS.hybrid_method = false;
      } else {
        assert (NIS.dkconfig_suppression_processor == SuppressionProcessor.FALSIFIED);
        NIS.antecedent_method = false;
        NIS.hybrid_method = false;
      }
    }

    // Create the list of all invariant types
    setup_proto_invs();

    if (PrintInvariants.print_discarded_invariants) {
      DiscReasonMap.initialize();
    }

    fileio_progress = new FileIOProgress();
    fileio_progress.start();

    // Load declarations and splitters
    load_spinfo_files(spinfo_files);
    all_ppts = load_decls_files(decls_files);
    load_map_files(all_ppts, map_files);

    all_ppts.trimToSize();

    // Only for assertion checks
    isInferencing = true;

    // Infer invariants
    process_data(all_ppts, dtrace_files);
    isInferencing = false;
    if (Debug.logOn()) Debug.check(all_ppts, "After process data");

    // If requested, just calculate the total number of invariants possible
    if (dkconfig_calc_possible_invs) {
      fileio_progress.shouldStop = true;
      int total_invs = 0;
      // Can't use new for syntax because the default iterator for all_ppts
      // is not the one I want here.
      for (PptTopLevel ppt : all_ppts.ppt_all_iterable()) {
        System.out.printf("Processing %s with %d variables", ppt.name(), ppt.var_infos.length);
        int inv_cnt = 0;
        if (ppt.var_infos.length > 1600) {
          System.out.println("Skipping, too many variables!");
        } else {
          inv_cnt = ppt.getInvariants().size();
          System.out.println(" " + inv_cnt + " invariants");
          total_invs += inv_cnt;
        }
      }
      System.out.println(total_invs + " invariants total");
      return;
    }

    if (suppress_redundant_invariants_with_simplify) {
      suppressWithSimplify(all_ppts);
    }

    // Check that PptMap created was correct
    all_ppts.repCheck();

    // Remove undesired invariants, if requested
    if (omit_from_output) {
      processOmissions(all_ppts);
    }

    // Write serialized output - must be done before guarding invariants
    if (inv_file != null) {
      try {
        FileIO.write_serialized_pptmap(all_ppts, inv_file);
      } catch (IOException e) {
        throw new RuntimeException("Error while writing .inv file: " + inv_file, e);
      }
    }

    //     if ((Daikon.dkconfig_guardNulls == "always") // interned
    //         || (Daikon.dkconfig_guardNulls == "missing")) { // interned
    //       // This side-effects the PptMap, but it has already been saved
    //       // to disk and is now being used only for printing.
    //       guardInvariants(all_ppts);
    //     }

    // Debug print information about the variables
    if (false) {
      for (PptTopLevel ppt : all_ppts.all_ppts()) {
        System.out.printf("Dumping variables for ppt %s%n", ppt.name());
        for (VarInfo vi : ppt.var_infos) {
          System.out.printf("  vi %s%n", vi);
          System.out.printf("    file_rep_type = %s%n", vi.file_rep_type);
          System.out.printf("    type = %s%n", vi.type);
        }
      }
    }

    // print out the invariants for each program point
    if (Daikon.dkconfig_undo_opts) {
      // Print out the invariants for each program point (sort first)
      for (PptTopLevel ppt : all_ppts.pptIterable()) {

        // We do not need to print out program points that have not seen
        // any samples.
        if (ppt.num_samples() == 0) {
          continue;
        }
        List<Invariant> invs = PrintInvariants.sort_invariant_list(ppt.invariants_vector());
        List<Invariant> filtered_invs = filter_invs(invs);

        // Debugging output.  Sometimes the program points actually differ in number of samples seen
        // due to differences in how Daikon and DaikonSimple see the variable hierarchy.
        if (false) {
          System.out.println("====================================================");
          System.out.println(ppt.name());
          System.out.println(ppt.num_samples());

          for (Invariant inv : filtered_invs) {
            System.out.println(inv.getClass());
            System.out.println(inv);
          }
        }
      }

      // exit the program
      if (false) {
        return;
      }
    }

    // Display invariants
    if (output_num_samples) {
      System.out.println("The --output_num_samples debugging flag is on.");
      System.out.println("Some of the debugging output may only make sense to Daikon programmers.");
    }

    // If they want to see discarded invariants, they probably don't
    // want to see the true ones.
    if (!PrintInvariants.print_discarded_invariants) {
      PrintInvariants.print_invariants(all_ppts);
    } else {
      PrintInvariants.print_reasons(all_ppts);
    }

    if (output_num_samples) {
      Global.output_statistics();
    }
    if (dkconfig_print_sample_totals) {
      System.out.println(FileIO.samples_processed + " samples processed");
    }

    // print statistics concerning what invariants are printed
    if (debugStats.isLoggable(Level.FINE)) {
      for (PptTopLevel ppt : all_ppts.ppt_all_iterable()) {
        PrintInvariants.print_filter_stats(debugStats, ppt, all_ppts);
      }
    }

    // Done
    if (!Daikon.dkconfig_quiet) {
      System.out.println("Exiting Daikon.");
    }
  }

  /** Cleans up static variables so that mainHelper can be called more than once. */
  @SuppressWarnings("nullness") // reinitialization
  public static void cleanup() {

    // Stop the thread that prints out progress information
    if ((fileio_progress != null) && (fileio_progress.getState() != Thread.State.NEW)) {
      fileio_progress.shouldStop = true;
      try {
        fileio_progress.join(2000);
      } catch (InterruptedException e) {
      }
      if (fileio_progress.getState() != Thread.State.TERMINATED) {
        throw new BugInDaikon("Can't stop fileio_progress thead");
      }
    }
    fileio_progress = null;
    progress = "";

    // Reset statics.  Unfortunately, these must match the settings where
    // these are declared and I don't know how to do that automatically.
    inv_file = null;
    no_text_output = false;
    show_progress = false;
    output_format = OutputFormat.DAIKON;
    noversion_output = false;
    output_num_samples = false;
    omit_from_output = false;
    omit_types = new boolean[256];
    use_dataflow_hierarchy = true;
    suppress_redundant_invariants_with_simplify = false;
    ppt_regexp = null;
    ppt_omit_regexp = null;
    var_regexp = null;
    var_omit_regexp = null;
    server_dir = null;
    use_mem_monitor = false;

    proto_invs.clear();
  }

  // Structure for return value of read_options.
  // Return an array of {decls, dtrace, spinfo, map} files.
  public static class FileOptions {
    public Set<File> decls;
    public Set<String> dtrace;
    public Set<File> spinfo;
    public Set<File> map;

    public FileOptions(Set<File> decls, Set<String> dtrace, Set<File> spinfo, Set<File> map) {
      this.decls = decls;
      this.dtrace = dtrace;
      this.spinfo = spinfo;
      this.map = map;
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // Read in the command line options
  // Return {decls, dtrace, spinfo, map} files.
  protected static FileOptions read_options(String[] args, String usage) {
    if (args.length == 0) {
      System.out.println("Error: no files supplied on command line.");
      System.out.println(usage);
      throw new Daikon.UserError();
    }

    // LinkedHashSet because it can be confusing to users if files (of the
    // same type) are gratuitously processed in a different order than they
    // were supplied on the command line.
    HashSet<File> decl_files = new LinkedHashSet<>();
    HashSet<String> dtrace_files = new LinkedHashSet<>(); // file names or "-" or "+"
    HashSet<File> spinfo_files = new LinkedHashSet<>();
    HashSet<File> map_files = new LinkedHashSet<>();

    LongOpt[] longopts =
        new LongOpt[] {
          // Control output
          new LongOpt(help_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(no_text_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(format_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(show_progress_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(no_show_progress_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(noversion_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(output_num_samples_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(files_from_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(omit_from_output_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          // Control invariant detection
          new LongOpt(conf_limit_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(list_type_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(user_defined_invariant_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(disable_all_invariants_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(no_dataflow_hierarchy_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(suppress_redundant_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          // Process only part of the trace file
          new LongOpt(ppt_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(ppt_omit_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(var_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(var_omit_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          // Configuration options
          new LongOpt(server_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(config_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(config_option_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          // Debugging
          new LongOpt(debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(track_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(disc_reason_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(mem_stat_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
        };
    Getopt g = new Getopt("daikon.Daikon", args, "ho:", longopts);
    int c;

    while ((c = g.getopt()) != -1) {
      switch (c) {
        case 0:
          // got a long option
          String option_name = longopts[g.getLongind()].getName();

          // Control output
          if (help_SWITCH.equals(option_name)) {
            System.out.println(usage);
            throw new Daikon.NormalTermination();
          } else if (no_text_output_SWITCH.equals(option_name)) {
            no_text_output = true;
          } else if (format_SWITCH.equals(option_name)) {
            String format_name = getOptarg(g);
            Daikon.output_format = OutputFormat.get(format_name);
            if (Daikon.output_format == null) {
              throw new Daikon.UserError("Unknown output format:  --format " + format_name);
            }
          } else if (show_progress_SWITCH.equals(option_name)) {
            show_progress = true;
            LogHelper.setLevel("daikon.Progress", LogHelper.FINE);
          } else if (no_show_progress_SWITCH.equals(option_name)) {
            show_progress = false;
          } else if (noversion_SWITCH.equals(option_name)) {
            noversion_output = true;
          } else if (output_num_samples_SWITCH.equals(option_name)) {
            output_num_samples = true;
          } else if (files_from_SWITCH.equals(option_name)) {
            String files_from_filename = getOptarg(g);
            try {
              for (String filename : new EntryReader(files_from_filename)) {
                // Ignore blank lines in file.
                if (filename.equals("")) {
                  continue;
                }
                // This code is duplicated below outside the options loop.
                // These aren't "endsWith()" because there might be a suffix
                // on the end (eg, a date, or ".gz").
                File file = new File(filename);
                if (!file.exists()) {
                  throw new Daikon.UserError("File " + filename + " not found.");
                }
                if (filename.indexOf(".decls") != -1) {
                  decl_files.add(file);
                } else if (filename.indexOf(".dtrace") != -1) {
                  dtrace_files.add(filename);
                } else if (filename.indexOf(".spinfo") != -1) {
                  spinfo_files.add(file);
                } else if (filename.indexOf(".map") != -1) {
                  map_files.add(file);
                } else {
                  throw new Daikon.UserError("Unrecognized file extension: " + filename);
                }
              }
            } catch (IOException e) {
              throw new RuntimeException(
                  String.format("Error reading --files_from file: %s", files_from_filename));
            }
            break;
          } else if (omit_from_output_SWITCH.equals(option_name)) {
            String f = getOptarg(g);
            for (int i = 0; i < f.length(); i++) {
              if ("0rs".indexOf(f.charAt(i)) == -1) {
                throw new Daikon.UserError(
                    "omit_from_output flag letter '" + f.charAt(i) + "' is unknown");
              }
              omit_types[f.charAt(i)] = true;
            }
            omit_from_output = true;
          }
          // Control invariant detection
          else if (conf_limit_SWITCH.equals(option_name)) {
            double limit = Double.parseDouble(getOptarg(g));
            if ((limit < 0.0) || (limit > 1.0)) {
              throw new Daikon.UserError(conf_limit_SWITCH + " must be between [0..1]");
            }
            Configuration.getInstance()
                .apply("daikon.inv.Invariant.confidence_limit", String.valueOf(limit));
          } else if (list_type_SWITCH.equals(option_name)) {
            try {
              String list_type_string = getOptarg(g);
              ProglangType.list_implementors.add(list_type_string);
            } catch (Exception e) {
              throw new Daikon.UserError("Problem parsing " + list_type_SWITCH + " option: " + e);
            }
            break;
          } else if (user_defined_invariant_SWITCH.equals(option_name)) {
            try {
              String user_defined_invariant_string = getOptarg(g);
              Matcher m = classGetNamePattern.matcher(user_defined_invariant_string);
              if (!m.matches()) {
                throw new Daikon.UserError(
                    "Bad argument "
                        + user_defined_invariant_string
                        + " for "
                        + ppt_regexp_SWITCH
                        + ": not in the format required by Class.getName(String)");
              }
              @SuppressWarnings("signature") // Regex match guarantees the format of Class.getName()
              @ClassGetName String cname = user_defined_invariant_string;
              userDefinedInvariants.add(cname);
            } catch (Exception e) {
              throw new Daikon.UserError(
                  "Problem parsing " + user_defined_invariant_SWITCH + " option: " + e);
            }
            break;
          } else if (disable_all_invariants_SWITCH.equals(option_name)) {
            // There are two possibilities:
            //  * a given invariant class is not yet loaded, in which case
            //    we set the default value for its dkconfig_enabled field.
            //  * a given invariant class is already loaded, in which case
            //    we reflectively set its dkconfig_enabled to false.

            // Set the default for not-yet-loaded invariants.
            Invariant.invariantEnabledDefault = false;

            // Get all loaded classes.  This solution is from
            // http://stackoverflow.com/a/10261850/173852 .  Alternate approach:
            // http://stackoverflow.com/questions/2548384/java-get-a-list-of-all-classes-loaded-in-the-jvm
            Field f;
            try {
              f = ClassLoader.class.getDeclaredField("classes");
            } catch (NoSuchFieldException e) {
              throw new Daikon.BugInDaikon("Didn't find field ClassLoader.classes");
            }
            f.setAccessible(true);
            Object classesAsObject;
            {
              ClassLoader cl = Thread.currentThread().getContextClassLoader();
              if (cl == null) {
                throw new Daikon.BugInDaikon(
                    "Need to handle when getContextClassLoader returns null");
              }
              try {
                classesAsObject = f.get(cl);
              } catch (IllegalAccessException e) {
                throw new Daikon.BugInDaikon("Field ClassLoader.classes was not made accessible");
              }
            }
            @SuppressWarnings({
              "unchecked", // type of ClassLoader.classes field is known
              "nullness" // ClassLoader.classes is non-null
            })
            @NonNull List<Class<?>> classes = (List<Class<?>>) classesAsObject;
            for (int i = 0; i < classes.size(); i++) {
              Class<?> loadedClass = classes.get(i);
              if (Invariant.class.isAssignableFrom(loadedClass)) {
                @SuppressWarnings("unchecked") // loadedClass is a subclass of Invariant
                Class<? extends Invariant> invType = (Class<? extends Invariant>) loadedClass;
                try {
                  Field field = invType.getField("dkconfig_enabled");
                  if (field.getType() != Boolean.TYPE) {
                    throw new Daikon.BugInDaikon(
                        "Field "
                            + invType
                            + ".dkconfig_enabled has type "
                            + field.getType()
                            + " instead of boolean");
                  } else {
                    setStaticField(field, false);
                    // System.out.println(
                    //     "Set field "
                    //         + invType
                    //         + ".dkconfig_enabled to false");
                  }
                } catch (NoSuchFieldException e) {
                  // System.out.println(
                  //     "Class " + invType + " does not have a dkconfig_enabled field");
                } catch (IllegalAccessException e) {
                  throw new Daikon.BugInDaikon(
                      "IllegalAccessException for field " + invType + ".dkconfig_enabled");
                }
              }
            }
          } else if (no_dataflow_hierarchy_SWITCH.equals(option_name)) {
            use_dataflow_hierarchy = false;
          } else if (suppress_redundant_SWITCH.equals(option_name)) {
            suppress_redundant_invariants_with_simplify = true;
          }

          // Process only part of the trace file
          else if (ppt_regexp_SWITCH.equals(option_name)) {
            if (ppt_regexp != null) {
              throw new Daikon.UserError(
                  "multiple --"
                      + ppt_regexp_SWITCH
                      + " regular expressions supplied on command line");
            }
            String regexp_string = getOptarg(g);
            if (!RegexUtil.isRegex(regexp_string)) {
              throw new Daikon.UserError(
                  "Bad regexp "
                      + regexp_string
                      + " for "
                      + ppt_regexp_SWITCH
                      + ": "
                      + RegexUtil.regexError(regexp_string));
            }
            ppt_regexp = Pattern.compile(regexp_string);
            break;
          } else if (ppt_omit_regexp_SWITCH.equals(option_name)) {
            if (ppt_omit_regexp != null) {
              throw new Daikon.UserError(
                  "multiple --"
                      + ppt_omit_regexp_SWITCH
                      + " regular expressions supplied on command line");
            }
            String regexp_string = getOptarg(g);
            if (!RegexUtil.isRegex(regexp_string)) {
              throw new Daikon.UserError(
                  "Bad regexp "
                      + regexp_string
                      + " for "
                      + ppt_omit_regexp_SWITCH
                      + ": "
                      + RegexUtil.regexError(regexp_string));
            }
            ppt_omit_regexp = Pattern.compile(regexp_string);
            break;
          } else if (var_regexp_SWITCH.equals(option_name)) {
            if (var_regexp != null) {
              throw new Daikon.UserError(
                  "multiple --"
                      + var_regexp_SWITCH
                      + " regular expressions supplied on command line");
            }
            String regexp_string = getOptarg(g);
            if (!RegexUtil.isRegex(regexp_string)) {
              throw new Daikon.UserError(
                  "Bad regexp "
                      + regexp_string
                      + " for "
                      + var_regexp_SWITCH
                      + ": "
                      + RegexUtil.regexError(regexp_string));
            }
            var_regexp = Pattern.compile(regexp_string);
            break;
          } else if (var_omit_regexp_SWITCH.equals(option_name)) {
            if (var_omit_regexp != null) {
              throw new Daikon.UserError(
                  "multiple --"
                      + var_omit_regexp_SWITCH
                      + " regular expressions supplied on command line");
            }
            String regexp_string = getOptarg(g);
            if (!RegexUtil.isRegex(regexp_string)) {
              throw new Daikon.UserError(
                  "Bad regexp "
                      + regexp_string
                      + " for "
                      + var_omit_regexp_SWITCH
                      + ": "
                      + RegexUtil.regexError(regexp_string));
            }
            var_omit_regexp = Pattern.compile(regexp_string);
            break;
          } else if (server_SWITCH.equals(option_name)) {
            String input_dir = getOptarg(g);
            server_dir = new File(input_dir);
            if (!server_dir.isDirectory() || !server_dir.canRead() || !server_dir.canWrite()) {
              throw new RuntimeException(
                  "Could not open config file in server directory " + server_dir);
            }
            break;

            // Configuration options

          } else if (config_SWITCH.equals(option_name)) {
            String config_file = getOptarg(g);
            try {
              InputStream stream = new FileInputStream(config_file);
              Configuration.getInstance().apply(stream);
            } catch (IOException e) {
              throw new Daikon.UserError(
                  // Is this the only possible reason for an IOException?
                  "Could not open config file " + config_file);
            }
            break;
          } else if (config_option_SWITCH.equals(option_name)) {
            String item = getOptarg(g);
            try {
              Configuration.getInstance().apply(item);
            } catch (daikon.config.Configuration.ConfigException e) {
              throw new Daikon.UserError(e);
            }
            break;
          } else if (debugAll_SWITCH.equals(option_name)) {
            Global.debugAll = true;
          } else if (debug_SWITCH.equals(option_name)) {
            LogHelper.setLevel(getOptarg(g), LogHelper.FINE);
          } else if (track_SWITCH.equals(option_name)) {
            LogHelper.setLevel("daikon.Debug", LogHelper.FINE);
            String error = Debug.add_track(getOptarg(g));
            if (error != null) {
              throw new Daikon.UserError(
                  "Error parsing track argument '" + getOptarg(g) + "' - " + error);
            }
          } else if (disc_reason_SWITCH.equals(option_name)) {
            try {
              PrintInvariants.discReasonSetup(getOptarg(g));
            } catch (IllegalArgumentException e) {
              throw new Daikon.UserError(e);
            }
          } else if (mem_stat_SWITCH.equals(option_name)) {
            use_mem_monitor = true;
          } else {
            throw new Daikon.UserError("Unknown option " + option_name + " on command line");
          }
          break;
        case 'h':
          System.out.println(usage);
          throw new Daikon.NormalTermination();
        case 'o':
          String inv_filename = getOptarg(g);

          if (inv_file != null) {
            throw new Daikon.UserError(
                "multiple serialization output files supplied on command line: "
                    + inv_file
                    + " "
                    + inv_filename);
          }

          inv_file = new File(inv_filename);

          if (!UtilPlume.canCreateAndWrite(inv_file)) {
            throw new Daikon.UserError("Cannot write to serialization output file " + inv_file);
          }
          break;
          //
        case '?':
          // break; // getopt() already printed an error
          System.out.println(usage);
          throw new Daikon.NormalTermination();
          //
        default:
          throw new Daikon.BugInDaikon("getopt() returned " + c);
      }
    }

    // This code is duplicated above within the switch processing.
    // First check that all the file names are OK, so we don't do lots of
    // processing only to bail out at the end.
    for (int i = g.getOptind(); i < args.length; i++) {
      String filename = args[i];
      if (filename.equals("-") || filename.equals("+")) {
        dtrace_files.add(filename);
        continue;
      }

      File file = new File(filename);
      if (!file.exists()) {
        throw new Daikon.UserError("File " + file + " not found.");
      }
      filename = file.toString();

      // These aren't "endsWith()" because there might be a suffix on the end
      // (eg, a date or ".gz").
      if (filename.indexOf(".decls") != -1) {
        decl_files.add(file);
      } else if (filename.indexOf(".dtrace") != -1) {
        dtrace_files.add(filename);
        // Always output an invariant file by default, even if none is
        // specified on the command line.
        if (inv_file == null) {
          String basename;
          // This puts the .inv file in the current directory.
          basename = new File(filename).getName();
          // This puts the .inv file in the same directory as the .dtrace file.
          // basename = filename;
          int base_end = basename.indexOf(".dtrace");
          String inv_filename = basename.substring(0, base_end) + ".inv.gz";

          inv_file = new File(inv_filename);
          if (!UtilPlume.canCreateAndWrite(inv_file)) {
            throw new Daikon.UserError("Cannot write to file " + inv_file);
          }
        }
      } else if (filename.indexOf(".spinfo") != -1) {
        spinfo_files.add(file);
      } else if (filename.indexOf(".map") != -1) {
        map_files.add(file);
      } else {
        throw new Daikon.UserError("Unrecognized file type: " + file);
      }
    }

    // Set the fuzzy float comparison ratio.  This needs to be done after
    // any configuration options (which may set the ratio) are processed.
    Global.fuzzy.setRelativeRatio(Invariant.dkconfig_fuzzy_ratio);

    // Setup ppt_max_name based on the specified percentage of ppts to process
    if (dkconfig_ppt_perc != 100) {
      ppt_max_name = setup_ppt_perc(decl_files, dkconfig_ppt_perc);
      System.out.println("Max ppt name = " + ppt_max_name);
    }

    // Validate guardNulls option
    PrintInvariants.validateGuardNulls();

    return new FileOptions(decl_files, dtrace_files, spinfo_files, map_files);
  }

  /**
   * Set a static field to the given value.
   *
   * @param field a field; must be static
   * @param value the value to set the field to
   * @throws IllegalAccessException if {@code field} is enforcing Java language access control and
   *     the underlying field is either inaccessible or final.
   */
  // This method exists to reduce the scope of the warning suppression.
  @SuppressWarnings({
    "nullness:argument.type.incompatible", // field is static, so object may be null
    "interning:argument.type.incompatible" // interning is not necessary for how this method is used
  })
  private static void setStaticField(Field field, Object value) throws IllegalAccessException {
    field.set(null, value);
  }

  /**
   * Just like {@code g.getOptarg()}, but only to be called in circumstances when the programmer
   * knows that the return value is non-null.
   */
  public static String getOptarg(Getopt g) {
    String result = g.getOptarg();
    if (result == null) {
      throw new Error("getOptarg returned null for " + g);
    }
    return result;
  }

  /**
   * Invariants passed on the command line with the {@code --user_defined_invariant} option. A list
   * of class names in the format required by {@link Class#forName(String)}.
   */
  private static List<@ClassGetName String> userDefinedInvariants =
      new ArrayList<@ClassGetName String>();

  /**
   * Creates the list of prototype invariants for all Daikon invariants. New invariants must be
   * added to this list.
   */
  public static void setup_proto_invs() {

    // Unary scalar invariants
    {
      // OneOf (OneOf.java.jpp)
      proto_invs.add(OneOfScalar.get_proto());
      proto_invs.add(OneOfFloat.get_proto());
      proto_invs.add(OneOfString.get_proto());

      // NonZero (NonZero.java.jpp)
      proto_invs.add(NonZero.get_proto());
      proto_invs.add(NonZeroFloat.get_proto());

      proto_invs.add(IsPointer.get_proto());

      // Lower and Upper bound (Bound.java.jpp)
      proto_invs.add(LowerBound.get_proto());
      proto_invs.add(LowerBoundFloat.get_proto());
      proto_invs.add(UpperBound.get_proto());
      proto_invs.add(UpperBoundFloat.get_proto());

      // Modulus and NonModulus (Modulus.java and NonModulus.java)
      proto_invs.add(Modulus.get_proto());
      proto_invs.add(NonModulus.get_proto());

      // Range invariant (Range.java.jpp)
      proto_invs.addAll(RangeInt.get_proto_all());
      proto_invs.addAll(RangeFloat.get_proto_all());

      // Printable String
      proto_invs.add(PrintableString.get_proto());

      // Complete One Of
      proto_invs.add(CompleteOneOfString.get_proto());
      proto_invs.add(CompleteOneOfScalar.get_proto());

      // Positive (x > 0) (Postive.java).  Positive is a sample invariant
      // that is only included as an example.
      // proto_invs.add (Postive.get_proto());
    }

    // Unary sequence invariants
    {
      // OneOf (OneOf.java.jpp)
      proto_invs.add(OneOfSequence.get_proto());
      proto_invs.add(OneOfFloatSequence.get_proto());
      proto_invs.add(OneOfStringSequence.get_proto());
      proto_invs.add(EltOneOf.get_proto());
      proto_invs.add(EltOneOfFloat.get_proto());
      proto_invs.add(EltOneOfString.get_proto());

      // Range invariant (Range.java.jpp)
      proto_invs.addAll(EltRangeInt.get_proto_all());
      proto_invs.addAll(EltRangeFloat.get_proto_all());

      // Sequence Index Comparisons (SeqIndexComparison.java.jpp)
      proto_invs.add(SeqIndexIntEqual.get_proto());
      proto_invs.add(SeqIndexIntNonEqual.get_proto());
      proto_invs.add(SeqIndexIntGreaterEqual.get_proto());
      proto_invs.add(SeqIndexIntGreaterThan.get_proto());
      proto_invs.add(SeqIndexIntLessEqual.get_proto());
      proto_invs.add(SeqIndexIntLessThan.get_proto());
      proto_invs.add(SeqIndexFloatEqual.get_proto());
      proto_invs.add(SeqIndexFloatNonEqual.get_proto());
      proto_invs.add(SeqIndexFloatGreaterEqual.get_proto());
      proto_invs.add(SeqIndexFloatGreaterThan.get_proto());
      proto_invs.add(SeqIndexFloatLessEqual.get_proto());
      proto_invs.add(SeqIndexFloatLessThan.get_proto());

      // foreach i compare a[i] to a[i+1] (EltwiseIntComparisons.java.jpp)
      proto_invs.add(EltwiseIntEqual.get_proto());
      proto_invs.add(EltwiseIntLessEqual.get_proto());
      proto_invs.add(EltwiseIntGreaterEqual.get_proto());
      proto_invs.add(EltwiseIntLessThan.get_proto());
      proto_invs.add(EltwiseIntGreaterThan.get_proto());
      proto_invs.add(EltwiseFloatEqual.get_proto());
      proto_invs.add(EltwiseFloatLessEqual.get_proto());
      proto_invs.add(EltwiseFloatGreaterEqual.get_proto());
      proto_invs.add(EltwiseFloatLessThan.get_proto());
      proto_invs.add(EltwiseFloatGreaterThan.get_proto());

      // EltNonZero (EltNonZero.java.jpp)
      proto_invs.add(EltNonZero.get_proto());
      proto_invs.add(EltNonZeroFloat.get_proto());

      // No Duplicates (NoDuplicates.java.jpp)
      proto_invs.add(NoDuplicates.get_proto());
      proto_invs.add(NoDuplicatesFloat.get_proto());

      // Element bounds (Bound.java.jpp)
      proto_invs.add(EltLowerBound.get_proto());
      proto_invs.add(EltUpperBound.get_proto());
      proto_invs.add(EltLowerBoundFloat.get_proto());
      proto_invs.add(EltUpperBoundFloat.get_proto());

      // CommonSequence (CommonSequence.java.jpp)
      proto_invs.add(CommonSequence.get_proto());
      proto_invs.add(CommonFloatSequence.get_proto());

      // CommonStringSequence (CommonStringSubsequence.java)
      proto_invs.add(CommonStringSequence.get_proto());
    }

    // Binary scalar-scalar invariants
    {
      // Int, Float, String comparisons (from IntComparisons.java.jpp)
      proto_invs.add(IntEqual.get_proto());
      proto_invs.add(IntNonEqual.get_proto());
      proto_invs.add(IntLessThan.get_proto());
      proto_invs.add(IntGreaterThan.get_proto());
      proto_invs.add(IntLessEqual.get_proto());
      proto_invs.add(IntGreaterEqual.get_proto());
      proto_invs.add(FloatEqual.get_proto());
      proto_invs.add(FloatNonEqual.get_proto());
      proto_invs.add(FloatLessThan.get_proto());
      proto_invs.add(FloatGreaterThan.get_proto());
      proto_invs.add(FloatLessEqual.get_proto());
      proto_invs.add(FloatGreaterEqual.get_proto());
      proto_invs.add(StringEqual.get_proto());
      proto_invs.add(StringNonEqual.get_proto());
      proto_invs.add(StringLessThan.get_proto());
      proto_invs.add(StringGreaterThan.get_proto());
      proto_invs.add(StringLessEqual.get_proto());
      proto_invs.add(StringGreaterEqual.get_proto());

      // LinearBinary over integer/float (from LinearBinary.java.jpp)
      proto_invs.add(LinearBinary.get_proto());
      proto_invs.add(LinearBinaryFloat.get_proto());

      // Numeric invariants (from Numeric.java.jpp)
      proto_invs.addAll(NumericInt.get_proto_all());
      proto_invs.addAll(NumericFloat.get_proto_all());

      // Standard binary string invariants
      proto_invs.addAll(StdString.get_proto_all());
    }

    // Binary sequence-sequence invariants
    {
      // Numeric invariants (from Numeric.java.jpp)
      proto_invs.addAll(PairwiseNumericInt.get_proto_all());
      proto_invs.addAll(PairwiseNumericFloat.get_proto_all());

      // Pairwise string invariants (also from Numeric.java.jpp)
      proto_invs.addAll(PairwiseString.get_proto_all());

      // Lexical sequence comparisons (from SeqComparison.java.jpp)
      proto_invs.add(SeqSeqIntEqual.get_proto());
      proto_invs.add(SeqSeqIntLessThan.get_proto());
      proto_invs.add(SeqSeqIntGreaterThan.get_proto());
      proto_invs.add(SeqSeqIntLessEqual.get_proto());
      proto_invs.add(SeqSeqIntGreaterEqual.get_proto());
      proto_invs.add(SeqSeqFloatEqual.get_proto());
      proto_invs.add(SeqSeqFloatLessThan.get_proto());
      proto_invs.add(SeqSeqFloatGreaterThan.get_proto());
      proto_invs.add(SeqSeqFloatLessEqual.get_proto());
      proto_invs.add(SeqSeqFloatGreaterEqual.get_proto());
      proto_invs.add(SeqSeqStringEqual.get_proto());
      proto_invs.add(SeqSeqStringLessThan.get_proto());
      proto_invs.add(SeqSeqStringGreaterThan.get_proto());
      proto_invs.add(SeqSeqStringLessEqual.get_proto());
      proto_invs.add(SeqSeqStringGreaterEqual.get_proto());

      // Pairwise sequence comparisons (from PairwiseIntComparison.java.jpp)
      proto_invs.add(PairwiseIntEqual.get_proto());
      proto_invs.add(PairwiseIntLessThan.get_proto());
      proto_invs.add(PairwiseIntGreaterThan.get_proto());
      proto_invs.add(PairwiseIntLessEqual.get_proto());
      proto_invs.add(PairwiseIntGreaterEqual.get_proto());
      proto_invs.add(PairwiseFloatEqual.get_proto());
      proto_invs.add(PairwiseFloatLessThan.get_proto());
      proto_invs.add(PairwiseFloatGreaterThan.get_proto());
      proto_invs.add(PairwiseFloatLessEqual.get_proto());
      proto_invs.add(PairwiseFloatGreaterEqual.get_proto());
      proto_invs.add(PairwiseStringEqual.get_proto());
      proto_invs.add(PairwiseStringLessThan.get_proto());
      proto_invs.add(PairwiseStringGreaterThan.get_proto());
      proto_invs.add(PairwiseStringLessEqual.get_proto());
      proto_invs.add(PairwiseStringGreaterEqual.get_proto());

      // Array Reverse (from Reverse.java.jpp)
      proto_invs.add(Reverse.get_proto());
      proto_invs.add(ReverseFloat.get_proto());

      // Pairwise Linear Binary (from PairwiseLinearBinary.java.jpp)
      proto_invs.add(PairwiseLinearBinary.get_proto());
      proto_invs.add(PairwiseLinearBinaryFloat.get_proto());

      // Subset and Superset (from SubSet.java.jpp)
      proto_invs.add(SubSet.get_proto());
      proto_invs.add(SuperSet.get_proto());
      proto_invs.add(SubSetFloat.get_proto());
      proto_invs.add(SuperSetFloat.get_proto());

      // Subsequence (from SubSequence.java.jpp)
      proto_invs.add(SubSequence.get_proto());
      proto_invs.add(SubSequenceFloat.get_proto());
      proto_invs.add(SuperSequence.get_proto());
      proto_invs.add(SuperSequenceFloat.get_proto());
    }

    // Binary sequence-scalar invariants
    {
      // Comparison of scalar to each array element (SeqIntComparison.java.jpp)
      proto_invs.add(SeqIntEqual.get_proto());
      proto_invs.add(SeqIntLessThan.get_proto());
      proto_invs.add(SeqIntGreaterThan.get_proto());
      proto_invs.add(SeqIntLessEqual.get_proto());
      proto_invs.add(SeqIntGreaterEqual.get_proto());
      proto_invs.add(SeqFloatEqual.get_proto());
      proto_invs.add(SeqFloatLessThan.get_proto());
      proto_invs.add(SeqFloatGreaterThan.get_proto());
      proto_invs.add(SeqFloatLessEqual.get_proto());
      proto_invs.add(SeqFloatGreaterEqual.get_proto());

      // Scalar is an element of the array (Member.java.jpp)
      proto_invs.add(Member.get_proto());
      proto_invs.add(MemberFloat.get_proto());
      proto_invs.add(MemberString.get_proto());
    }

    // Ternary invariants
    {
      // FunctionBinary (FunctionBinary.java.jpp)
      proto_invs.addAll(FunctionBinary.get_proto_all());
      proto_invs.addAll(FunctionBinaryFloat.get_proto_all());

      // LinearTernary (LinearTernary.java.jpp)
      proto_invs.add(LinearTernary.get_proto());
      proto_invs.add(LinearTernaryFloat.get_proto());
    }

    // User-defined invariants
    for (String invariantClassName : userDefinedInvariants) {
      Class<?> invClass;
      try {
        invClass = Class.forName(invariantClassName);
      } catch (ClassNotFoundException e) {
        throw new Daikon.UserError(
            "Cannot load class "
                + invariantClassName
                + " in "
                + user_defined_invariant_SWITCH
                + " command-line argument; is it on the classpath?");
      }
      Method get_proto_method;
      try {
        get_proto_method = invClass.getMethod("get_proto");
      } catch (NoSuchMethodException e) {
        throw new Daikon.UserError(
            "No get_proto() method in user-defined invariant class " + invariantClassName);
      } catch (SecurityException e) {
        throw new Daikon.UserError(
            e,
            "SecurityException while looking up get_proto() method in user-defined invariant class "
                + invariantClassName);
      }
      Invariant inv;
      try {
        @SuppressWarnings("nullness") // null argument is OK because get_proto_method is static
        Object inv_as_object = get_proto_method.invoke(null);
        if (inv_as_object == null) {
          throw new Daikon.UserError(
              invariantClassName
                  + ".get_proto() returned null but should have returned an Invariant");
        }
        if (!(inv_as_object instanceof Invariant)) {
          Class<?> cls = inv_as_object.getClass();
          throw new Daikon.UserError(
              invariantClassName
                  + ".get_proto() returned object of the wrong type.  It should have been a subclass of invariant, but was "
                  + cls
                  + ": "
                  + inv_as_object);
        }
        inv = (Invariant) inv_as_object;
      } catch (Exception e) {
        throw new Daikon.UserError(
            e, "Exception while invoking " + invariantClassName + ".get_proto()");
      }
      proto_invs.add(inv);
    }

    // Remove any elements that are not enabled
    for (Iterator<@Prototype Invariant> i = proto_invs.iterator(); i.hasNext(); ) {
      @Prototype Invariant inv = i.next();
      assert inv != null;
      if (!inv.enabled()) i.remove();
    }
  }

  /**
   * Creates invariants for upper program points by merging together the invariants from all of the
   * lower points.
   */
  public static void createUpperPpts(PptMap all_ppts) {

    // Process each ppt that doesn't have a parent
    // (mergeInvs is called on a root, and recursively processes children)
    for (PptTopLevel ppt : all_ppts.pptIterable()) {
      // System.out.printf("considering ppt %s parents: %s, children: %s%n",
      //                     ppt.name, ppt.parents, ppt.children);
      if (ppt.parents.size() == 0) {
        ppt.mergeInvs();
      }
    }
  }

  /** Setup splitters. Add orig and derived variables. Recursively call init_ppt on splits. */
  public static void init_ppt(PptTopLevel ppt, PptMap all_ppts) {

    if (!Daikon.using_DaikonSimple) {
      // Create orig variables and set up splitters.
      // This must be done before adding derived variables.
      // Do not add splitters to ppts that were already created by splitters!
      // Also, ppts created by splitters already have their orig_vars.
      if (!(ppt instanceof PptConditional)) {
        progress = "Creating orig variables and splitters for: " + ppt.name;
        create_orig_vars(ppt, all_ppts);
        setup_splitters(ppt);
      }
    }

    // Create derived variables
    if (!Derivation.dkconfig_disable_derived_variables) {
      progress = "Creating derived variables for: " + ppt.name;
      ppt.create_derived_variables();
    }

    if (!Daikon.using_DaikonSimple) {
      // Initialize equality sets on leaf nodes
      setupEquality(ppt);
      // System.out.printf("initialized equality %s for ppt %s%n",
      //                    ppt.equality_view, ppt.name());

      // Recursively initialize ppts created by splitters
      if (ppt.has_splitters()) {
        for (PptConditional ppt_cond : ppt.cond_iterable()) {
          init_ppt(ppt_cond, all_ppts);
        }
      }
    }

    if (VarInfo.assertionsEnabled()) {
      for (VarInfo vi : ppt.var_infos) {
        vi.checkRep();
      }
    }
  }

  /** Create EXIT program points as needed for EXITnn program points. */
  public static void create_combined_exits(PptMap ppts) {

    // We can't add the newly created exit Ppts directly to ppts while we
    // are iterating over it, so store them temporarily in this map.
    PptMap exit_ppts = new PptMap();

    for (PptTopLevel ppt : ppts.pptIterable()) {
      // skip unless it's an EXITnn
      if (!ppt.is_subexit()) {
        continue;
      }

      PptTopLevel exitnn_ppt = ppt;
      PptName exitnn_name = exitnn_ppt.ppt_name;
      PptName exit_name = ppt.ppt_name.makeExit();
      PptTopLevel exit_ppt = exit_ppts.get(exit_name);

      if (debugInit.isLoggable(Level.FINE)) {
        debugInit.fine("create_combined_exits: encountered exit " + exitnn_ppt.name());
      }

      // Create the exit, if necessary
      if (exit_ppt == null) {
        // This is a hack.  It should probably filter out orig and derived
        // vars instead of taking the first n.
        int len = ppt.num_tracevars + ppt.num_static_constant_vars;
        VarInfo[] exit_vars = new VarInfo[len];
        // System.out.printf("new decl fmt = %b%n", FileIO.new_decl_format);
        for (int j = 0; j < len; j++) {
          @SuppressWarnings("interning") // about to be used in new program point
          @Interned VarInfo exit_var = new VarInfo(ppt.var_infos[j]);
          exit_vars[j] = exit_var;
          // System.out.printf("exitNN name '%s', exit name '%s'%n",
          //                   ppt.var_infos[j].name(), exit_vars[j].name());
          exit_vars[j].varinfo_index = ppt.var_infos[j].varinfo_index;
          exit_vars[j].value_index = ppt.var_infos[j].value_index;
          // Don't inherit the entry variable's equalitySet.
          @SuppressWarnings("nullness") // reinitialization
          @NonNull Equality es = null;
          exit_vars[j].equalitySet = es;
        }

        exit_ppt =
            new PptTopLevel(
                exit_name.getName(),
                PptTopLevel.PptType.EXIT,
                ppt.parent_relations,
                ppt.flags,
                exit_vars);

        // exit_ppt.ppt_name.setVisibility(exitnn_name.getVisibility());
        exit_ppts.add(exit_ppt);
        if (debugInit.isLoggable(Level.FINE)) {
          debugInit.fine("create_combined_exits: created exit " + exit_name);
        }
        init_ppt(exit_ppt, ppts);
      }
    }

    // Now add the newly created Ppts to the global map.
    for (PptTopLevel ppt : exit_ppts.pptIterable()) {
      ppts.add(ppt);
    }
  }

  // The function filters out the reflexive invs in binary slices,
  // reflexive and partially reflexive invs in ternary slices
  // and also filters out the invariants that have not seen enough
  // samples in ternary slices.
  static List<Invariant> filter_invs(List<Invariant> invs) {
    List<Invariant> new_list = new ArrayList<>();

    for (Invariant inv : invs) {
      VarInfo[] vars = inv.ppt.var_infos;

      // This check is the most non-intrusive way to filter out the invs
      // Filter out reflexive invariants in the binary invs
      if (!((inv.ppt instanceof PptSlice2) && vars[0] == vars[1])) {

        // Filter out the reflexive and partially reflexive invs in the
        // ternary slices
        if (!((inv.ppt instanceof PptSlice3)
            && (vars[0] == vars[1] || vars[1] == vars[2] || vars[0] == vars[2]))) {
          if (inv.ppt.num_values() != 0) {

            // filters out "warning: too few samples for
            // daikon.inv.ternary.threeScalar.LinearTernary invariant"
            if (inv.isActive()) {
              new_list.add(inv);
            }
          }
        }
      }
    }

    return new_list;
  }

  /**
   * Add orig() variables to the given EXIT/EXITnn point. Does nothing if exit_ppt is not an
   * EXIT/EXITnn.
   */
  private static void create_orig_vars(PptTopLevel exit_ppt, PptMap ppts) {
    if (!exit_ppt.ppt_name.isExitPoint()) {
      if (VarInfo.assertionsEnabled()) {
        for (VarInfo vi : exit_ppt.var_infos) {
          try {
            vi.checkRep();
          } catch (Throwable e) {
            System.err.println("\nError with variable " + vi + " at ppt " + exit_ppt);
            throw e;
          }
        }
      }
      return;
    }

    if (debugInit.isLoggable(Level.FINE)) {
      debugInit.fine("Doing create and relate orig vars for: " + exit_ppt.name());
    }

    PptTopLevel entry_ppt = ppts.get(exit_ppt.ppt_name.makeEnter());
    if (entry_ppt == null) {
      throw new Daikon.UserError("exit found with no corresponding entry: " + exit_ppt.name());
    }

    // Add "orig(...)" (prestate) variables to the program point.
    // Don't bother to include the constants.  Walk through
    // entry_ppt's vars.  For each non-constant, put it on the
    // new_vis worklist after fixing its comparability information.
    exit_ppt.num_orig_vars = entry_ppt.num_tracevars;
    VarInfo[] new_vis = new VarInfo[exit_ppt.num_orig_vars];
    {
      VarInfo[] entry_ppt_vis = entry_ppt.var_infos;
      int new_vis_index = 0;
      for (int k = 0; k < entry_ppt.num_declvars; k++) {
        VarInfo vi = entry_ppt_vis[k];
        assert !vi.isDerived() : "Derived when making orig(): " + vi.name();
        if (vi.isStaticConstant()) {
          continue;
        }
        VarInfo origvar = VarInfo.origVarInfo(vi);
        // Fix comparability
        VarInfo postvar = exit_ppt.find_var_by_name(vi.name());
        if (postvar == null) {
          System.out.printf(
              "Looking for postvar, can't find var %s in exit of ppt %s%n", vi, exit_ppt.name());
          for (VarInfo cvi : entry_ppt.var_infos) {
            System.out.printf("  entry var = %s%n", cvi);
          }
          for (VarInfo cvi : exit_ppt.var_infos) {
            System.out.printf("  exit var = %s%n", cvi);
          }
          throw new RuntimeException("this can't happen: postvar is null");
        }
        origvar.postState = postvar;
        origvar.comparability = postvar.comparability.makeAlias();

        // add parents for override relations
        // exit_ppt.parents has not been loaded at this point
        for (VarParent pi : postvar.parents) {
          PptTopLevel parentppt = ppts.get(pi.parent_ppt);
          if (parentppt != null) {
            if (!parentppt.is_object() && !parentppt.is_class()) {
              VarInfo pvi =
                  parentppt.find_var_by_name(
                      pi.parent_variable == null ? postvar.name() : pi.parent_variable);
              if (pvi != null) {
                origvar.parents.add(
                    new VarParent(pi.parent_ppt, pi.parent_relation_id, pvi.prestate_name()));
              }
            }
          }
        }

        // Add to new_vis
        new_vis[new_vis_index] = origvar;
        new_vis_index++;
        // System.out.printf("adding origvar %s to ppt %s%n", origvar.name(),
        //                   exit_ppt.name());
      }
      assert new_vis_index == exit_ppt.num_orig_vars;
    }
    exit_ppt.addVarInfos(new_vis);

    if (VarInfo.assertionsEnabled()) {
      for (VarInfo vi : exit_ppt.var_infos) {
        try {
          vi.checkRep();
        } catch (Throwable e) {
          System.err.println("\nError with variable " + vi + " at ppt " + exit_ppt);
          throw e;
        }
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // Read decls, dtrace, etc. files

  @RequiresNonNull("fileio_progress")
  // set in mainHelper
  private static PptMap load_decls_files(Set<File> decl_files) {
    long startTime = System.nanoTime();
    try {
      if (!Daikon.dkconfig_quiet) {
        System.out.print("Reading declaration files ");
      }
      PptMap all_ppts = FileIO.read_declaration_files(decl_files);
      if (debugTrace.isLoggable(Level.FINE)) {
        debugTrace.fine("Initializing partial order");
      }
      fileio_progress.clear();
      if (!Daikon.dkconfig_quiet && decl_files.size() > 0) {
        System.out.print("\r(read ");
        System.out.print(UtilPlume.nplural(decl_files.size(), "decls file"));
        System.out.println(")");
      }
      return all_ppts;
    } catch (IOException e) {
      // System.out.println();
      // e.printStackTrace();
      throw new Daikon.UserError(e, "Error parsing decl file");
    } finally {
      long duration = System.nanoTime() - startTime;
      debugProgress.fine(
          "Time spent on read_declaration_files: " + TimeUnit.NANOSECONDS.toSeconds(duration));
    }
  }

  private static void load_spinfo_files(Set<File> spinfo_files) {
    if (PptSplitter.dkconfig_disable_splitting || spinfo_files.isEmpty()) {
      return;
    }
    long startTime = System.nanoTime();
    try {
      System.out.print("Reading splitter info files ");
      create_splitters(spinfo_files);
      System.out.print("\r(read ");
      System.out.print(UtilPlume.nplural(spinfo_files.size(), "spinfo file"));
      System.out.print(", ");
      System.out.print(UtilPlume.nplural(SpinfoFile.numSplittterObjects(spinfoFiles), "splitter"));
      System.out.println(")");
    } catch (IOException e) {
      System.out.println();
      e.printStackTrace();
      throw new Error(e);
    } finally {
      long duration = System.nanoTime() - startTime;
      debugProgress.fine(
          "Time spent on load_spinfo_files: " + TimeUnit.NANOSECONDS.toSeconds(duration));
    }
  }

  private static void load_map_files(PptMap all_ppts, Set<File> map_files) {
    long startTime = System.nanoTime();
    if (!PptSplitter.dkconfig_disable_splitting && map_files.size() > 0) {
      System.out.print("Reading map (context) files ");
      ContextSplitterFactory.load_mapfiles_into_splitterlist(
          map_files, ContextSplitterFactory.dkconfig_granularity);
      System.out.print("\r(read ");
      System.out.print(UtilPlume.nplural(map_files.size(), "map (context) file"));
      System.out.println(")");
      long duration = System.nanoTime() - startTime;
      debugProgress.fine(
          "Time spent on load_map_files: " + TimeUnit.NANOSECONDS.toSeconds(duration));
    }
  }

  /**
   * Sets up splitting on all ppts. Currently only binary splitters over boolean returns or exactly
   * two return statements are enabled by default (though other splitters can be defined by the
   * user).
   */
  @SuppressWarnings("nullness:contracts.precondition.not.satisfied")
  public static void setup_splitters(PptTopLevel ppt) {
    if (PptSplitter.dkconfig_disable_splitting) {
      return;
    }

    Global.debugSplit.fine("<<enter>> setup_splitters");

    SplitterFactory.load_splitters(ppt, spinfoFiles);

    Splitter[] pconds;
    if (SplitterList.dkconfig_all_splitters) {
      pconds = SplitterList.get_all();
    } else {
      pconds = SplitterList.get(ppt.name());
    }
    if (pconds != null) {
      Global.debugSplit.fine(
          "Got " + UtilPlume.nplural(pconds.length, "splitter") + " for " + ppt.name());
      ppt.addConditions(pconds);
    }

    Global.debugSplit.fine("<<exit>>  setup_splitters");
  }

  ///////////////////////////////////////////////////////////////////////////
  // Infer invariants over the trace data

  /**
   * The number of columns of progress information to display. In many Unix shells, this can be set
   * to an appropriate value by {@code --config_option
   * daikon.Daikon.progress_display_width=$COLUMNS}.
   */
  public static int dkconfig_progress_display_width = 80;

  /**
   * Human-friendly progress status message. If {@code fileio_progress} is non-null, then this is
   * ignored. So this is primarily for progress reports that are not IO-related.
   */
  public static String progress = "";

  // Is set unconditionally in mainHelper
  /** Takes precedence over the progress variable. */
  private static @MonotonicNonNull FileIOProgress fileio_progress = null;

  /** Outputs FileIO progress information. Uses global variable FileIO.data_trace_state. */
  public static class FileIOProgress extends Thread {
    public FileIOProgress() {
      setDaemon(true);
      df = DateFormat.getTimeInstance(/*DateFormat.LONG*/ );
    }
    /**
     * Clients should set this variable instead of calling Thread.stop(), which is deprecated.
     * Typically a client calls "display()" before setting this. The stopping happens later, and
     * calls clear() anyway.
     */
    public boolean shouldStop = false;

    private final DateFormat df;

    @Override
    public void run() {
      if (dkconfig_progress_delay == -1) {
        return;
      }
      while (true) {
        if (shouldStop) {
          clear();
          return;
        }
        display();
        try {
          sleep(dkconfig_progress_delay);
        } catch (InterruptedException e) {
          // hmm
        }
      }
    }
    /** Clear the display; good to do before printing to System.out. */
    public void clear() {
      if (dkconfig_progress_delay == -1) {
        return;
      }
      // "display("");" is wrong becuase it leaves the timestamp and writes
      // spaces across the screen.
      String status = UtilPlume.rpad("", dkconfig_progress_display_width - 1);
      System.out.print("\r" + status);
      System.out.print("\r"); // return to beginning of line
      System.out.flush();
    }
    /**
     * Displays the current status. Call this if you don't want to wait until the next automatic
     * display.
     */
    public void display() {
      if (dkconfig_progress_delay == -1) {
        return;
      }

      String message;
      if (FileIO.data_trace_state != null) {
        message = FileIO.data_trace_state.reading_message();
      } else {
        if (Daikon.progress == null) {
          message = "[no status]";
        } else {
          message = Daikon.progress;
        }
      }
      display(message);
    }
    /** Displays the given message. */
    public void display(String message) {
      if (dkconfig_progress_delay == -1) {
        return;
      }
      String status =
          UtilPlume.rpad(
              "[" + df.format(new Date()) + "]: " + message, dkconfig_progress_display_width - 1);
      System.out.print("\r" + status);
      System.out.flush();
      // System.out.println (status); // for debugging

      if (debugTrace.isLoggable(Level.FINE)) {
        debugTrace.fine("Free memory: " + java.lang.Runtime.getRuntime().freeMemory());
        debugTrace.fine(
            "Used memory: "
                + (java.lang.Runtime.getRuntime().totalMemory()
                    - java.lang.Runtime.getRuntime().freeMemory()));
        try {
          if (FileIO.data_trace_state != null) {
            debugTrace.fine("Active slices: " + FileIO.data_trace_state.all_ppts.countSlices());
          }
        } catch (ConcurrentModificationException e) {
          // Because this code is a separate thread, the number of ppts
          // could change during countSlices.  Just ignore and continue.
        }
      }
    }
  }

  /**
   * The data-processing routine of the daikon engine. At this point, the decls and spinfo files
   * have been loaded, all of the program points have been setup, and candidate invariants have been
   * instantiated. This routine processes data to falsify the candidate invariants.
   */
  @SuppressWarnings("nullness:contracts.precondition.not.satisfied") // private field
  @RequiresNonNull("fileio_progress")
  // set in mainHelper
  private static void process_data(PptMap all_ppts, Set<String> dtrace_files) {
    MemMonitor monitor = null;
    if (use_mem_monitor) {
      monitor = new MemMonitor("stat.out");
      new Thread((Runnable) monitor).start();
    }

    long startTime = System.nanoTime();

    // Preprocessing
    setup_NISuppression();

    // Processing (actually using dtrace files)
    try {
      fileio_progress.clear();
      if (!Daikon.dkconfig_quiet) {
        System.out.println(
            "Processing trace data; reading "
                + UtilPlume.nplural(dtrace_files.size(), "dtrace file")
                + ":");
      }
      FileIO.read_data_trace_files(dtrace_files, all_ppts);
      // Final update, so "100%", not "99.70%", is the last thing printed.
      // (This doesn't seem to achieve that, though...)
      fileio_progress.display();
      fileio_progress.shouldStop = true;
      fileio_progress.display();
      if (!Daikon.dkconfig_quiet) {
        System.out.println();
      }
      // System.out.print("Creating implications "); // XXX untested code
      // for (PptTopLevel ppt : all_ppts) {
      //   System.out.print('.');
      //   ppt.addImplications();
      // }
      // System.out.println();
    } catch (IOException e) {
      // System.out.println();
      // e.printStackTrace();
      throw new Error(e);
    } finally {
      long duration = System.nanoTime() - startTime;
      debugProgress.fine(
          "Time spent on read_data_trace_files: " + TimeUnit.NANOSECONDS.toSeconds(duration));
    }

    if (monitor != null) {
      monitor.stop();
    }

    if (FileIO.dkconfig_read_samples_only) {
      throw new Daikon.NormalTermination(
          String.format("Finished reading %d samples", FileIO.samples_processed));
    }

    if (all_ppts.size() == 0) {
      String message = "No program point declarations were found.";
      if (FileIO.omitted_declarations != 0) {
        message +=
            lineSep
                + "  "
                + FileIO.omitted_declarations
                + " "
                + ((FileIO.omitted_declarations == 1) ? "declaration was" : "declarations were")
                + " omitted by regexps (e.g., --ppt-select-pattern).";
      }
      throw new Daikon.UserError(message);
    }

    // System.out.println("samples processed: " + FileIO.samples_processed);

    int unmatched_count = FileIO.call_stack.size() + FileIO.call_hashmap.size();
    if ((use_dataflow_hierarchy && FileIO.samples_processed == unmatched_count)
        || (FileIO.samples_processed == 0)) {
      throw new Daikon.UserError(
          "No samples found for any of " + UtilPlume.nplural(all_ppts.size(), "program point"));
    }

    // ppt_stats (all_ppts);

    //     if (debugStats.isLoggable (Level.FINE)) {
    //       PptSliceEquality.print_equality_stats (debugStats, all_ppts);
    //     }

    // Print equality set info
    //     for (PptTopLevel ppt : all_ppts.pptIterable()) {
    //       System.out.printf("ppt: %s", ppt.name);
    //       if ((ppt.equality_view == null) || (ppt.equality_view.invs == null))
    //       continue;
    //       for (Invariant inv : ppt.equality_view.invs) {
    //       Equality e = (Equality) inv;
    //       System.out.printf("    equality set = %s", e);
    //       }
    //     }

    // System.out.printf("printing ternary invariants");
    // PrintInvariants.print_all_ternary_invs (all_ppts);
    // System.exit(0);

    // Postprocessing

    debugProgress.fine("Create Combined Exits ... ");
    startTime = System.nanoTime();
    create_combined_exits(all_ppts);
    long duration = System.nanoTime() - startTime;
    debugProgress.fine(
        "Create Combined Exits ... done [" + TimeUnit.NANOSECONDS.toSeconds(duration) + "]");

    // Post process dynamic constants
    if (DynamicConstants.dkconfig_use_dynamic_constant_optimization) {
      debugProgress.fine("Constant Post Processing ... ");
      startTime = System.nanoTime();
      for (PptTopLevel ppt : all_ppts.ppt_all_iterable()) {
        if (ppt.constants != null) ppt.constants.post_process();
      }
      duration = System.nanoTime() - startTime;
      debugProgress.fine(
          "Constant Post Processing ... done [" + TimeUnit.NANOSECONDS.toSeconds(duration) + "]");
    }

    // Initialize the partial order hierarchy
    debugProgress.fine("Init Hierarchy ... ");
    startTime = System.nanoTime();
    assert FileIO.new_decl_format != null
        : "@AssumeAssertion(nullness): read data, so new_decl_format is set";
    if (FileIO.new_decl_format) {
      PptRelation.init_hierarchy_new(all_ppts);
    } else {
      PptRelation.init_hierarchy(all_ppts);
    }
    duration = System.nanoTime() - startTime;
    debugProgress.fine(
        "Init Hierarchy ... done [" + TimeUnit.NANOSECONDS.toSeconds(duration) + "]");

    // Calculate invariants at all non-leaf ppts
    if (use_dataflow_hierarchy) {
      debugProgress.fine("createUpperPpts ... ");
      startTime = System.nanoTime();
      // calculates invariants; does not actually create any ppts
      createUpperPpts(all_ppts);
      duration = System.nanoTime() - startTime;
      debugProgress.fine(
          "createUpperPpts ... done [" + TimeUnit.NANOSECONDS.toSeconds(duration) + "]");
    }

    // Equality data for each PptTopLevel.
    if (Daikon.use_equality_optimization && !Daikon.dkconfig_undo_opts) {
      debugProgress.fine("Equality Post Process ... ");
      startTime = System.nanoTime();
      for (PptTopLevel ppt : all_ppts.ppt_all_iterable()) {
        // ppt.equality_view can be null here
        ppt.postProcessEquality();
      }
      duration = System.nanoTime() - startTime;
      debugProgress.fine(
          "Equality Post Process ... done [" + TimeUnit.NANOSECONDS.toSeconds(duration) + "]");
    }

    // undo optimizations; results in a more redundant but more complete
    // set of invariants
    if (Daikon.dkconfig_undo_opts) {
      undoOpts(all_ppts);
    }

    // Debug print information about equality sets
    if (debugEquality.isLoggable(Level.FINE)) {
      for (PptTopLevel ppt : all_ppts.ppt_all_iterable()) {
        debugEquality.fine(ppt.name() + ": " + ppt.equality_sets_txt());
      }
    }

    debugProgress.fine("Non-implication postprocessing ... done");

    isInferencing = false;

    // Add implications
    startTime = System.nanoTime();
    fileio_progress.clear();
    if (!PptSplitter.dkconfig_disable_splitting) {
      debugProgress.fine("Adding Implications ... ");
      for (PptTopLevel ppt : all_ppts.pptIterable()) {
        // debugProgress.fine ("  Adding implications for " + ppt.name);
        ppt.addImplications();
      }
      duration = System.nanoTime() - startTime;
      debugProgress.fine(
          "Time spent adding implications: " + TimeUnit.NANOSECONDS.toSeconds(duration));
    }
  }

  private static class Count {
    public int val;

    Count(int val) {
      this.val = val;
    }
  }

  /** Print out basic statistics (samples, invariants, variables, etc) about each ppt. */
  public static void ppt_stats(PptMap all_ppts) {

    int all_ppt_cnt = 0;
    int ppt_w_sample_cnt = 0;
    for (PptTopLevel ppt : all_ppts.pptIterable()) {
      all_ppt_cnt++;
      if (ppt.num_samples() == 0) {
        continue;
      }
      ppt_w_sample_cnt++;
      System.out.printf("%s%n", ppt.name());
      System.out.printf("  samples    = %n%d", ppt.num_samples());
      System.out.printf("  invariants = %n%d", ppt.invariant_cnt());
      Map<ProglangType, Count> type_map = new LinkedHashMap<>();
      int leader_cnt = 0;
      for (VarInfo v : ppt.var_infos) {
        if (!v.isCanonical()) {
          continue;
        }
        leader_cnt++;
        Count cnt = type_map.get(v.file_rep_type);
        if (cnt == null) type_map.put(v.file_rep_type, cnt = new Count(0));
        cnt.val++;
      }
      System.out.println("  vars       = " + ppt.var_infos.length);
      System.out.println("  leaders    = " + leader_cnt);
      for (Map.Entry<@KeyFor("type_map") ProglangType, Count> e : type_map.entrySet()) {
        ProglangType file_rep_type = e.getKey();
        Count cnt = e.getValue();
        System.out.printf("  %s  = %d%n", file_rep_type, cnt.val);
      }
    }
    System.out.println("Total ppt count     = " + all_ppt_cnt);
    System.out.println("PPts w/sample count = " + ppt_w_sample_cnt);
  }

  /** Process the invariants with simplify to remove redundant invariants. */
  private static void suppressWithSimplify(PptMap all_ppts) {
    System.out.print("Invoking Simplify to identify redundant invariants");
    System.out.flush();
    long startTime = System.nanoTime();
    for (PptTopLevel ppt : all_ppts.ppt_all_iterable()) {
      ppt.mark_implied_via_simplify(all_ppts);
      System.out.print(".");
      System.out.flush();
    }
    long duration = System.nanoTime() - startTime;
    System.out.println(TimeUnit.NANOSECONDS.toSeconds(duration));

    // Make sure the Simplify process and helper threads are finished
    if (PptTopLevel.getProverStack() != null) {
      PptTopLevel.getProverStack().closeSession();
    }
  }

  /** Initialize NIS suppression. */
  public static void setup_NISuppression() {
    NIS.init_ni_suppression();
  }

  /** Initialize the equality sets for each variable. */
  public static void setupEquality(PptTopLevel ppt) {

    if (!Daikon.use_equality_optimization) {
      return;
    }

    // Skip points that are not leaves.
    if (use_dataflow_hierarchy) {
      PptTopLevel p = ppt;
      if (ppt instanceof PptConditional) p = ((PptConditional) ppt).parent;

      // Rather than defining leaves as :::GLOBAL or :::EXIT54 (numbered
      // exit), we define them as everything except
      // ::EXIT (combined), :::ENTER, :::THROWS, :::OBJECT
      //  and :::CLASS program points.  This scheme ensures that arbitrarly
      //  named program points such as :::POINT (used by convertcsv.pl)
      //  will be treated as leaves.
      if (p.ppt_name.isCombinedExitPoint()
          || p.ppt_name.isEnterPoint()
          || p.ppt_name.isThrowsPoint()
          || p.ppt_name.isObjectInstanceSynthetic()
          || p.ppt_name.isClassStaticSynthetic()) {
        return;
      }

      if (ppt.has_splitters()) {
        return;
      }
    }

    // Create the initial equality sets
    ppt.equality_view = new PptSliceEquality(ppt);
    ppt.equality_view.instantiate_invariants();
  }

  private static List<SpinfoFile> spinfoFiles = new ArrayList<>();

  /**
   * Create user-defined splitters. For each file in the input, add a SpinfoFile to the spinfoFiles
   * variable.
   */
  public static void create_splitters(Set<File> spinfo_files) throws IOException {
    for (File filename : spinfo_files) {
      SpinfoFile sf = SplitterFactory.parse_spinfofile(filename);
      spinfoFiles.add(sf);
    }
  }

  //   /**
  //    * Guard the invariants at all PptTopLevels. Note that this changes
  //    * the contents of the PptTopLevels, and the changes made should
  //    * probably not be written out to an inv file (save the file before
  //    * this is called).
  //    */
  //   public static void guardInvariants(PptMap allPpts) {
  //     for (PptTopLevel ppt : allPpts.asCollection()) {
  //       if (ppt.num_samples() == 0)
  //         continue;
  //       // Make sure isDerivedParam is set before guarding.  Otherwise
  //       // we'll never get it correct.
  //       for (int iVarInfo = 0;
  //         iVarInfo < ppt.var_infos.length;
  //         iVarInfo++) {
  //         boolean temp =
  //           ppt.var_infos[iVarInfo].isDerivedParamAndUninteresting();
  //       }
  //
  //       ppt.guardInvariants();
  //     }
  //   }

  /** Removed invariants as specified in omit_types. */
  private static void processOmissions(PptMap allPpts) {
    if (omit_types['0']) allPpts.removeUnsampled();
    for (PptTopLevel ppt : allPpts.asCollection()) {
      ppt.processOmissions(omit_types);
    }
  }

  /**
   * Returns the ppt name, max_ppt, that corresponds to the specified percentage of ppts (presuming
   * that only those ppts &le; max_ppt will be processed).
   */
  private static @Nullable String setup_ppt_perc(Collection<File> decl_files, int ppt_perc) {

    // Make sure the percentage is valid
    if ((ppt_perc < 1) || (ppt_perc > 100)) {
      // The number should already have been checked, so don't use UserError.
      throw new BugInDaikon("ppt_perc of " + ppt_perc + " is out of range 1..100");
    }
    if (ppt_perc == 100) {
      return null;
    }

    // Keep track of all of the ppts in a set ordered by the ppt name
    Set<String> ppts = new TreeSet<>();

    // Read all of the ppt names out of the decl files
    try {
      for (File file : decl_files) {

        // Open the file
        LineNumberReader fp = UtilPlume.lineNumberFileReader(file);

        // Read each ppt name from the file
        for (String line = fp.readLine(); line != null; line = fp.readLine()) {
          if (line.equals("") || FileIO.isComment(line)) {
            continue;
          }
          if (!line.equals("DECLARE")) {
            continue;
          }
          // Just read "DECLARE", so next line has ppt name.
          String ppt_name = fp.readLine();
          if (ppt_name == null) {
            throw new Daikon.UserError("File " + file + " terminated prematurely");
          }
          ppts.add(ppt_name);
        }

        fp.close();
      }
    } catch (IOException e) {
      e.printStackTrace();
      throw new Error(e);
    }

    // Determine the ppt_name that matches the specified percentage.  Always
    // return the last exit point from the method (so we don't get half the
    // exits from a method or enters without exits, etc)
    int ppt_cnt = (ppts.size() * ppt_perc) / 100;
    if (ppt_cnt == 0) {
      throw new Daikon.UserError(
          "ppt_perc of " + ppt_perc + "% results in processing 0 out of " + ppts.size() + " ppts");
    }
    for (Iterator<String> i = ppts.iterator(); i.hasNext(); ) {
      String ppt_name = i.next();
      if (--ppt_cnt <= 0) {
        String last_ppt_name = ppt_name;
        while (i.hasNext()) {
          ppt_name = i.next();
          if ((last_ppt_name.indexOf("EXIT") != -1) && (ppt_name.indexOf("EXIT") == -1)) {
            return last_ppt_name;
          }
          last_ppt_name = ppt_name;
        }
        return ppt_name;
      }
    }
    // Execution should not reach this line
    throw new Error("ppt_cnt " + ppt_cnt + " ppts.size " + ppts.size());
  }

  /**
   * Undoes the invariants suppressed for the dynamic constant, suppression and equality set
   * optimizations (should yield the same invariants as the simple incremental algorithm.
   */
  @SuppressWarnings("flowexpr.parse.error") // private field
  @RequiresNonNull({"NIS.all_suppressions", "NIS.suppressor_map"})
  public static void undoOpts(PptMap all_ppts) {

    // undo suppressions
    for (PptTopLevel ppt : all_ppts.ppt_all_iterable()) {
      NIS.create_suppressed_invs(ppt);
    }

    // undo equality sets
    for (PptTopLevel ppt : all_ppts.ppt_all_iterable()) {
      PptSliceEquality sliceEquality = ppt.equality_view;

      // some program points have no equality sets?
      if (sliceEquality == null) {
        // System.out.println(ppt.name);
        continue;
      }

      // get the new leaders
      List<Equality> allNewInvs = new ArrayList<>();

      for (Invariant eq_as_inv : sliceEquality.invs) {
        Equality eq = (Equality) eq_as_inv;
        VarInfo leader = eq.leader();
        List<VarInfo> vars = new ArrayList<>();

        for (VarInfo var : eq.getVars()) {
          if (!var.equals(leader)) {
            vars.add(var);
          }
        }

        if (vars.size() > 0) {

          // Create new equality sets for all of the non-equal vars
          List<Equality> newInvs = sliceEquality.createEqualityInvs(vars, eq);

          // Create new slices and invariants for each new leader
          // copyInvsFromLeader(sliceEquality, leader, vars);
          sliceEquality.copyInvsFromLeader(leader, vars);

          // Keep track of all of the new invariants created.
          // Add all of the new equality sets to our list
          allNewInvs.addAll(newInvs);
        }
      }

      sliceEquality.invs.addAll(allNewInvs);
    }
  }
}
