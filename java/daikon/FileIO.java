package daikon;

import daikon.derive.ValueAndModified;
import daikon.config.Configuration;

import utilMDE.ArraysMDE;
import utilMDE.Assert;
import utilMDE.Intern;
import utilMDE.UtilMDE;

import org.apache.log4j.Category;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.LineNumberReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.PrintStream;
import java.io.Serializable;
import java.io.StringWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.TreeSet;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

public final class FileIO
{

// We get all the declarations before reading any traces because at each
// program point, we need to know the names of all the available variables,
// which includes all global variables (of which these are examples).
//  * Alternative:  in each trace file, declare all the functions before
//    any data are seen
//     + more reasonable for on-line information (can't know when a
//       new declaration would be seen)
//     + obviates the need for this function and extra pass over the data
//     - forces declaration of all functions ahead of time
//     * perhaps not reasonable (at the least, complicated) for dynamic
//       loading of code; but I should know what I've instrumented and can
//       specify all those declaration files; it isn't possible to instrument
//       as part of dynamic loading
//  * Alternative:  go back and update info to insert new "missing" or
//    "zero" values everywhere that the variables weren't yet known about
//     - tricky for online operation


  /** Nobody should ever instantiate a FileIO. **/
  private FileIO() { throw new Error(); }
  
/// Constants

  final static String comment_prefix = "//";
  final static String declaration_header = "DECLARE";
  // Program point name tags
  public final static String ppt_tag_separator = ":::";
  public final static String enter_suffix = "ENTER";
  public final static String enter_tag = ppt_tag_separator + enter_suffix;
  // EXIT does not necessarily appear at the end of the program point name;
  // a number may follow it.
  public final static String exit_suffix = "EXIT";
  public final static String exit_tag = ppt_tag_separator + exit_suffix;
  public final static String object_suffix = "OBJECT";
  public final static String object_tag = ppt_tag_separator + object_suffix;
  public final static String class_static_suffix = "CLASS";
  public final static String class_static_tag = ppt_tag_separator + class_static_suffix;

/// Settings

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.

  // If true, prints the unmatched procedure entries verbosely
  public static boolean dkconfig_verbose_unmatched_procedure_entries = false;

  // When false, set modbits to 1 iff the printed representation has
  // changed.  When true, set modbits to 1 if the printed
  // representation has changed; leave other modbits as is.
  public static boolean dkconfig_add_changed = true;

/// Variables

  // This hashmap maps every program point to an array, which contains the
  // old values of all variables in scope the last time the program point
  // was executed. This enables us to determine whether the values have been
  // modified since this program point was last executed.
  static HashMap ppt_to_value_reps = new HashMap();

  //for debugging purposes: printing out a modified trace file with changed modbits
  private static boolean to_write_nonce = false;
  private static String nonce_value, nonce_string;

// Logging Categories

  public static final Category debugRead =
    Category.getInstance (FileIO.class.getName() + ".read");
  public static final Category debugPrint =
    Category.getInstance (FileIO.class.getName() + ".printDtrace");

  // Tempoary routine, for debugging
  // Will eventually move into daikon.test.FileIOTest
  public static void main(String[] args)
    throws Exception
  {
    String inf = "daikon/test/fileIOTest.testStackAr";
    String outf = "daikon/test/fileIOTest.testStackAr.goal";
    if (args.length > 0) {
      inf = args[0];
      outf = inf + ".parsed";
    }
    PptMap map = read_declaration_files(Arrays.asList(new File[] { new File(inf) } ));
    dump_ppts(new FileOutputStream(new File(outf)), map);
  }


  
///////////////////////////////////////////////////////////////////////////
/// Declaration files
///

  /** This is intended to be used only interactively, while debugging. */
  static void reset_declarations() {
    throw new Error("to implement");
  }

  /**
   * @param files files to be read (java.io.File)
   * @return a new PptMap containing declarations read from the files
   * listed in the argument; connection information (controlling
   * variables and entry ppts) is set correctly upon return.
   **/
  public static PptMap read_declaration_files(Collection files // [File]
					      )
    throws IOException
  {
    PptMap all_ppts = new PptMap();
    // Read all decls, creating PptTopLevels and VarInfos
    for (Iterator i = files.iterator(); i.hasNext(); ) {
      File file = (File) i.next();
      System.out.print(".");  // show progress
      read_declaration_file(file, all_ppts);
    }
    // Set up EXIT points to control EXITnn points
    setup_combined_exits(all_ppts);
    // Set up OBJECT and CLASS relationships
    setup_object_relations(all_ppts);
    // Set up orig() declarations and relationships
    setup_orig_decls(all_ppts);
    // Set up OBJECT on arguments relationships
    setup_argument_relations(all_ppts);
    return all_ppts;
  }

  /** Read one decls file; add it to all_ppts. **/
  private static void read_declaration_file(File filename,
					    PptMap all_ppts)
    throws IOException
  {
    if (debugRead.isDebugEnabled()) {
      debugRead.debug("read_declaration_file " + filename
		      + ((Daikon.ppt_regexp != null) ? " " + Daikon.ppt_regexp.getPattern() : "")
		      + ((Daikon.ppt_omit_regexp != null) ? " " + Daikon.ppt_omit_regexp.getPattern() : ""));
    }

    int varcomp_format = VarComparability.NONE;

    LineNumberReader reader = UtilMDE.LineNumberFileReader(filename.toString());

    String line = reader.readLine();

    // line == null when we hit end of file
    for ( ; line != null; line = reader.readLine()) {
      if (debugRead.isDebugEnabled())
	debugRead.debug("read_declaration_file line: " + line);
      if (line.equals("") || line.startsWith("//") || line.startsWith("#"))	continue;
      if (line.equals(declaration_header)) {
	PptTopLevel ppt = read_declaration(reader, all_ppts, varcomp_format, filename);
	// ppt can be null if this declaration was skipped because of --ppt or --ppt_omit.
        if (ppt != null) {
          all_ppts.add(ppt);
        }
	continue;
      }
      if (line.equals("VarComparability")) {
        line = reader.readLine();
        if (line.equals("none")) {
          varcomp_format = VarComparability.NONE;
        } else if (line.equals("implicit")) {
          varcomp_format = VarComparability.IMPLICIT;
        } else if (line.equals("explicit")) {
          varcomp_format = VarComparability.EXPLICIT;
        } else {
          throw new IOException("Unrecognized VarComparability: " + line);
        }
        continue;
      }

      // Not a declaration.
      // Read the rest of this entry (until we find a blank line).
      if (debugRead.isDebugEnabled())
	debugRead.debug("Skipping paragraph starting at line " + reader.getLineNumber() + " of file " + filename + ": " + line);
      while ((line != null) && (!line.equals("")) && !line.startsWith("//")) {
	System.out.println("Unrecognized paragraph contains line = `" + line + "'");
	System.out.println("" + (line != null) + " " + (line.equals("")) + " " + !line.startsWith("//"));
	if (line == null)
	  throw new IllegalStateException();
	line = reader.readLine();
      }
      continue;
    }
  }


  // The "DECLARE" line has alredy been read.
  private static PptTopLevel read_declaration(LineNumberReader file,
					      PptMap all_ppts,
					      int varcomp_format,
					      File filename)
    throws IOException
  {
    // We have just read the "DECLARE" line.
    String ppt_name = file.readLine().intern();

    // This program point name has already been encountered.
    if (all_ppts.containsName(ppt_name)) {
      throw new Error("Duplicate declaration of program point " + ppt_name
                      + " found at file " + filename
                      + " line " + file.getLineNumber());
    }

    if (!daikon.split.SplitterList.dkconfig_all_splitters) {
      //if all the splitters are to be tried at all program points,
      //then we need to create all the program points because the
      //creation of splitters requires information from the program
      //points
      if (((Daikon.ppt_omit_regexp != null)
	   && Global.regexp_matcher.contains(ppt_name, Daikon.ppt_omit_regexp))
	  || ((Daikon.ppt_regexp != null)
	      && ! Global.regexp_matcher.contains(ppt_name, Daikon.ppt_regexp))) {
	// Discard this declaration
	// System.out.println("Discarding non-matching program point declaration " + ppt_name);
	String line = file.readLine();
	// This fails if some lines of a declaration (e.g., the comparability
	// field) are empty.
	while ((line != null) && !line.equals("")) {
	  line = file.readLine();
	}
	return null;
      }
    }

    // Each iteration reads a variable name, type, and comparability.
    List var_infos = new ArrayList();
    VarInfo vi;
    while ((vi = read_VarInfo(file, varcomp_format, filename, ppt_name)) != null) {
      for (int i=0; i<var_infos.size(); i++) {
        if (vi.name == ((VarInfo)var_infos.get(i)).name) {
          throw new IOException("Duplicate variable name " + vi.name
				+ " found at file " + filename
				+ " line " + file.getLineNumber());
        }
      }
      // Can't do this test in read_VarInfo, it seems, because of the test
      // against null above.
      if ((Daikon.var_omit_regexp != null)
          && Global.regexp_matcher.contains(vi.name.name(), Daikon.var_omit_regexp)) {
        continue;
      }
      var_infos.add(vi);
    }

    VarInfo[] vi_array = (VarInfo[]) var_infos.toArray(new VarInfo[var_infos.size()]);
    return new PptTopLevel(ppt_name, vi_array);
  }


  /**
   * Read a variable name, type, and comparability; construct a VarInfo.
   * Return null after reading the last variable in this program point
   * declaration.
   **/
  private static VarInfo read_VarInfo(LineNumberReader file,
				      int varcomp_format,
				      File filename,
				      String ppt_name)
    throws IOException
  {
    String line = file.readLine();
    if ((line == null) || (line.equals("")))
      return null;
    String varname = line;
    String proglang_type_string = file.readLine();
    String file_rep_type_string = file.readLine();
    String comparability_string = file.readLine();
    if ((varname == null) || (proglang_type_string == null) || (file_rep_type_string == null) || (comparability_string == null))
      throw new Error("End of file " + filename + " while reading variable " + varname + " in declaration of program point " + ppt_name);
    int equals_index = file_rep_type_string.indexOf(" = ");
    String static_constant_value_string = null;
    Object static_constant_value = null;
    boolean is_static_constant = false;
    if (equals_index != -1) {
      is_static_constant = true;
      static_constant_value_string = file_rep_type_string.substring(equals_index+3);
      file_rep_type_string = file_rep_type_string.substring(0, equals_index);
    }
    // XXX temporary, for compatibility with older .dtrace files.  12/20/2001
    if ("String".equals(file_rep_type_string)) {
      file_rep_type_string = "java.lang.String";
    }
    /// XXX
    ProglangType prog_type = ProglangType.parse(proglang_type_string);
    ProglangType file_rep_type = ProglangType.rep_parse(file_rep_type_string);
    ProglangType rep_type = file_rep_type.fileTypeToRepType();
    if (static_constant_value_string != null) {
      static_constant_value = rep_type.parse_value(static_constant_value_string);
      // Why can't the value be null?
      Assert.assert(static_constant_value != null);
    }
    VarComparability comparability
      = VarComparability.parse(varcomp_format, comparability_string, prog_type);
    // Not a call to Assert.assert in order to avoid doing the (expensive)
    // string concatenations.
    if (! VarInfo.legalFileRepType(file_rep_type)) {
      throw new IOException("Unsupported (file) representation type " +
			    file_rep_type.format() + " for variable " +
			    varname + " at line " + file.getLineNumber() +
			    " of file " + filename);
    }
    if (! VarInfo.legalRepType(rep_type)) {
      throw new IOException("Unsupported (converted) representation type " +
			    file_rep_type.format() + " for variable " +
			    varname + " at line " + file.getLineNumber() +
			    " of file " + filename);
    }

    return new VarInfo(VarInfoName.parse(varname), prog_type, file_rep_type, comparability, is_static_constant, static_constant_value);
  }

  /**
   * For every variable that has the same name in higher and lower,
   * add a link in the po relating them.  See the definitions of lower
   * and higher in VarInfo for their semantics.
   * @see VarInfo.po_higher
   * @see VarInfo.po_lower
   **/
  private static void setup_po_same_name(VarInfo[] lower,
					 VarInfo[] higher)
  {
    setup_po_same_name(lower, VarInfoName.IDENTITY_TRANSFORMER,
		       higher, VarInfoName.IDENTITY_TRANSFORMER);
  }

  /**
   * For every variable that has the same name in higher and lower (after transformation),
   * add a link in the po relating them.  See the definitions of lower
   * and higher in VarInfo for their semantics.
   * @see VarInfo.po_higher
   * @see VarInfo.po_lower
   **/
  private static void setup_po_same_name(VarInfo[] lower,
					 VarInfoName.Transformer lower_xform,
					 VarInfo[] higher,
					 VarInfoName.Transformer higher_xform)
  {
    for (int i=0; i<higher.length; i++) {
      VarInfo higher_vi = higher[i];
      VarInfoName higher_vi_name = higher_xform.transform(higher_vi.name);
      for (int j=0; j<lower.length; j++) {
	VarInfo lower_vi = lower[j];
	VarInfoName lower_vi_name = lower_xform.transform(lower_vi.name);
	if (higher_vi_name == lower_vi_name) { // VarInfoNames are interned
	  lower_vi.addHigherPO(higher_vi);
	}
      }
    }
  }

  /**
   * For each fooENTER point, add a fooEXIT point that contains all of
   * the common variables from the fooEXITnn points.  It is an error
   * if one of the fooEXIT points already exists.
   **/
  public static void setup_combined_exits(PptMap ppts)
  {
    List new_ppts = new ArrayList(); // worklist to avoid concurrent mod exn
    for (Iterator itor = ppts.iterator(); itor.hasNext(); ) {
      PptTopLevel enter_ppt = (PptTopLevel) itor.next();
      if (!enter_ppt.ppt_name.isEnterPoint())
	continue;
      // Construct the combined EXIT name
      PptName exit_name = enter_ppt.ppt_name.makeExit();
      Assert.assert(ppts.get(exit_name) == null);
      // Find all of the EXITnn points
      List exits = new ArrayList();
      for (Iterator it = ppts.iterator(); it.hasNext(); ) {
	PptTopLevel exitnn = (PptTopLevel) it.next();
	if (exitnn.ppt_name.isExitPoint()) {
	  Assert.assert(!exitnn.ppt_name.isCombinedExitPoint());
	  if (exit_name.equals(exitnn.ppt_name.makeExit())) {
	    exits.add(exitnn);
	  }
	}
      }
      // Find common vars in EXITnn points, and make a new ppt from them
      VarInfo[] exit_vars = VarInfo.arrayclone_simple(Ppt.common_vars(exits));
      PptTopLevel exit_ppt = new PptTopLevel(exit_name.getName(), exit_vars);
      // Set up the PO between EXIT and EXITnn
      for (Iterator it = exits.iterator(); it.hasNext(); ) {
	PptTopLevel exitnn = (PptTopLevel) it.next();
	setup_po_same_name(exitnn.var_infos, exit_ppt.var_infos);
      }
      // Put new ppt on worklist to be added once iteration is complete
      new_ppts.add(exit_ppt);
    }
    // Avoid ConcurrentModificationException by adding after the above loop
    for (int i=0; i<new_ppts.size(); i++) {
      ppts.add((PptTopLevel) new_ppts.get(i));
    }
  }

  /**
   * Set up the "controlling program point" partial ordering on
   * VarInfos using the OBJECT, CLASS, ENTER-EXIT program point
   * naming.
   **/
  private static void setup_object_relations(PptMap ppts)
  {
    for (Iterator itor = ppts.iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      PptName ppt_name = ppt.ppt_name;
      // Find the ppt which controlls this one.
      // CLASS controls OBJECT controls ENTER/EXIT.
      PptTopLevel controlling_ppt = null;
      if (ppt_name.isEnterPoint() || ppt_name.isCombinedExitPoint()) {
	// TODO: also require that this is a public method (?)
	controlling_ppt = ppts.get(ppt_name.makeObject());
	if (controlling_ppt == null) {
	  // If we didn't find :::OBJECT, fall back to :::CLASS
	  controlling_ppt = ppts.get(ppt_name.makeClassStatic());
	}
      } else if (ppt_name.isObjectInstanceSynthetic()) {
	controlling_ppt = ppts.get(ppt_name.makeClassStatic());
      }
      // Create VarInfo relations with the controller when names match.
      if (controlling_ppt != null) {
	setup_po_same_name(ppt.var_infos, controlling_ppt.var_infos);
      }
    }
  }

  /**
   * Add orig() variables to all EXIT or EXITnn program points.
   * Should be performed after combined exits and controlling ppts
   * have been added.
   **/
  private static void setup_orig_decls(PptMap ppts)
  {
    for (Iterator itor = ppts.iterator() ; itor.hasNext() ; ) {
      PptTopLevel entry_ppt = (PptTopLevel) itor.next();
      if (! entry_ppt.ppt_name.isEnterPoint()) {
	continue;
      }
      for (Iterator it = ppts.iterator() ; it.hasNext() ; ) {
	PptTopLevel exit_ppt = (PptTopLevel) it.next();
	if (! exit_ppt.ppt_name.isExitPoint()) {
	  continue;
	}
	if (! entry_ppt.ppt_name.equals(exit_ppt.ppt_name.makeEnter())) {
	  continue;
	}
	// comb_exit_ppt may be same as exit_ppt if exit_ppt is EXIT
	PptTopLevel comb_exit_ppt = ppts.get(exit_ppt.ppt_name.makeExit());
	// Add "orig(...)" (prestate) variables to the program point.
	// Don't bother to include the constants.  Walk through
	// entry_ppt's vars.  For each non-constant, put it on the
	// new_vis worklist after fixing its comparability information.
	exit_ppt.num_orig_vars = entry_ppt.var_infos.length - entry_ppt.num_static_constant_vars;
	VarInfo[] new_vis = new VarInfo[exit_ppt.num_orig_vars];
	{
	  VarInfo[] begin_vis = entry_ppt.var_infos;
	  Assert.assert(exit_ppt.num_orig_vars == entry_ppt.num_tracevars);
	  int new_vis_index = 0;
	  for (int i=0; i<begin_vis.length; i++) {
	    VarInfo vi = begin_vis[i];
	    Assert.assert(!vi.isDerived(),"Derived when making orig(): "+vi.name);
	    if (vi.isStaticConstant())
	      continue;
	    VarInfo origvar = VarInfo.origVarInfo(vi);
	    // Fix comparability
	    VarInfo postvar = exit_ppt.findVar(vi.name);
	    Assert.assert(postvar != null,"Exit not superset of entry: "+vi.name);
	    origvar.comparability = postvar.comparability.makeAlias(origvar.name);
	    // Setup PO
	    if (exit_ppt.ppt_name.isCombinedExitPoint()) {
	      origvar.addHigherPO(vi);
	    } else {
	      // (We depend on the fact that we see EXIT before EXITnn
	      // in the iterator (eep!).)
	      VarInfo combvar = comb_exit_ppt.findVar(origvar.name);
	      origvar.addHigherPO(combvar);
	    }
	    // Add to new_vis
	    new_vis[new_vis_index] = origvar;
	    new_vis_index++;
	  }
	  Assert.assert(new_vis_index == exit_ppt.num_orig_vars);
	}
	exit_ppt.addVarInfos(new_vis);
      }
    }
  }

  /**
   * For all ENTER point arguments that have a type that we have an
   * OBJECT ppt for, add the appropriate partial order relation.  This
   * must be done after OBJECT-ENTER controlling relations are already
   * set up.
   **/
  private static void setup_argument_relations(PptMap ppts)
  {
    for (Iterator itor = ppts.iterator() ; itor.hasNext() ; ) {
      PptTopLevel entry_ppt = (PptTopLevel) itor.next();
      if (! entry_ppt.ppt_name.isEnterPoint()) {
	continue;
      }
      // All derived expressions from arguments
      List args = new ArrayList(); // [VarInfo]
      // Subset of above which we have an OBJECT ppt for
      Map known = new HashMap(); // [VarInfo -> PptTopLevel]
      // Search and fill these lists
      VarInfo[] vis = entry_ppt.var_infos;
      for (int i=0; i<vis.length; i++) {
	VarInfo vi = vis[i];
	// Arguments are the things with no controller yet
	if (vi.po_higher.size() == 0) {
	  args.add(vi);
	  if (! vi.type.isPseudoArray()) {
	    PptName objname = new PptName(vi.type.base(), // class
					  null, // method
					  FileIO.object_suffix // point
					  );
	    Assert.assert(objname.isObjectInstanceSynthetic());
	    PptTopLevel object_ppt = ppts.get(objname);
	    if (object_ppt != null) {
	      known.put(vi, object_ppt);
	    }
	  }
	}
      }
      // For each known-type variable, substitute its name in for
      // 'this' in the OBJECT ppt and see if we get any expression
      // matches with other "argument" variables.
      VarInfo[] args_array = (VarInfo[]) args.toArray(new VarInfo[args.size()]);
      for (Iterator it = known.keySet().iterator(); it.hasNext(); ) {
	final VarInfo known_vi = (VarInfo) it.next();
	PptTopLevel object_ppt = (PptTopLevel) known.get(known_vi);
	setup_po_same_name(args_array, // lower
			   VarInfoName.IDENTITY_TRANSFORMER,
			   object_ppt.var_infos, // higher
			   // but with known_vi.name in for "this"
			   new VarInfoName.Transformer() {
			       public VarInfoName transform(VarInfoName v) {
				 return v.replaceAll(VarInfoName.parse("this"),
						     known_vi.name);
			       }
			     }
			   );
      }
    }
  }

  /**
   * @param outstream the stream to send output to
   * @param ppts the program points to dump
   *
   * Writes a textual (debugging) form of the program point hierarchy
   * and relationships to the given stream.
   **/
  public static void dump_ppts(OutputStream outstream,
			       PptMap ppts
			       )
  {
    PrintStream out = new PrintStream(outstream);

    for (Iterator iter = ppts.iterator(); iter.hasNext(); ) {
      Ppt ppt = (Ppt) iter.next();

      out.println(ppt.name);
      for (int i=0; i < ppt.var_infos.length; i++) {
	VarInfo vi = ppt.var_infos[i];
	out.println(vi.name.toString());
	out.println("  Declared type: " + vi.type.format() );
	out.println("  File rep type: " + vi.file_rep_type.format() );
	out.println("  Internal type: " + vi.rep_type.format() );
	out.println("  Comparability: " + vi.comparability );
	out.println("  PO higher:");
	for (Iterator vs = vi.po_higher.iterator(); vs.hasNext(); ) {
	  VarInfo v = (VarInfo) vs.next();
	  out.println("    " + v.name + " in " + v.ppt.name);
	}
	out.println("  PO lower:");
	for (Iterator vs = vi.po_lower.iterator(); vs.hasNext(); ) {
	  VarInfo v = (VarInfo) vs.next();
	  out.println("    " + v.name + " in " + v.ppt.name);
	}
      }
      out.println();

    }
  }

///////////////////////////////////////////////////////////////////////////
/// invocation tracking for dtrace files entry/exit grouping
///

  static final class Invocation {
    String fn_name;		// not interned:  not worth the bother

    // Rather than a valuetuple, place its elements here.
    // ValueTuple value;
    Object[] vals;
    int[] mods;

    Invocation(String fn_name, Object[] vals, int[] mods) {
      Assert.assert(!fn_name.equals(""));
      this.fn_name = fn_name;
      this.vals = vals;
      this.mods = mods;
    }

    // Print the Invocation on two lines, indented by two spaces
    String format() {
      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);

      pw.println("  " + fn_name);
      pw.print("    ");
      for (int j=0; j<vals.length; j++) {
        if (j != 0)
          pw.print(", ");
        Object val = vals[j];
        if (val instanceof int[])
          pw.print(ArraysMDE.toString((int[]) val));
        else if (val instanceof String)
          pw.print((String)val);
        else
          pw.print(val);
      }
      pw.println();

      return sw.toString();
    }

    // Return true if the invocations print the same
    public boolean equals(Object other) {
      if (other instanceof FileIO.Invocation)
        return this.format().equals(((FileIO.Invocation) other).format());
      else
        return false;
    }

    public int hashCode() {
      return this.format().hashCode();
    }
  }

  // call_hashmap is for functions with a nonce to indicate which returns
  // are associated with which entries.
  // call_stack is for functions without nonces.

  // Reading a data trace file first initializes this.  I could save some
  // Object overhead by using two parallel stacks instead of Invocation
  // objects; but that's not worth it.
  static Stack call_stack;		// stack of Invocation objects

  static HashMap call_hashmap; 	// map from Integer to Invocation


// This should call the new version; but as of 1/9/2000, it isn't called at all.
// /// Old version.
// /// This should probably call the new version.
// // I ought to be able to call this individually for each program point,
// // after processing its declarations, right?  Yes for adding _orig
// // variables (but only after reading the corresponding entry program
// // point!), but, not for adding invocation counts, unless I have a way to
// // add new variables to an existing program point.
//
// // I probably *do* want to have original values even for global variables,
// // so I probably want to add original values after globals.
// // What about original vs. final invocation counts?  That could give info
// // about the dynamic call graph.  For now I'll do it, to avoid potential
// // problems with number of orig vars not equaling number of final vars.
//   static void after_processing_file_declarations(PptMap all_ppts) {
//
//     //// Take care not to call this multiple times.  If fn_truevars is set,
//     //// it has been called.  But we need to set all the fn_truevars (for use
//     //// adding _orig) before doing the rest of the work.
//
//     /// Add _orig vars.
//
//     for (Iterator itor = all_ppts.values().iterator() ; itor.hasNext() ; ) {
//       PptTopLevel ppt = (PptTopLevel) itor.next();
//       PptTopLevel entry_ppt = ppt.entry_ppt(all_ppts);
//       if (entry_ppt != null)
// 	entry_ppts.put(ppt, entry_ppt);
//     }
//     for (Iterator itor = entry_ppts.entrySet().iterator() ; itor.hasNext() ; ) {
//       Map.Entry entry = (Map.Entry) itor.next();
//       PptTopLevel exit_ppt = (PptTopLevel) entry.getKey();
//       PptTopLevel entry_ppt = (PptTopLevel) entry.getValue();
//       exit_ppt.add_orig_vars(entry_ppt);
//     }
//
//     // // Add function invocation counts.
//     // Vector fn_names = new HashSet();
//     // for (int i=0; i<all_ppts.size(); i++) {
//     //   String fn_name = ppt.fn_name();
//     //   if (fn_name != null)
//     //     fn_names.add(fn_name);
//     // }
//     // for (int i=0; i<all_ppts.size(); i++) {
//     //   Ppt ppt = all_ppts[i];
//     //   ppt.add_fn_invocation_counts();
//     // }
//
// //     for (Iterator itor = all_ppts.values().iterator() ; itor.hasNext() ; ) {
// //       PptTopLevel ppt = (PptTopLevel) itor.next();
// //       ppt.initial_processing();
// //     }
//
//
//   }


///////////////////////////////////////////////////////////////////////////
/// Data trace files
///


// This is going to manage the orig variables, augmenting variable lists
// before the rest of the system ever sees them.

// This is important, but it's largely user interface,
// so I can get around to it later.
// /**
//  * Read data traces from FILES.
//  * See read_decls_and_traces for more documentation.
//  */
//   void read_data_traces(String[] files, int clear, Pattern fn_regexp, Integer num_files, Long random_seed) {
//     if (clear) {
//       clear_trace_variables();
//     }
//     fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE);
//
//     files_orig = files;
//     if type(files) == types.StringType {
//       files = util.expand_file_name(files);
//       files = glob.glob(files);
//     }
//     if files == [] {
//       raise "No files specified";
//     }
//     if (num_files != null) {
//       int num_files_int = num_files.intValue();
//       if num_files_int == 0 {
// 	raise "Requested 0 of %d files" % len(files);
//       }
//       total_files = len(files);
//       if (num_files_int > total_files) {
// 	raise "Requested %d files, but only %d supplied" % (num_files, total_files);
//       }
//       if (random_seed != null)
// 	Global.random.setSeed(random_seed.longValue());
//       files = util.random_subset(files, num_files);
//       //// Skip this, too wordy.  We can always determine them later.
//       // print num_files, "files randomly chosen:"
//       // for file in files:
//       //     print " ", file
//       assert num_files == None or num_files == len(files);
//     }
//
//     for file in files {
//       read_merge_data_trace_file(file, fn_regexp);
//     }
//
//     if (__debug__) {                       // for loop is outside assert, yuck
//       for fname in fn_var_values.keys() {
// 	assert ((len(fn_var_values[fname].keys()) == 0)
// 		or (len(fn_var_infos[fname]) == len(fn_var_values[fname].keys()[0])));
//       }
//     }
//   }




// An instrumented program produces a .dtrace file containing information about
// run-time values of expressions and variables.  The invariant detector tries
// to find patterns in the values recorded in one or more trace files.
// To detect invariants in a particular program, it is enough to insert code
// in the application which creates a trace file.  In Lisp, the
// `write-to-data-trace' macro and `instrument' function perform this task.
// For documentation of the data trace file format, see dtrace-format.txt.




// All this appears in the Ppt now, and none of this is called as of 1/9/2000.
// It should probably all be deleted.
// // Since I want Ppts to be dumb, fat, and happy, they won't contain any
// // info about number of nonconstant variables, etc.  That all appears here.
//
//   // Additional information useful when reading from a data trace file.
//   // I doubt this will be able to be garbage-collected, unfortunately.
//   static final class PptReadingInfo {
//     int trace_vars;		// number of vars actually in the trace file
//     int[] vi_index;		// map from position in trace file (and thus in
// 				//   ValueTuple) to VarInfo index
//
//     PptReadingInfo(PptTopLevel ppt) {
//       trace_vars = 0;
//       for (int i=0; i<ppt.var_infos.length; i++) {
// 	VarInfo vi = ppt.var_infos[i];
// 	if (! (vi.isStaticConstant() || vi.isDerived())) {
// 	  trace_vars++;
// 	}
//       }
//       vi_index = new int[trace_vars];
//       int trace_var = 0;
//       for (int i=0; i<ppt.var_infos.length; i++) {
// 	VarInfo vi = ppt.var_infos[i];
// 	if (! (vi.isStaticConstant() || vi.isDerived())) {
// 	  vi_index[trace_var] = i;
// 	  trace_var++;
// 	}
//       }
//       Assert.assert(trace_var == trace_vars);
//     }
//   }
//
//   // returns a map from program point name to PptReadingInfo
//   static HashMap getPptReadingInfoMap(PptMap all_ppts) {
//     HashMap result = new HashMap(all_ppts.size());
//
//     for (Iterator itor = all_ppts.entrySet().iterator() ; itor.hasNext() ; ) {
//       Map.Entry entry = (Map.Entry) itor.next();
//       result.put(entry.getKey(), new PptReadingInfo((PptTopLevel) entry.getValue()));
//     }
//     return result;
//   }


  // Eventually add these arguments to this function; they were in the Python
  // version.
  //  *     NUM_FILES indicates how many (randomly chosen) files are to be read;
  //  *       it defaults to all of the files.
  //  *     RANDOM_SEED is a triple of numbers, each in range(0,256), used to
  //  *       initialize the random number generator.

  /**
   * Read data from .dtrace files.
   * Calls @link{read_data_trace_file(String,PptMap,Pattern)} for each
   * element of filenames.
   **/
  static void read_data_trace_files(Collection files, PptMap all_ppts) throws IOException {

    init_call_stack_and_hashmap();

    for (Iterator i = files.iterator(); i.hasNext(); ) {
      System.out.print(".");
      File file = (File) i.next();
      read_data_trace_file(file, all_ppts);
    }

    process_unmatched_procedure_entries();
  }


  // for debugging only.  We stash values here to be examined/printed later.
  static public LineNumberReader data_trace_reader;
  static public File data_trace_filename;

  static void init_call_stack_and_hashmap() {
    call_stack = new Stack();
    call_hashmap = new HashMap();
  }

  /**
   * Read data from .dtrace file.
   **/
  static void read_data_trace_file(File filename, PptMap all_ppts) throws IOException {

    if (debugRead.isDebugEnabled()) {
      debugRead.debug("read_data_trace_file " + filename
		      + ((Daikon.ppt_regexp != null) ? " " + Daikon.ppt_regexp.getPattern() : "")
		      + ((Daikon.ppt_omit_regexp != null) ? " " + Daikon.ppt_omit_regexp.getPattern() : ""));
    }

    LineNumberReader reader = UtilMDE.LineNumberFileReader(filename.toString());
    data_trace_reader = reader;
    data_trace_filename = filename;

    //used for debugging: write new data trace file
    if (Global.debugPrintDtrace) {
      Global.dtraceWriter = new PrintWriter(new FileWriter(new File(filename + ".debug")));
    }
    // init_ftn_call_ct();          // initialize function call counts to 0

    // Maps from a function to the cumulative modification bits seen for
    // the entry since the time other elements were seen.  There is one tag
    // for each exit point associated with this entry.
    HashMap cumulative_modbits = new HashMap();
    for (Iterator itor = all_ppts.iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      PptTopLevel entry_ppt = null; // ppt.entry_ppt; // XXXXXX
      if (entry_ppt != null) {
        int num_vars = entry_ppt.num_vars() - entry_ppt.num_static_constant_vars;
        int[] mods = new int[num_vars];
        Arrays.fill(mods, 0);
        HashMap subhash = (HashMap) cumulative_modbits.get(entry_ppt);
        if (subhash == null) {
          subhash = new HashMap();
          cumulative_modbits.put(entry_ppt, subhash);
        }
        subhash.put(ppt, mods);
      }
    }


    // try {
      // "line_" is uninterned, "line" is interned
      for (String line_ = reader.readLine(); line_ != null; line_ = reader.readLine()) {
        if (line_.equals("") || (line_.startsWith(comment_prefix))) {
          continue;
        }

        String line = line_.intern();

        if ((line == declaration_header)
            || ((Daikon.ppt_omit_regexp != null)
                && Global.regexp_matcher.contains(line, Daikon.ppt_omit_regexp))
            || ((Daikon.ppt_regexp != null)
                && ! Global.regexp_matcher.contains(line, Daikon.ppt_regexp))) {
          // Discard this entire program point information
          // System.out.println("Discarding non-matching dtrace program point " + line);
          while ((line != null) && !line.equals(""))
            line = reader.readLine();
          continue;
        }

        String ppt_name = line; // already interned

        PptTopLevel ppt = (PptTopLevel) all_ppts.get(ppt_name);
        Assert.assert(ppt != null, "Program point " + ppt_name + " appears in dtrace file but not in any decl file");

        VarInfo[] vis = ppt.var_infos;

        // not vis.length, as that includes constants, derived variables, etc.
        // Actually, we do want to leave space for _orig vars.
        // And for the time being (and possibly forever), for derived variables.
        int num_tracevars = ppt.num_tracevars;
        int vals_array_size = ppt.var_infos.length - ppt.num_static_constant_vars;
        Assert.assert(vals_array_size == num_tracevars + ppt.num_orig_vars);

        Object[] vals = new Object[vals_array_size];
        int[] mods = new int[vals_array_size];

	// Read an invocation nonce if one exists
        Integer nonce = null;
        {
          // arbitrary number, hopefully big enough; catch exceptins
          reader.mark(100);
          String nonce_name_maybe;
          try {
            nonce_name_maybe = reader.readLine();
          } catch (Exception e) {
            nonce_name_maybe = null;
          }
          reader.reset();
          if ("this_invocation_nonce".equals(nonce_name_maybe)) {

	      String nonce_name = reader.readLine();
	      Assert.assert(nonce_name.equals("this_invocation_nonce"));
	      nonce = new Integer(reader.readLine());

	      if (Global.debugPrintDtrace) {
		to_write_nonce = true;
		nonce_value = nonce.toString();
		nonce_string = nonce_name_maybe;
	      }
          }
        }

        // Fills up vals and mods arrays by side effect.
        read_vals_and_mods_from_data_trace_file(reader, ppt, vals, mods);

        // Now add some additional variable values that don't appear directly
        // in the data trace file but aren't traditional derived variables.

        add_orig_variables(ppt, cumulative_modbits, vals, mods, nonce);

        // // Add invocation counts
        // if not no_invocation_counts {
        //   for ftn_ppt_name in fn_invocations.keys() {
        //     calls_var_name = "calls(%s)" % ftn_ppt_name;
        //     assert calls_var_name == these_var_infos[current_var_index].name;
        //     these_values.append((fn_invocations[ftn_ppt_name],1))e;
        //     current_var_index++;
        //   }
        // }

        // Add derived variables
        add_derived_variables(ppt, vals, mods);


        vals = Intern.intern(vals);
        Assert.assert(Intern.isInterned(vals));

        // Done adding additional variable values that don't appear directly
        // in the data trace file.

        ValueTuple vt = new ValueTuple(vals, mods);

        if (debugRead.isDebugEnabled()) {
          debugRead.debug("Adding ValueTuple to " + ppt.name);
        }
        ppt.add(vt, 1);

        PptTopLevel exit_ppt = null; // (PptTopLevel) ppt.combined_exit; // XXXXXXXX
        if (exit_ppt != null) {
          VarInfo[] exit_vis = exit_ppt.var_infos;
          // System.out.println("ppt = " + ppt.name);
          // System.out.println(" comb_indices = " + utilMDE.ArraysMDE.toString(ppt.combined_exit_var_indices));
          // System.out.println(" vt = " + vt.toString());
          ValueTuple exit_vt = null; // vt.slice(ppt.combined_exit_var_indices); // XXXXXXX
          exit_ppt.add(exit_vt, 1);
        }

      }
    // }
    // // This catch clause is a bit of a pain.  On the plus side, it gives
    // // line number information in the error message.  On the minus side, it
    // // prevents the debugger from getting to the right frame.  As a
    // // compromise, perhaps eliminate this but have callers use the public
    // // "data_trace_reader" and "data_trace_filename" members.
    // catch (RuntimeException e) {
    //   System.out.println(lineSep + "At " + filename + " line " + reader.getLineNumber() + ":");
    //   e.printStackTrace();
    //   throw e;
    // } catch (Error e) {
    //   System.out.println(lineSep + "At " + filename + " line " + reader.getLineNumber() + ":");
    //   e.printStackTrace();
    //   throw e;
    // }

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.close();
    }

  }


  static void process_unmatched_procedure_entries() {
    if ((!call_stack.empty()) || (!call_hashmap.isEmpty())) {
      System.out.println();
      System.out.println("No return from procedure observed "
			 + (call_stack.size() + call_hashmap.size())
                         + " times.");
      if (!call_hashmap.isEmpty()) {
        System.out.println("Unterminated calls:");
        print_invocations(call_hashmap.values());
      }

      if (!call_stack.empty()) {
        System.out.println("Remaining call stack:");
        print_invocations(call_stack);
      }
      System.out.println("End of report for procedures not returned from.");
    }
  }

  static void print_invocations(Collection invocations) {
    if (dkconfig_verbose_unmatched_procedure_entries) {
      print_invocations_verbose(invocations);
    } else {
      print_invocations_grouped(invocations);
    }
  }

  static void print_invocations_verbose(Collection invocations) {
    for (Iterator i = invocations.iterator(); i.hasNext(); ) {
      Invocation invok = (Invocation) i.next();
      System.out.println(invok.format());
    }
  }

  static void print_invocations_grouped(Collection invocations) {
    // Maps an Invocation to its frequency
    Map counter = new HashMap();

    for (Iterator i = invocations.iterator(); i.hasNext(); ) {
      Invocation invok = (Invocation) i.next();
      if (counter.containsKey(invok)) {
        Integer oldCount = (Integer) counter.get(invok);
        Integer newCount = new Integer(oldCount.intValue() + 1);
        counter.put(invok, newCount);
      } else {
        counter.put(invok, new Integer(1));
      }
    }

    for (Iterator i = counter.keySet().iterator(); i.hasNext(); ) {
      Invocation invok = (Invocation) i.next();
      Integer count = (Integer) counter.get(invok);
      System.out.println(count + " instance" + ((count.intValue() == 1) ? "" : "s") + " of:");
      System.out.println(invok.format());
    }
  }

  // This procedure fills up vals and mods by side effect.
  static void read_vals_and_mods_from_data_trace_file(LineNumberReader reader, PptTopLevel ppt, Object[] vals, int[] mods) throws IOException {
    VarInfo[] vis = ppt.var_infos;
    int num_tracevars = ppt.num_tracevars;


    String[] oldvalue_reps;
    if ( (oldvalue_reps = (String[]) ppt_to_value_reps.get(ppt)) == null) {
      // We've not encountered this program point before.  The nulls in
      // this array will compare non-equal to whatever is in the trace
      // file, which is the desired behavior.
      oldvalue_reps = new String [num_tracevars];
    }

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.println(ppt.name);

      if (to_write_nonce) {
	Global.dtraceWriter.println(nonce_string);
	Global.dtraceWriter.println(nonce_value);
	to_write_nonce = false;
      }
    }

    for (int vi_index=0, val_index=0; val_index<num_tracevars; vi_index++) {
      Assert.assert(vi_index < vis.length
                    // , "Got to vi_index " + vi_index + " after " + val_index + " of " + num_tracevars + " values"
                    );
      VarInfo vi = vis[vi_index];
      Assert.assert((! vi.is_static_constant)
                    || (vi.value_index == -1)
                    // , "Bad value_index " + vi.value_index + " when static_constant_value = " + vi.static_constant_value + " for " + vi.repr() + " at " + ppt_name
                    );
      if (vi.is_static_constant)
        continue;
      Assert.assert(val_index == vi.value_index
                    // , "Differing val_index = " + val_index
                    // + " and vi.value_index = " + vi.value_index
                    // + " for " + vi.name + lineSep + vi.repr()
                    );

      // In errors, say "for program point", not "at program point" as the
      // latter confuses Emacs goto-error.

      String line = reader.readLine();
      if (line == null) {
        throw new Error("Unexpected end of file at " + data_trace_filename + " line " + reader.getLineNumber()
                        + "\n  Expected variable " + vi.name + ", got " + line
                        + " for program point " + ppt.name);
      }

      while ((Daikon.var_omit_regexp != null)
             && (line != null)
             && Global.regexp_matcher.contains(line, Daikon.var_omit_regexp)) {
        line = reader.readLine(); // value
        line = reader.readLine(); // modbit
        line = reader.readLine(); // next variable name
      }

      if (!VarInfoName.parse(line).equals(vi.name)) {
        throw new Error("Expected variable " + vi.name + ", got " + line
                        + " for program point " + ppt.name
                        + " at " + data_trace_filename + " line " + reader.getLineNumber()
                        // + "\n  VarInfo detail: " + vi.repr()
                        );
      }
      line = reader.readLine();
      if (line == null) {
        throw new Error("Unexpected end of file at " + data_trace_filename + " line " + reader.getLineNumber()
                        + "\n  Expected value for variable " + vi.name + ", got " + line
                        + " for program point " + ppt.name);
      }
      String value_rep = line;
      line = reader.readLine();
      if (line == null) {
        throw new Error("Unexpected end of file at " + data_trace_filename + " line " + reader.getLineNumber()
                        + "\n  Expected modbit for variable " + vi.name + ", got " + line
                        + " for program point " + ppt.name);
      }
      if (!((line.equals("0") || line.equals("1") || line.equals("2")))) {
        throw new Error("Bad modbit"
                        + " at " + data_trace_filename + " line " + reader.getLineNumber()
                        + ": " + line);
      }
      String mod_string = line;
      int mod = ValueTuple.parseModified(line);

      // System.out.println("Mod is " + mod + " at " + data_trace_filename + " line " + reader.getLineNumber()
      //                   + "\n  for variable " + vi.name
      //                   + " for program point " + ppt.name);

      if (mod != ValueTuple.MISSING) {
        // Set the modbit now, depending on whether the value of the variable
        // has been changed or not.
        if (value_rep.equals(oldvalue_reps[val_index])) {
          if (!dkconfig_add_changed) {
            mod = ValueTuple.UNMODIFIED;
          }
        } else {
          mod = ValueTuple.MODIFIED;
        }
      }

      mods[val_index] = mod;
      oldvalue_reps[val_index] = value_rep;

      if (Global.debugPrintDtrace) {
	Global.dtraceWriter.println(vi.name);
	Global.dtraceWriter.println(value_rep);
	Global.dtraceWriter.println(mod);
      }

      if (ValueTuple.modIsMissing(mod)) {
        if (!(value_rep.equals("missing") || value_rep.equals("uninit"))) {
          System.out.println("\nModbit indicates missing value for variable " + vi.name + " with value \"" + value_rep + "\";\n  text of value should be \"missing\" or \"uninit\" at " + data_trace_filename + " line " + reader.getLineNumber());
          System.exit(1);
        }
        vals[val_index] = null;
        // vis[val_index].canBeMissing = true; // [[INCR]]
      } else {
        // System.out.println("Mod is " + mod + " (missing=" + ValueTuple.MISSING + "), rep=" + value_rep + " (modIsMissing=" + ValueTuple.modIsMissing(mod) + ")");
        vals[val_index] = vi.rep_type.parse_value(value_rep);
        // Testing, to catch a particular value once upon a time.
        // Assert.assert(! vals[val_index].equals("null"));
      }
      val_index++;

    }

    ppt_to_value_reps.put(ppt, oldvalue_reps);

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.println();
    }

    String blank_line = reader.readLine();
    // Expecting the end of a block of values.
    Assert.assert((blank_line == null) || (blank_line.equals("")), blank_line);
  }


  static void add_orig_variables(PptTopLevel ppt, HashMap cumulative_modbits, Object[] vals, int[] mods, Integer nonce) throws IOException {

    VarInfo[] vis = ppt.var_infos;
    String fn_name = ppt.fn_name();
    String ppt_name = ppt.name;
    if (ppt_name.endsWith(enter_tag)) {
      Invocation invok = new Invocation(fn_name, vals, mods);
      if (nonce == null) {
        call_stack.push(invok);
      } else {
        call_hashmap.put(nonce, invok);
      }
      HashMap subhash = (HashMap) cumulative_modbits.get(ppt);
      // If subhash is null, then there must have been no exit program
      // point that mapped back to this entry.  That could happen if the
      // body is "while (true) { }"; Jikes/dfej adds no synthetic "return"
      // statement in that case.
      // if (subhash == null) {
      //   System.out.println("Entry " + ppt_name + " has no cumulative_modbits");
      // }
      if (subhash != null) {
        // System.out.println("Entry " + ppt_name + " has " + subhash.size() + " exits");
        for (Iterator itor = subhash.values().iterator(); itor.hasNext(); ) {
          int[] exitmods = (int[]) itor.next();
          // System.out.println("lengths: " + exitmods.length + " " + mods.length);
          ValueTuple.orModsInto(exitmods, mods);
        }
      }
    } else {
      PptTopLevel entry_ppt = null; // (PptTopLevel) ppt.entry_ppt; // XXXXXXXX
      if (entry_ppt != null) {
        Invocation invoc;
        // Set invoc
        if (nonce == null) {
          if (call_stack.empty()) {
            throw new Error("Function exit without corresponding entry: "
                            + ppt.name);
          }
          invoc = (Invocation) call_stack.pop();
          while (invoc.fn_name != fn_name) {
            // Should also mark as a function that made an exceptional exit
            // at runtime.
            System.err.println("Exceptional exit from function " + fn_name
                               + ", expected to first exit from " + invoc.fn_name
                               + "; at " + data_trace_filename + " line " + data_trace_reader.getLineNumber());
            invoc = (Invocation) call_stack.pop();
          }
        } else {
          if (! call_hashmap.containsKey(nonce)) {
            throw new Error("Didn't find call to " + ppt.name + " with nonce " + nonce);
          }
          invoc = (Invocation) call_hashmap.get(nonce);
          call_hashmap.remove(nonce);
        }
        Assert.assert(ppt.num_orig_vars == entry_ppt.num_tracevars
                      // , ppt.name + " has " + ppt.num_orig_vars + " orig_vars, but " + entry_ppt.name + " has " + entry_ppt.num_tracevars + " tracevars"
                      );
        int[] entrymods = (int[]) ((HashMap)cumulative_modbits.get(entry_ppt)).get(ppt);
        for (int i=0; i<ppt.num_orig_vars; i++) {
          vals[ppt.num_tracevars+i] = invoc.vals[i];
          int mod = invoc.mods[i];
          if ((mod == ValueTuple.UNMODIFIED)
              && (entrymods[i] == ValueTuple.MODIFIED)) {
            // System.out.println("Entrymods made a difference.");
            mod = ValueTuple.MODIFIED;
          }
          mods[ppt.num_tracevars+i] = mod;
          // Possibly more efficient to set this all at once, late in
          // the game; but this gets it done.
          if (ValueTuple.modIsMissing(mods[ppt.num_tracevars+i])) {
            // vis[ppt.num_tracevars+i].canBeMissing = true; // [[INCR]]
            Assert.assert(vals[ppt.num_tracevars+i] == null);
          }
        }
        Arrays.fill(entrymods, 0);

      }
    }

  }

  // Add derived variables
  static void add_derived_variables(PptTopLevel ppt, Object[] vals, int[] mods) throws IOException {
    // This ValueTuple is temporary:  we're temporarily suppressing interning,
    // which we will do after we have all the values available.
    ValueTuple partial_vt = ValueTuple.makeUninterned(vals, mods);
    int filled_slots = ppt.num_orig_vars+ppt.num_tracevars+ppt.num_static_constant_vars;
    for (int i=0; i<filled_slots; i++) {
      Assert.assert(!ppt.var_infos[i].isDerived());
    }
    for (int i=filled_slots; i<ppt.var_infos.length; i++) {
      Assert.assert(ppt.var_infos[i].isDerived(),
                    "variable not derived: " + ppt.var_infos[i].repr());
    }
    for (int i=filled_slots; i<ppt.var_infos.length; i++) {
      // Add this derived variable's value
      ValueAndModified vm = ppt.var_infos[i].derived.computeValueAndModified(partial_vt);
      vals[i] = vm.value;
      mods[i] = vm.modified;
    }
  }


///////////////////////////////////////////////////////////////////////////
/// Serialized PptMap files
///

  /**
   * Use a special record type.  Saving as one object allows for
   * reference-sharing, easier saves and loads, and potential for
   * later overriding of SerialFormat.readObject if the save format
   * changes (ick).
   **/
  private final static class SerialFormat implements Serializable
  {
    public SerialFormat(PptMap map, Configuration config)
    {
      this.map = map;
      this.config = config;
    }
    public PptMap map;
    public Configuration config;
  }

  public static void write_serialized_pptmap(PptMap map, File file)
    throws IOException
  {
    SerialFormat record = new SerialFormat(map, Configuration.getInstance());
    OutputStream bytes = new FileOutputStream(file);
    if (file.getName().endsWith(".gz")) {
      bytes = new GZIPOutputStream(bytes);
    }
    ObjectOutputStream objs = new ObjectOutputStream(bytes);
    objs.writeObject(record);
    objs.close();
  }

  public static PptMap read_serialized_pptmap(File file, boolean use_saved_config)
    throws IOException
  {
    try {
      InputStream istream = new FileInputStream(file);
      if (file.getName().endsWith(".gz")) {
	istream = new GZIPInputStream(istream);
      }
      ObjectInputStream objs = new ObjectInputStream(istream);
      SerialFormat record = (SerialFormat) objs.readObject();
      if (use_saved_config) {
	Configuration.getInstance().overlap(record.config);
      }
      return record.map;
    } catch (ClassNotFoundException e) {
      throw new IOException("Error while loading inv file: " + e);
    }
    // } catch (InvalidClassException e) {    // already extends IOException
    // } catch (StreamCorruptedException e) { // already extends IOException
    // } catch (OptionalDataException e) {    // already extends IOException
  }


}
