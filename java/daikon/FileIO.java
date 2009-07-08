package daikon;

import daikon.derive.ValueAndModified;
import daikon.config.Configuration;
import daikon.diff.InvMap;
import daikon.inv.Invariant;
import static daikon.PptRelation.PptRelationType;
import static daikon.PptTopLevel.PptFlags;
import static daikon.PptTopLevel.PptType;
import static daikon.VarInfo.RefType;
import static daikon.VarInfo.VarKind;
import static daikon.VarInfo.VarFlags;
import static daikon.VarInfo.LangFlags;

import utilMDE.*;

import java.io.*;
import java.net.*;
import java.text.*;
import java.util.*;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.zip.*;

public final class FileIO {

  /** Nobody should ever instantiate a FileIO. **/
  private FileIO() {
    throw new Error();
  }

  /// Constants

  static final String declaration_header = "DECLARE";

  // Program point name tags
  public static final String ppt_tag_separator = ":::";
  public static final String enter_suffix = "ENTER";
  public static final String enter_tag = ppt_tag_separator + enter_suffix;
  // EXIT does not necessarily appear at the end of the program point name;
  // a number may follow it.
  public static final String exit_suffix = "EXIT";
  public static final String exit_tag = ppt_tag_separator + exit_suffix;
  public static final String throws_suffix = "THROWS";
  public static final String throws_tag = ppt_tag_separator + throws_suffix;
  public static final String object_suffix = "OBJECT";
  public static final String object_tag = ppt_tag_separator + object_suffix;
  public static final String class_static_suffix = "CLASS";
  public static final String class_static_tag = ppt_tag_separator
                                                        + class_static_suffix;
  public static final String global_suffix = "GLOBAL";

  private static final String lineSep = Global.lineSep;


  /// Settings

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.

  /**
   * When true, just ignore exit ppts that don't have a matching enter
   * ppt rather than exiting with an error.  Unmatched exits can occur
   * if only a portion of a dtrace file is processed
   */
  public static boolean dkconfig_ignore_missing_enter = false;

  /**
   * Boolean.  When false, set modbits to 1 iff the printed
   * representation has changed.  When true, set modbits to 1 if the
   * printed representation has changed; leave other modbits as is.
   **/
  public static boolean dkconfig_add_changed = true;

  /**
   * Integer.  Maximum number of lines to read from the dtrace file.  If
   * 0, reads the entire file.
   */
  public static int dkconfig_max_line_number = 0;

  /**
   * Boolean. When false, don't count the number of lines in the dtrace file
   * before reading.  This will disable the percentage progress printout.
   */
  public static boolean dkconfig_count_lines = true;

  /**
   * Boolean.  When true, only read the samples, but don't process them.
   * Used to gather timing information.
   */
  public static boolean dkconfig_read_samples_only = false;

  /** Boolean.  When true, don't print a warning about unmatched procedure
   * entries, which are ignored by Daikon (unless the --nohierarchy switch
   * is provided).
   **/
  public static boolean dkconfig_unmatched_procedure_entries_quiet = false;

  /**
   * Boolean.  If true, prints the unmatched procedure entries
   * verbosely.
   **/
  public static boolean dkconfig_verbose_unmatched_procedure_entries = false;

  /**
   * Boolean.  When true, suppress exceptions related to file reading.
   * This permits Daikon to continue even if there is a malformed trace
   * file.  Use this with care:  in general, it is better to fix the
   * problem that caused a bad trace file, rather than to suppress the
   * exception.
   **/
  public static boolean dkconfig_continue_after_file_exception = false;

  /**
   * Long integer. If non-zero, this value will be used as the number
   * of lines in (each) dtrace file input for the purposes of the
   * progress display, and the counting of the lines in the file will
   * be suppressed.
   */
  public static long dkconfig_dtrace_line_count = 0;

  /**
   * If true, check all of the basic blocks that make up a function
   * to ensure that there is a path from function entry to the block
   */
  public static boolean dkconfig_check_bb_connections = true;

  /** True if declaration records are in the new format **/
  public static /*@LazyNonNull*/ Boolean new_decl_format = null;

  /** Input language as read from the declaration file**/
  public static String input_lang = null;

  /// Variables

  // This hashmap maps every program point to an array, which contains the
  // old values of all variables in scope the last time the program point
  // was executed. This enables us to determine whether the values have been
  // modified since this program point was last executed.
  static HashMap<PptTopLevel,String[]> ppt_to_value_reps = new HashMap<PptTopLevel,String[]>();

  // For debugging purposes: printing out a modified trace file with
  // changed modbits.
  private static boolean to_write_nonce = false;
  private static final String NONCE_HEADER = "this_invocation_nonce";
  private static String nonce_value;

  // (This implementation as a public static variable is a bit unclean.)
  // Number of ignored declarations.
  public static int omitted_declarations = 0;

  /**
   * Map from function id to list of program points in that function.
   * Valid for basic block program points only.
   **/
  private static Map<String,List<PptTopLevel>> func_ppts
    = new LinkedHashMap<String,List<PptTopLevel>>();

  /**
   * If true, variables from basic blocks which predominate a basic block X
   * will be included when X is processed.  This allows Daikon to find
   * invariants between variables in different program points (basic blocks
   * in this case)
   */
  public static boolean dkconfig_merge_basic_blocks = false;

  // Logging Categories

  /** true prints info about variables marked as missing/nonsensical **/
  public static boolean debug_missing = false;

  /** Debug tracer for reading. **/
  public static final Logger debugRead = Logger.getLogger("daikon.FileIO.read");
  /** Debug tracer for printing. **/
  public static final Logger debugPrint =
    Logger.getLogger("daikon.FileIO.printDtrace");

  /** Debug tracer for printing variable values. **/
  public static final Logger debugVars = Logger.getLogger("daikon.FileIO.vars");

  public static final SimpleLog debug_decl = new SimpleLog(false);

  /** Errors while processing ppt declarations */
  public static class DeclError extends IOException {

    static final long serialVersionUID = 20060518L;

    public DeclError (String msg) {
      super (msg);
    }

    public static DeclError detail (ParseState state, String format,
                                    /*@Nullable*/ Object... args) {
      String msg = String.format (format, args)
        + state.line_file_message();
      return new DeclError (msg);
    }
  }

  /**
   * Parents in the ppt/variable hierarchy for a particular program point
   */
  static final class ParentRelation implements java.io.Serializable {
    static final long serialVersionUID = 20060622L;
    PptRelationType rel_type;
    /*@Interned*/ String parent_ppt_name;
    int id;
    public String toString() { return parent_ppt_name + "[" + id + "] "
                                 + rel_type; };
  }

  // Utilities
  public static final boolean isComment(/*@Nullable*/ String s) {
    return s != null && (s.startsWith("//") || s.startsWith("#"));
  }

  public static final boolean nextLineIsComment(BufferedReader reader) {
    boolean result = false;
    try {
      reader.mark(100);
      String nextline = reader.readLine();
      result = isComment(nextline);
    } catch (IOException e) {
      result = false;
    } finally {
      try {
        reader.reset();
      } catch (IOException e) {
        throw new Error(e);
      }
    }
    return result;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Declaration files
  ///

  /**
   * @param files files to be read (java.io.File)
   * @return a new PptMap containing declarations read from the files
   * listed in the argument; connection information (controlling
   * variables and entry ppts) is set correctly upon return.
   **/
  public static PptMap read_declaration_files(Collection<File> files) throws IOException {
    PptMap all_ppts = new PptMap();
    // Read all decls, creating PptTopLevels and VarInfos
    for (File file : files) {
      Daikon.progress = "Reading " + file;
      if (!Daikon.dkconfig_quiet) {
        System.out.print("."); // show progress
      }
      read_declaration_file(file, all_ppts);
    }
    return all_ppts;
  }

  /** Read one decls file; add it to all_ppts. **/
  public static void read_declaration_file(File filename, PptMap all_ppts)
    throws IOException {
    if (Daikon.using_DaikonSimple) {
      Processor processor = new DaikonSimple.SimpleProcessor();
      read_data_trace_file(filename.toString(), all_ppts, processor, true,
                           false);
    } else {
      Processor processor = new Processor();
      read_data_trace_file(filename.toString(), all_ppts, processor, true,
                           true);
    }
  }

    /** Writes every declaration string as one byte length and
        a byte sequence of characters.
        Records a byte -1 to denote the end of the declaration. **/
    public static void write_binaryDeclaration( PrintWriter declWriter,
						DataOutputStream binWriter, ParseState state ) {

      echoMsg( declWriter, "" );
      int index = 0;
      List<String> identifiers = new ArrayList<String>( );

      PptTopLevel ppt = state.ppt;
      String pptName = ppt.ppt_name.getName( );
      identifiers.add( "ppt" );
      identifiers.add( pptName );

      if ( !pptNamesMap.containsKey( pptName ) ) {
        pptNamesIndex++;
        pptNamesMap.put( pptName, pptNamesIndex );
      }
      identifiers.add( "ppt-type" );
      identifiers.add( ppt.type.toString( ).toLowerCase( ) );

      declWriter.println( "ppt " +  ppt.ppt_name.name( ).replaceAll( " ", "\\\\_" ) );
      declWriter.println( "ppt-type " + ppt.type.toString( ).toLowerCase( ) );

      if ( !ppt.flags.isEmpty( ) ) {
        String fls = "";
        identifiers.add( "flags" );
        identifiers.add( "" + ppt.flags.size( ) );

        for ( PptFlags flag : ppt.flags ) {
          identifiers.add( flag.toString( ).toLowerCase( ) );
          fls += flag.toString( ).toLowerCase( ) + " ";
        }
        declWriter.println( "flags " + fls.trim( ) );
      }

      if ( ppt.parent_relations != null ) {

        for ( ParentRelation pr : ppt.parent_relations ) {
          if ( pr != null ) {
            identifiers.add( "parent" );

            String rts = pr.rel_type.toString( );
            identifiers.add( rts.toLowerCase( ) );

            identifiers.add( pr.parent_ppt_name );
            identifiers.add( Integer.toString( pr.id ) );

            declWriter.println( "parent " + rts.toLowerCase( )
                                + " " + pr.parent_ppt_name + " " + pr.id );
          }
        }
      }

      if ( ppt.ppt_successors != null ) {
        if ( !ppt.ppt_successors.isEmpty( ) ) {
          identifiers.add( "ppt-successors" );
          identifiers.add( Integer.toString( ppt.ppt_successors.size( ) ) );
        }

        for ( String sc : ppt.ppt_successors ) {
          if ( sc != null ) {
            declWriter.print( " " + sc );
            identifiers.add( sc );
          }
        }
        declWriter.println( );
      }

      if ( ppt.function_id != null ) {
        identifiers.add( "ppt-func" );
        identifiers.add( ppt.function_id );

        declWriter.println( "ppt-func " + ppt.function_id );
      }

      if ( ppt.bb_length != 0 ) {
        identifiers.add( "ppt-length" );
        identifiers.add( Integer.toString( ppt.bb_length ) );

        declWriter.println( "ppt-length " + ppt.bb_length );
      }

      VarInfo [] varInfos = ppt.var_infos;
      for ( int i = 0; i < varInfos.length; i++ ) {

        VarDefinition vardef = varInfos[i].vardef;

        // don't include orig( ) variables
        if ( vardef != null && !varInfos[i].isPrestate( ) ) {

          identifiers.add( "variable" );
          identifiers.add( vardef.name );
          declWriter.println( "  variable " + vardef.name );

          String kind = vardef.kind.toString( );
          String relname = "";
          boolean isRelname = false;

          if ( vardef.relative_name != null ) {
            isRelname = true;
            relname = vardef.relative_name;
          }
          declWriter.println( "    var-kind " + kind.toLowerCase( ) + " " + relname );
          identifiers.add( "var-kind" );
          identifiers.add( kind.toLowerCase( ) );
          identifiers.add( Boolean.toString( isRelname ) );

          if ( isRelname ) {
            identifiers.add( relname );
          }

          if ( vardef.enclosing_var != null ) {
            identifiers.add(  "enclosing-var" );
            identifiers.add( vardef.enclosing_var );
            declWriter.println( "    enclosing-var " + vardef.enclosing_var );
          }

          if ( vardef.arr_dims != 0  ) {
            identifiers.add(  "array");
            identifiers.add( Integer.toString( vardef.arr_dims ) );
            declWriter.println( "    array" + " " + vardef.arr_dims );
          }

          declWriter.println( "    dec-type " + vardef.declared_type );
          declWriter.println( "    rep-type " + vardef.rep_type );

          identifiers.add( "dec-type" );
          identifiers.add( vardef.declared_type.toString( ) );

          identifiers.add( "rep-type" );
          identifiers.add( vardef.rep_type.toString( ) );

          if ( vardef.static_constant_value != null ) {
            identifiers.add( "constant" );
            identifiers.add( vardef.static_constant_value.toString( ) );
            declWriter.println( "    constant " + vardef.static_constant_value );
          }

          String fls = "";
          if ( !vardef.flags.isEmpty( ) ) {
            identifiers.add( "flags" );
            identifiers.add( Integer.toString( vardef.flags.size( ) ) );

            for ( VarFlags flag : vardef.flags ) {
              fls += flag.toString( ) + " ";
              identifiers.add( flag.toString( ).toLowerCase( ) );
            }
            if ( !fls.equals("") ) {
              declWriter.println( "    flags " + fls.toLowerCase( ).trim( ) );
            }
          }

          fls = "";
          if ( !vardef.lang_flags.isEmpty( ) ) {
            identifiers.add(  "lang-flags" );
            identifiers.add( Integer.toString( vardef.lang_flags.size( ) ) );

            for ( LangFlags langflag : vardef.lang_flags ) {
              fls += langflag.toString( ) + " ";
              identifiers.add( langflag.toString( ).toLowerCase( ) );
            }
            if ( !fls.equals("") ) {
              declWriter.println( "    lang-flags " + fls.toLowerCase( ).trim( ) );
            }
          }

          identifiers.add( "comparability" );
          identifiers.add( "22" );
          declWriter.println( "    comparability " +  22 );

          if ( vardef.parent_ppt != null ) {
            identifiers.add( "parent" );
            identifiers.add( vardef.parent_ppt );
            identifiers.add( "" + vardef.parent_relation_id );

            if ( vardef.parent_variable != null ) {
              identifiers.add( vardef.parent_variable );
            }
            else {
              identifiers.add( "null" );
            }
            declWriter.println( "    parent " + vardef.parent_ppt + " " + vardef.parent_relation_id );
          }
        }
      }
      try {
        for ( String binIdent : identifiers ) {
          binWriter.writeByte( binIdent.length( ) );
          binWriter.writeBytes( binIdent );
        }
        binWriter.writeByte( -1 );

      } catch ( IOException e ) {
          e.printStackTrace( System.out );
      }
    }

    // Reads from the binary file the specified number of records
    // The method is used for reading declarations from a binary file
    public static String [] get_binaryRecords( DataInputStream binReader, int records )
    throws IOException {

      String readRecords [] = new String [records];

      for (int i=0; i<records; i++) {
	int strLen = binReader.readByte( ); // gets the length of the string
        byte[] buf = new byte [strLen];
        binReader.read( buf, 0, strLen ); // gets the string
        readRecords[i] = new String( buf ); // platform charset dependant
      }
      return readRecords;
    }

    // Integer key, String value
    static Map<Integer, String> pptDeclMap = new HashMap<Integer, String>( );
    static int pptDeclIndex = 0;
    // Vardef_names of all variable (included and excluded)
    static Vector<String> allVars = new Vector<String>( );

    // Reads a binary declaration
    // "ppt" string has already been read
    // -1 denotes the end of a declaration block in a binary file
    public static PptTopLevel read_binaryDeclaration( DataInputStream binReader,
						      ParseState state, PrintWriter pWriter )
	throws IOException {

	int strLen = binReader.readByte( ); // gets the length of the string
	byte [] buf = new byte [strLen];
	binReader.read( buf, 0, strLen ); // gets the string
	String s_ppt_name = new String( buf );

	// registers the name, so that it is referred to by its integer index
	// in the corresponding data samples
	if ( !pptDeclMap.containsValue( s_ppt_name ) ) {
	  pptDeclIndex++;
          pptDeclMap.put( pptDeclIndex, s_ppt_name );
	}

	String msg = "\nppt " + s_ppt_name.replaceAll( " ", "\\\\_" );
	if ( bin2ascii_dbg )
	  msg += " hashKey = "  + pptDeclIndex;
	echoMsg( pWriter, msg );

	// Information that will populate the new program point
	Map<String,VarDefinition> varmap
	    = new LinkedHashMap<String,VarDefinition>();

	VarDefinition vardef = null;
	List<ParentRelation> ppt_parents = new ArrayList<ParentRelation>();
	EnumSet<PptFlags> ppt_flags = EnumSet.noneOf (PptFlags.class);
	PptType ppt_type = PptType.POINT;
	List<String> ppt_successors = null;
	/*@Interned*/ String function_id = null;
	int bb_length = 0;

	// while the end of the declaration hasn't been reached
	while ( ( strLen = binReader.readByte( ) ) != -1 ) {

	  buf = new byte [strLen];
          binReader.read( buf, 0, strLen );
          String record = new String( buf );

          if ( vardef == null ) {
            if ( record.equals( "parent") ) {

              // attr[0] = parent_relation
              // attr[1] = parent_name
              // attr[2] = parent_id
              String p_attributes [] = get_binaryRecords( binReader, 3 );
              String line = "";

              line = p_attributes[0] + " " + p_attributes[1] + " " + p_attributes[2];

              Scanner scanner = new Scanner( line + "\n" );
              ppt_parents.add( parse_ppt_parent( state, scanner ) );
              echoMsg( pWriter, record + " " + line );

            } else if ( record.equals( "flags" ) ) {
                String flg_num [] = get_binaryRecords( binReader, 1 );
                String flags [] = get_binaryRecords( binReader, Integer.valueOf( flg_num[0] ) );

                String line = "";
                for ( int j = 0; j < flags.length; j++ ) {
                    line += flags[j] + " ";
                }

                Scanner scanner = new Scanner( line.trim( ) + "\n" );
                parse_ppt_flags( state, scanner, ppt_flags );
                echoMsg( pWriter, (record + " " + line) );

		} else if ( record.equals( "variable" ) ) {
                    //vd_attr[0] = vardef_name
		    String vd_attr [] = get_binaryRecords( binReader, 1 );

		    Scanner scanner = new Scanner( vd_attr[0] + "\n" );
		    vardef = new VarDefinition( state, scanner );

		    if (var_included (vardef.name)) {
		      varmap.put ( vardef.name, vardef );
		      allVars.add( vardef.name );
		    }
		    else {
		      allVars.add( vardef.name );
		    }
		    echoMsg( pWriter, ("  " + record + " " + vardef.name) );

		} else if ( record.equals( "ppt-type" ) ) {
		    //type_attr[0] = ppt_type string
		    String type_attr [] = get_binaryRecords( binReader, 1 );

		    Scanner scanner = new Scanner( type_attr[0] + "\n" );
		    ppt_type = parse_ppt_type ( state, scanner );
		    echoMsg( pWriter, (record + " " + type_attr[0]) );

		} else if ( record.equals( "ppt-successors" ) ) {
		    String succ_num [] = get_binaryRecords( binReader, 1 );
		    String successors [] = get_binaryRecords( binReader, Integer.valueOf( succ_num[0] ) );

		    String line = "";
		    for ( int j = 0; j < successors.length; j++ ) {
		      line += successors[j] + " ";
		    }

		    Scanner scanner = new Scanner( line.trim( ) + "\n" );
		    ppt_successors = parse_ppt_successors (state, scanner);

		    echoMsg( pWriter, (record + " " + line.trim()) );

		} else if ( record.equals( "ppt-func" ) ) {
		    String f_attr [] = get_binaryRecords( binReader, 1 );
		    function_id = f_attr[0].intern();
		    echoMsg( pWriter, (record + " " + function_id) );

		} else if ( record.equals( "ppt-length" ) ) {
		    String length_attr [] = get_binaryRecords( binReader, 1 );
		    bb_length = Integer.valueOf( length_attr[0] );
		    echoMsg( pWriter, (record + " " + bb_length) );

		} else {
		    decl_error (state, "record '%s' found where %s expected", record,
				"'parent', 'flags', 'ppt-length', 'ppt-func'"
				+ " or 'ppt-successors'");
		}
	    }
	    else { // there must be a current variable
		if ( record.equals( "var-kind" ) ) {
		  String kind_attr [] = get_binaryRecords( binReader, 2 );
                  String var_kind = kind_attr[0];
                  String isRelname = kind_attr[1];

                  String relname = "";
                  if ( isRelname.equals( "true" ) ) {
                    String relname_attr [] = get_binaryRecords( binReader, 1 );
                    relname = relname_attr[0];
                  }

                  Scanner scanner = new Scanner( var_kind + " " + relname + "\n" );
                  vardef.parse_var_kind (scanner);
                  echoMsg( pWriter, ("    " + record + " " + var_kind + " " + relname) );

		} else if ( record.equals( "enclosing-var" ) ) {
		    String enclVar_attr [] = get_binaryRecords( binReader, 1 );
		    vardef.enclosing_var = enclVar_attr[0];
		    echoMsg( pWriter, ("    " + record + " " + vardef.enclosing_var) );

		} else if ( record.equals( "reference-type" ) ) {
		    String refType_attr [] = get_binaryRecords( binReader, 1 );
		    String ref_type = refType_attr[0];

		    Scanner scanner = new Scanner( ref_type + "\n" );
		    vardef.parse_reference_type (scanner);
		    echoMsg( pWriter, ("    " + record + " " + ref_type) );

		} else if ( record.equals( "array" ) ) {
		    String arr_attr [] = get_binaryRecords( binReader, 1 );
		    String arr_dim = arr_attr[0];

		    Scanner scanner = new Scanner( arr_dim );
		    vardef.parse_array (scanner);
		    echoMsg( pWriter, ("    " + record + " " + arr_dim) );

		} else if ( record.equals( "rep-type" ) ) {
		    String rep_type [] = get_binaryRecords( binReader, 1 );

		    Scanner scanner = new Scanner( rep_type[0] + "\n" );
		    vardef.parse_rep_type (scanner);
		    echoMsg( pWriter, ("    " + record + " " + rep_type[0]) );

		} else if ( record.equals( "dec-type" ) ) {
		    String dec_type [] = get_binaryRecords( binReader, 1 );

		    Scanner scanner = new Scanner( dec_type[0] + "\n" );
		    vardef.parse_dec_type (scanner);
		    echoMsg( pWriter, ("    " + record + " " + dec_type[0]) );

		} else if ( record.equals( "flags") ) {
		    String flg_num [] = get_binaryRecords( binReader, 1 );
		    String flags [] = get_binaryRecords( binReader, Integer.valueOf( flg_num[0] ) );

		    String line = "";
		    for ( int j = 0; j < flags.length; j++ ) {
			line += flags[j] + " ";
		    }
		    Scanner scanner = new Scanner( line.trim() + "\n" );
		    vardef.parse_flags (scanner);
		    echoMsg( pWriter, ("    " + record + " " + line.trim()) );

		} else if ( record.equals( "lang-flags" ) ) {
		    String flg_num [] = get_binaryRecords( binReader, 1 );
		    String lang_flags [] = get_binaryRecords( binReader, Integer.valueOf( flg_num[0] ) );

		    String line = "";
		    for ( int j = 0; j < lang_flags.length; j++ ) {
			line += lang_flags[j] + " ";
		    }
		    Scanner scanner = new Scanner( line.trim( ) + "\n" );
		    vardef.parse_lang_flags (scanner);
		    echoMsg( pWriter, ("    " + record + " " + line.trim()) );

		} else if ( record.equals( "parent" ) ) {
		    // attr[0] = parent_ppt
		    // attr[1] = parent_rel_id
		    // attr[2] = parent_var
		    String p_attr [] = get_binaryRecords( binReader, 3 );
		    String line = "";

		    if ( p_attr[2].equals( "null" ) )
		      p_attr[2] = "";

		    line = p_attr[0] + " " + p_attr[1] + " " + p_attr[2];
		    Scanner scanner = new Scanner( line + "\n" );
		    vardef.parse_parent (scanner, ppt_parents);
		    echoMsg( pWriter, ("    " + record + " " + line.trim()) );

		} else if ( record.equals( "comparability" ) ) {
		    String comp_attr [] = get_binaryRecords( binReader, 1 );

		    Scanner scanner = new Scanner( comp_attr[0] + "\n" );
		    vardef.parse_comparability (scanner);
		    echoMsg( pWriter, ("    " + record + " " + comp_attr[0]) );

		} else if ( record.equals( "constant") ) {
		    String const_attr [] = get_binaryRecords( binReader, 1 );
		    String constant = const_attr[0];

		    Scanner scanner = new Scanner( constant + "\n" );
		    vardef.parse_constant (scanner);
		    echoMsg( pWriter, ("    " + record + " " + constant) );

		} else if ( record.equals( "variable" ) ) {
		    String vd_attr [] = get_binaryRecords( binReader, 1 );

		    Scanner scanner = new Scanner( vd_attr[0] + "\n" );
		    vardef = new VarDefinition (state, scanner);

		    if (varmap.containsKey (vardef.name))
		      decl_error (state, "var %s declared twice", vardef.name);

		    if (var_included (vardef.name)) {
		      varmap.put (vardef.name, vardef);
                      allVars.add( vardef.name );
		    }
		    else {
		      allVars.add( vardef.name );
		    }
		    echoMsg( pWriter, ("  " + record + " " + vd_attr[0]) );

		} else {
		    decl_error (state, "Unexpected variable item '%s' found", record);
		}
	    }
	}

	// If we are excluding this ppt, just read the data and throw it away
	if (!ppt_included ( s_ppt_name)) {
	  omitted_declarations++;
          return null;
	}

	// Build the var infos from the var definitions.
	VarInfo[] vi_array = new VarInfo[varmap.size()];
	int ii = 0;
	for (VarDefinition vd : varmap.values()) {
	  vi_array[ii++] = new VarInfo (vd);
	}

	// Check to see if the program point is new
	if ( state.all_ppts.containsName( s_ppt_name ) ) {
	  if (state.ppts_are_new) {
            PptTopLevel existing_ppt = state.all_ppts.get( s_ppt_name );
            check_decl_match (state, existing_ppt, vi_array);
          } else { // ppts are already in the map
              return state.all_ppts.get ( s_ppt_name );
          }
	}

	// Build the program point
	PptTopLevel newppt = new PptTopLevel( s_ppt_name, ppt_type, ppt_parents,
					     ppt_flags, ppt_successors, function_id, bb_length, vi_array );

	// Add this ppt to the list of ppts for this function_id.  If we
	// are still getting ppts for this function id, they should not have
	// been yet combined
	if (function_id != null) {
	  System.out.printf ("Declaration of ppt %s with %d variables\n",
			       newppt.name(), newppt.var_infos.length);
          List<PptTopLevel> f_ppts = func_ppts.get (function_id);
          if (f_ppts == null) {
            f_ppts = new ArrayList<PptTopLevel>();
            func_ppts.put (function_id, f_ppts);
          }
          for (PptTopLevel ppt : f_ppts)
            assert !ppt.combined_ppts_init : ppt.name();
          f_ppts.add (newppt);
	}
	return newppt;
  }

  /**
   * Reads one ppt declaration.  The next line should be the ppt record.
   * After completion, the file pointer will be pointing at the next
   * record (ie, the blank line at the end of the ppt declaration will
   * have been read in).
   * Returns null if the ppt is excluded/omitted from this execution of Daikon.
   */
  private static /*@Nullable*/ PptTopLevel read_ppt_decl (ParseState state, String top_line)
    throws IOException {

    // process the ppt record
    String line = top_line;
    Scanner scanner = new Scanner (line);
    /*@Interned*/ String record_name = need (state, scanner, "'ppt'");
    if (record_name != "ppt") { // interned
      decl_error (state, "found '%s' where 'ppt' expected", record_name);
    }
    /*@Interned*/ String ppt_name = need (state, scanner, "ppt name");

    // Information that will populate the new program point
    Map<String,VarDefinition> varmap
      = new LinkedHashMap<String,VarDefinition>();
    VarDefinition vardef = null;
    List<ParentRelation> ppt_parents = new ArrayList<ParentRelation>();
    EnumSet<PptFlags> ppt_flags = EnumSet.noneOf (PptFlags.class);
    PptType ppt_type = PptType.POINT;
    List<String> ppt_successors = null;
    /*@Interned*/ String function_id = null;
    int bb_length = 0;

    // Read the records that define this program point
    while ((line = state.reader.readLine()) != null) {
      debug_decl.log ("read line %s%n", line);
      line = line.trim();
      if (line.length() == 0)
        break;

      scanner = new Scanner (line);
      /*@Interned*/ String record = scanner.next().intern();
      if (vardef == null) {
        if (record == "parent") { // interned
          ppt_parents.add (parse_ppt_parent (state, scanner));
        } else if (record == "flags") { // interned
          parse_ppt_flags (state, scanner, ppt_flags);
        } else if (record == "variable") { // interned
          vardef = new VarDefinition (state, scanner);
          if (var_included (vardef.name))
            varmap.put (vardef.name, vardef);
        } else if (record == "ppt-type") { // interned
          ppt_type = parse_ppt_type (state, scanner);
        } else if (record == "ppt-successors") { // interned
          ppt_successors = parse_ppt_successors (state, scanner);
        } else if (record == "ppt-func") { // interned
          function_id = need (state, scanner, "function id");
          need_eol (state, scanner);
        } else if (record == "ppt-length") { // interned
          bb_length = Integer.decode(need (state, scanner, "length"));
          need_eol (state, scanner);
        } else {
          decl_error (state, "record '%s' found where %s expected", record,
                      "'parent', 'flags', 'ppt-length', 'ppt-func'"
                      + " or 'ppt-successors'");
        }
      } else { // there must be a current variable
        if (record == "var-kind") { // interned
          vardef.parse_var_kind (scanner);
        } else if (record == "enclosing-var") { // interned
          vardef.parse_enclosing_var (scanner);
        } else if (record == "reference-type") { // interned
          vardef.parse_reference_type (scanner);
        } else if (record == "array") { // interned
          vardef.parse_array (scanner);
        } else if (record == "rep-type") { // interned
          vardef.parse_rep_type (scanner);
        } else if (record == "dec-type") { // interned
          vardef.parse_dec_type (scanner);
        } else if (record == "flags") { // interned
          vardef.parse_flags (scanner);
        } else if (record == "lang-flags") { // interned
          vardef.parse_lang_flags (scanner);
        } else if (record == "parent") { // interned
          vardef.parse_parent (scanner, ppt_parents);
        } else if (record == "comparability") { // interned
          vardef.parse_comparability (scanner);
        } else if (record == "constant") { // interned
          vardef.parse_constant (scanner);
        } else if (record == "variable") { // interned
          vardef = new VarDefinition (state, scanner);
          if (varmap.containsKey (vardef.name))
            decl_error (state, "var %s declared twice", vardef.name);
          if (var_included (vardef.name))
            varmap.put (vardef.name, vardef);
        } else {
          decl_error (state, "Unexpected variable item '%s' found", record);
        }
      }
    }

    // If we are excluding this ppt, just read the data and throw it away
    if (!ppt_included (ppt_name)) {
      omitted_declarations++;
      return null;
    }

    // Build the var infos from the var definitions.
    VarInfo[] vi_array = new VarInfo[varmap.size()];
    int ii = 0;
    for (VarDefinition vd : varmap.values()) {
      vi_array[ii++] = new VarInfo (vd);
    }

    // Check to see if the program point is new
    if (state.all_ppts.containsName(ppt_name)) {
      if (state.ppts_are_new) {
        PptTopLevel existing_ppt = state.all_ppts.get(ppt_name);
        assert existing_ppt != null; // because state.all_ppts.containsName(ppt_name)
        check_decl_match (state, existing_ppt, vi_array);
      } else { // ppts are already in the map
        return state.all_ppts.get (ppt_name);
      }
    }

    // Build the program point
    PptTopLevel newppt = new PptTopLevel(ppt_name, ppt_type, ppt_parents,
               ppt_flags, ppt_successors, function_id, bb_length, vi_array);

    // Add this ppt to the list of ppts for this function_id.  If we
    // are still getting ppts for this function id, they should not have
    // been yet combined
    if (function_id != null) {
      System.out.printf ("Declaration of ppt %s with %d variables\n",
                         newppt.name(), newppt.var_infos.length);
      List<PptTopLevel> f_ppts = func_ppts.get (function_id);
      if (f_ppts == null) {
        f_ppts = new ArrayList<PptTopLevel>();
        func_ppts.put (function_id, f_ppts);
      }
      for (PptTopLevel ppt : f_ppts)
        assert !ppt.combined_ppts_init : ppt.name();
      f_ppts.add (newppt);
    }
    return newppt;
  }

  /** Parses a ppt parent hierarchy record and returns it. **/
  private static ParentRelation parse_ppt_parent (ParseState state,
       Scanner scanner) throws DeclError {

    ParentRelation pr = new ParentRelation();
    PptRelationType rel_type = parse_enum_val (state, scanner, PptRelationType.class,
                                      "relation type");
    pr.rel_type = rel_type;
    pr.parent_ppt_name = need (state, scanner, "ppt name");
    pr.id = Integer.parseInt (need (state, scanner, "relation id"));
    need_eol (state, scanner);
    return (pr);
  }

  /**
   * Parses a program point flag record.  Adds any specified flags to
   * to flags.
   */
  private static void parse_ppt_flags (ParseState state, Scanner scanner,
                                  EnumSet<PptFlags> flags) throws DeclError {

    flags.add (parse_enum_val (state, scanner, PptFlags.class, "ppt flags"));
    while (scanner.hasNext())
      flags.add (parse_enum_val (state, scanner, PptFlags.class, "ppt flags"));
  }

  /** Parses a ppt-type record and returns the type **/
  private static PptType parse_ppt_type (ParseState state, Scanner scanner)
    throws DeclError {

    PptType ppt_type = parse_enum_val (state, scanner, PptType.class, "ppt type");
    need_eol (state, scanner);
    return (ppt_type);
  }

  /** Parses a ppt-successors record and returns the successors **/
  private static List<String> parse_ppt_successors (ParseState state,
                                                    Scanner scanner)
    throws DeclError {

    List<String> succs = new ArrayList<String>();
    String succ = need (state, scanner, "name of successor ppt");
    if (!succ.endsWith (":::BB"))
      succ = succ + ":::BB";
    succs.add (succ);
    while (scanner.hasNext()) {
      succ = need (state, scanner, "name of successor ppt");
      if (!succ.endsWith (":::BB"))
        succ = succ + ":::BB";
      succs.add (succ);
    }
    need_eol (state, scanner);
    return (succs);
  }


  // The "DECLARE" line has already been read.
  private static /*@Nullable*/ PptTopLevel read_declaration(ParseState state)
    throws IOException {

    // We have just read the "DECLARE" line.
    String ppt_name = state.reader.readLine();
    if (ppt_name == null) {
      throw new Daikon.TerminationMessage(
        "File ends with \"DECLARE\" with no following program point name",
        state);
    }
    ppt_name = ppt_name.intern();
    VarInfo[] vi_array = read_VarInfos(state, ppt_name);

    // System.out.printf ("Ppt %s with %d variables\n", ppt_name,
    //                   vi_array.length);

    // This program point name has already been encountered.
    if (state.all_ppts.containsName(ppt_name)) {
      if (state.ppts_are_new) {
        PptTopLevel existing_ppt = state.all_ppts.get(ppt_name);
        assert existing_ppt != null; // because state.all_ppts.containsName(ppt_name)
        check_decl_match (state, existing_ppt, vi_array);
      } else { // ppts are already in the map
        return state.all_ppts.get (ppt_name);
      }
    }

    // If we are excluding this ppt, just throw it away
    if (!ppt_included (ppt_name)) {
      omitted_declarations++;
      return null;
    }


    // taking care of visibility information
    // the information is needed in the variable hierarchy because private methods
    // should not be linked under the object program point
    // the ppt name is truncated before putting it in the pptMap because the visibility
    // information is only present in the decls file and not the dtrace file

    //    if (ppt_name.startsWith("public")) {
    //      int position = ppt_name.indexOf("public");
    //      ppt_name = ppt_name.substring(7);
    //      PptTopLevel newppt = new PptTopLevel(ppt_name, vi_array);
    //      newppt.ppt_name.setVisibility("public");
    //      return newppt;
    //    }
    //    if (ppt_name.startsWith("private")) {
    //      int position = ppt_name.indexOf("private");
    //      ppt_name = ppt_name.substring(8);
    //      PptTopLevel newppt = new PptTopLevel(ppt_name, vi_array);
    //      newppt.ppt_name.setVisibility("private");
    //      return newppt;
    //    }
    //    if (ppt_name.startsWith("protected")) {
    //      int position = ppt_name.indexOf("protected");
    //      ppt_name = ppt_name.substring(10);
    //      PptTopLevel newppt = new PptTopLevel(ppt_name, vi_array);
    //      newppt.ppt_name.setVisibility("protected");
    //      return newppt;
    //    }

    //TODO: add a new config variable to turn this accessibility flag processing on?
    PptTopLevel newppt = new PptTopLevel(ppt_name, vi_array);
    // newppt.ppt_name.setVisibility("package-protected");
    return newppt;
    // return new PptTopLevel(ppt_name, vi_array);
  }
  private static VarInfo[] read_VarInfos(ParseState state, String ppt_name)
    throws IOException {

    // The var_infos that will populate the new program point
    List<VarInfo> var_infos = new ArrayList<VarInfo>();

    // Each iteration reads a variable name, type, and comparability.
    // Possibly abstract this out into a separate function??
    VarInfo vi;
    while ((vi = read_VarInfo(state, ppt_name)) != null) {
      for (VarInfo vi2 : var_infos) {
        if (vi.name() == vi2.name()) {
          throw new Daikon.TerminationMessage("Duplicate variable name " + vi.name(), state);
        }
      }
      // Can't do this test in read_VarInfo, it seems, because of the test
      // against null above.
      if (!var_included (vi.name())) {
        continue;
      }
      var_infos.add(vi);
    }

    VarInfo[] result = var_infos.toArray(new VarInfo[var_infos.size()]);
    return result;
  }

  // So that warning message below is only printed once
  private static boolean seen_string_rep_type = false;

  /**
   * Read a variable name, type, and comparability; construct a VarInfo.
   * Return null after reading the last variable in this program point
   * declaration.
   **/
  private static /*@Nullable*/ VarInfo read_VarInfo(
    ParseState state,
    String ppt_name)
    throws IOException {
    LineNumberReader file = state.reader;
    int varcomp_format = state.varcomp_format;
    String filename = state.filename;

    String line = file.readLine();
    if ((line == null) || (line.equals("")))
      return null;
    String varname = line;
    String proglang_type_string_and_aux = file.readLine();
    String file_rep_type_string = file.readLine();
    String comparability_string = file.readLine();
    if ( // (varname == null) || // just cheeck varname above
        (proglang_type_string_and_aux == null)
        || (file_rep_type_string == null)
        || (comparability_string == null))
      throw new Daikon.TerminationMessage(
        "End of file "
          + filename
          + " while reading variable "
          + varname
          + " in declaration of program point "
          + ppt_name);
    int equals_index = file_rep_type_string.indexOf(" = ");
    String static_constant_value_string = null;
    /*@Interned*/ Object static_constant_value = null;
    boolean is_static_constant = false;
    if (equals_index != -1) {
      is_static_constant = true;
      static_constant_value_string =
        file_rep_type_string.substring(equals_index + 3);
      file_rep_type_string = file_rep_type_string.substring(0, equals_index);
    }
    // XXX temporary, for compatibility with older .dtrace files.  12/20/2001
    if ("String".equals(file_rep_type_string)) {
      file_rep_type_string = "java.lang.String";
      if (!seen_string_rep_type) {
        seen_string_rep_type = true;
        System.err.println("Warning: Malformed trace file.  Representation type 'String' should be "+
                           "'java.lang.String' instead on line " +
                           (file.getLineNumber()-1) + " of " + filename);
      }
    }
    // This is for people who were confused by the above temporary
    // workaround when it didn't have a warning. But this has never
    // worked, so it's fatal.
    else if ("String[]".equals(file_rep_type_string)) {
      throw new Daikon.TerminationMessage("Representation type 'String[]' should be " +
                                           "'java.lang.String[]' instead for variable " + varname,
                                           file, filename);
    }
    /// XXX

    int hash_position = proglang_type_string_and_aux.indexOf('#');
    String aux_string = "";
    if (hash_position == -1) {
      hash_position = proglang_type_string_and_aux.length();
    } else {
      aux_string =
        proglang_type_string_and_aux.substring(
          hash_position + 1,
          proglang_type_string_and_aux.length());
    }

    String proglang_type_string =
      proglang_type_string_and_aux.substring(0, hash_position).trim();

    ProglangType prog_type;
    ProglangType file_rep_type;
    ProglangType rep_type;
    VarInfoAux aux;
    try {
      prog_type = ProglangType.parse(proglang_type_string);
      file_rep_type = ProglangType.rep_parse(file_rep_type_string);
      rep_type = file_rep_type.fileTypeToRepType();
      aux = VarInfoAux.parse(aux_string);
    } catch (IOException e) {
      throw new Daikon.TerminationMessage(e, file, filename);
    }

    if (static_constant_value_string != null) {
      static_constant_value =
        rep_type.parse_value(static_constant_value_string);
      // Why can't the value be null?
      assert static_constant_value != null;
    }
    VarComparability comparability = null;
    try {
      comparability = VarComparability.parse(varcomp_format,
                                             comparability_string, prog_type);
    } catch (Exception e) {
      throw new Daikon.TerminationMessage
        (String.format ("Error parsing comparability (%s) at line %d "
                        + "in file %s", e, file.getLineNumber(), filename));
    }
    if (!VarInfo.legalFileRepType(file_rep_type)) {
      throw new Daikon.TerminationMessage(
        "Unsupported representation type "
          + file_rep_type.format()
          + " (parsed as "
          + rep_type
          + ")"
          + " for variable "
          + varname,
        file,
        filename);
    }
    if (!VarInfo.legalRepType(rep_type)) {
      throw new Daikon.TerminationMessage(
        "Unsupported (converted) representation type "
          + file_rep_type.format()
          + " for variable "
          + varname,
        file,
        filename);
    }
    // COMPARABILITY TEST
    // if (!(comparability.alwaysComparable()
    //       || ((VarComparabilityImplicit)comparability).dimensions == file_rep_type.dimensions())) {
    //   throw new FileIOException(
    //     "Rep type " + file_rep_type.format() + " has " + file_rep_type.dimensions() + " dimensions"
    //       + " but comparability " + comparability + " has " + ((VarComparabilityImplicit)comparability).dimensions + " dimensions"
    //       + " for variable "
    //       + varname,
    //     file,
    //     filename);
    // }

    return new VarInfo(varname,
      prog_type,
      file_rep_type,
      comparability,
      is_static_constant,
      static_constant_value,
      aux);
  }

  private static int read_var_comparability (ParseState state, String line)
    throws IOException {

    // System.out.printf("read_var_comparability, line = '%s' %b%n", line,
    //                   new_decl_format);
    String comp_str = null;
    if (new_decl_format) {
      Scanner scanner = new Scanner (line);
      scanner.next();
      comp_str = need (state, scanner, "comparability");
      need_eol (state, scanner);
    } else { // old format
      comp_str = state.reader.readLine();
      if (comp_str == null) {
        throw new Daikon.TerminationMessage("Found end of file, expected comparability",
                                            state);
      }
    }

    if (comp_str.equals("none")) {
      return (VarComparability.NONE);
    } else if (comp_str.equals("implicit")) {
      return (VarComparability.IMPLICIT);
    } else {
      throw new Daikon.TerminationMessage("Unrecognized VarComparability '" + comp_str
                                          + "'", state);
    }
  }

  private static /*@Interned*/ String read_input_language (ParseState state, String line)
    throws IOException {

    Scanner scanner = new Scanner (line);
    scanner.next();
    /*@Interned*/ String input_lang = need (state, scanner, "input language");
    need_eol (state, scanner);
    return input_lang;
  }

  private static void read_decl_version (ParseState state, String line)
    throws IOException {
    Scanner scanner = new Scanner (line);
    scanner.next();
    /*@Interned*/ String version = need (state, scanner, "declaration version number");
    need_eol (state, scanner);
    boolean new_df = false;
    if (version == "2.0")       // interned
      new_df = true;
    else if (version == "1.0")  // interned
      new_df = false;
    else
      decl_error (state, "'%s' found where 1.0 or 2.0 expected",
                  version);

    // Make sure that if a format was specified previously, it is the same
    if ((new_decl_format != null) && (new_df != new_decl_format))
      decl_error (state, "decl format '%s' does not match previous setting",
                  version);

    new_decl_format = new Boolean (new_df);
  }

  // Each line following is the name (in JVM form) of a class that
  // implements java.util.List.  All those lines (including interspersed
  // comments) are returned.
  private static String read_list_implementors (LineNumberReader reader)
    throws IOException {
    StringBuilderDelimited result = new StringBuilderDelimited(lineSep);
    for (;;) {
      String line = reader.readLine();
      if (line == null || line.equals(""))
        break;
      result.append(line);
      if (isComment(line))
        continue;
      ProglangType.list_implementors.add(line.intern());
    }
    return result.toString();
  }

  // Pre: the string "List Implementors" has been read
  // Interspersed comments aren't supported
  private static void read_binary_list_implementors ( DataInputStream binReader )
    throws IOException {
    // Each line following is the name (in JVM form) of a class
    // that implements java.util.List.
     int list_size = binReader.readByte( );

    while( list_size > 0 ) {
      list_size--;

      int strLen = binReader.readByte( );
      byte [] buf = new byte [strLen];

      binReader.read( buf, 0, strLen );
      String impl = new String( buf );

      ProglangType.list_implementors.add( impl );
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// invocation tracking for dtrace files entry/exit grouping
  ///

  static final class Invocation implements Comparable<Invocation> {
    PptTopLevel ppt; // used in printing and in suppressing duplicates
    // Rather than a valuetuple, place its elements here.
    /*@Nullable*/ Object[] vals;
    int[] mods;

    static Object canonical_hashcode = new Object();

    Invocation(PptTopLevel ppt, /*@Nullable*/ Object[] vals, int[] mods) {
      this.ppt = ppt;
      this.vals = vals;
      this.mods = mods;
    }

    // Print the Invocation on two lines, indented by two spaces
    // The receiver Invocation may be canonicalized or not.
    String format() {
      return format(true);
    }

    // Print the Invocation on one or two lines, indented by two spaces.
    // The receiver Invocation may be canonicalized or not.
    String format(boolean show_values) {
      if (! show_values) {
        return "  " + ppt.ppt_name.getNameWithoutPoint();
      }

      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);

      pw.println("  " + ppt.ppt_name.getNameWithoutPoint());
      pw.print("    ");

      // [adonovan] is this sound? Let me know if not (sorry).
      //assert ppt.var_infos.length == vals.length;

      for (int j = 0; j < vals.length; j++) {
        if (j != 0)
          pw.print(", ");

        pw.print(ppt.var_infos[j].name() + "=");

        Object val = vals[j];
        if (canonical_hashcode.equals(val)) // succeeds only for canonicalized Invocations.  Can be an == test, but there is little point.  val can be null, so it cannot be the receiver.
          pw.print("<hashcode>");
        else if (val instanceof int[])
          pw.print(ArraysMDE.toString((int[]) val));
        else if (val instanceof String)
          pw.print(val == null ? "null" : UtilMDE.escapeNonASCII((String) val));
        else
          pw.print(val);
      }
      pw.println();

      return sw.toString();
    }

    /** Change uses of hashcodes to canonical_hashcode. **/
    public /*@Interned*/ Invocation canonicalize() {
      Object[] new_vals = new Object[vals.length];
      System.arraycopy(vals, 0, new_vals, 0, vals.length);
      VarInfo[] vis = ppt.var_infos;
      // Warning: abstraction violation!
      for (VarInfo vi : vis) {
        if ((vi.value_index != -1)
          && (vi.file_rep_type == ProglangType.HASHCODE)) {
          new_vals[vi.value_index] = canonical_hashcode;
        }
      }
      return new /*@Interned*/ Invocation(ppt, new_vals, mods);
    }

    // Return true if the invocations print the same
    public boolean equals(/*@Nullable*/ Object other) {
      if (other instanceof FileIO.Invocation)
        return this.format().equals(((FileIO.Invocation) other).format());
      else
        return false;
    }

    public int compareTo(Invocation other) {
      return ppt.name().compareTo(other.ppt.name());
    }

    public int hashCode() {
      return this.format().hashCode();
    }
  }

  // call_hashmap is for procedures with a (global, not per-procedure)
  // nonce that indicates which returns are associated with which entries.
  // call_stack is for functions without nonces.

  // I could save some Object overhead by using two parallel stacks
  // instead of Invocation objects; but that's not worth it.
  static Stack<Invocation> call_stack = new Stack<Invocation>();
  static HashMap<Integer,Invocation> call_hashmap = new HashMap<Integer,Invocation>();

  /** Reads data trace files using the default sample processor. **/
  public static void read_data_trace_files(Collection<String> files,
                                           PptMap all_ppts) throws IOException {

    Processor processor = new Processor();
    read_data_trace_files(files, all_ppts, processor, true);
  }

  /**
   * Read data from .dtrace files.
   * Calls @link{read_data_trace_file(File,PptMap,Pattern,false)} for each
   * element of filenames.
   *
   * @param ppts_are_new - true if declarations of ppts read from the data
   *                       trace file are new (and thus are not in all_ppts)
   *                       false if the ppts may already be there.
   **/
  public static void read_data_trace_files(Collection<String> files,
                PptMap all_ppts, Processor processor, boolean ppts_are_new)
                throws IOException {

    for (String filename : files) {
      try {
        read_data_trace_file(filename, all_ppts, processor, false,
                             ppts_are_new);
      } catch (IOException e) {
        String message = e.getMessage();
        if (message != null && message.equals("Corrupt GZIP trailer")) {
          System.out.println(
            filename
              + " has a corrupt gzip trailer.  "
              + "All possible data was recovered.");
        } else {
          throw e;
        }
      }
    }
    if (Daikon.server_dir!=null) {
      // Yoav: server mode
      while (true) {
        String[] dir_files = Daikon.server_dir.list();
        assert dir_files != null; // server_dir was checked when it was set
        Arrays.sort(dir_files);
        boolean hasEnd = false;
        for (String f:dir_files) {
          if (f.endsWith(".end")) hasEnd = true;
          if (f.endsWith(".end") || f.endsWith(".start")) continue;
          if (files.contains(f)) continue;
          files.add(f);
          System.out.println("Reading "+f);
          read_data_trace_file(new File(Daikon.server_dir,f).toString(), all_ppts, processor, false, ppts_are_new);
        }
        if (hasEnd) break;
        try { Thread.sleep(1000); } catch(java.lang.InterruptedException e) {}
      }
    }

    process_unmatched_procedure_entries();

    warn_if_hierarchy_mismatch(all_ppts);
  }

  // Determine if dataflow hierarchy should have been used, and print
  // warning if this does not match Daikon.use_dataflow_hierarchy.
  // Dataflow hierarchy should be used only when all program points
  // correspond to points normally found in traces from a
  // programming languages.
  private static void warn_if_hierarchy_mismatch(PptMap all_ppts) {

    boolean some_program_points = false;
    boolean all_program_points = true;

    // go through each top level ppt, and make all_program_points
    // false if at least one of them is not a program point normally
    // found in traces from programming languages
    for (Iterator<PptTopLevel> all_ppts_iter = all_ppts.ppt_all_iterator();
        all_ppts_iter.hasNext(); ) {
      PptTopLevel ppt_top_level = all_ppts_iter.next();

      boolean is_program_point =
        (ppt_top_level.ppt_name.isExitPoint() ||
        ppt_top_level.ppt_name.isEnterPoint() ||
        ppt_top_level.ppt_name.isThrowsPoint() ||
        ppt_top_level.ppt_name.isObjectInstanceSynthetic() ||
        ppt_top_level.ppt_name.isClassStaticSynthetic() ||
        ppt_top_level.ppt_name.isGlobalPoint());

      all_program_points = all_program_points && is_program_point;
      some_program_points = some_program_points || is_program_point;
    }

    // if all program points correspond to a programming language,
    // but the dataflow hierarchy has been turned off, then
    // suggest not using the --nohierarchy flag
    //    if (all_program_points && (!Daikon.use_dataflow_hierarchy)) {
    //      System.out.println("Warning: data trace appears to be over" +
    //                         " a program execution, but dataflow" +
    //                         " hierarchy has been turned off," +
    //                         " consider running Daikon without the" +
    //                         " --nohierarchy flag");
    //    }

    // if some of the program points do not correspond to a
    // points from a programming language, and the dataflow
    // hierarchy is being used, suggest using the --nohierarchy flag.
    if (Daikon.use_dataflow_hierarchy &&
        (!all_program_points) &&
        some_program_points) {
      System.out.println("Warning: Daikon is using a dataflow" +
                         " hierarchy analysis on a data trace" +
                         " that does not appear to be over a" +
                         " program execution, consider running"+
                         " Daikon with the --nohierarchy flag.");
    }
  }


  private static InputStream connectToChicory()
    {


        ServerSocket daikonServer = null;
        try
        {
            daikonServer = new ServerSocket(0); //bind to any free port

            //tell Chicory what port we have!
            System.out.println("DaikonChicoryOnlinePort=" + daikonServer.getLocalPort());

            daikonServer.setReceiveBufferSize(64000);

        }
        catch (IOException e)
        {
            throw new RuntimeException("Unable to create server", e);
        }

        Socket chicSocket = null;
        try
        {
            daikonServer.setSoTimeout(5000);

            //System.out.println("waiting for chicory connection on port " + daikonServer.getLocalPort());
            chicSocket = daikonServer.accept();
        }
        catch (IOException e)
        {
            throw new RuntimeException("Unable to connect to Chicory", e);
        }


        try
        {
            return chicSocket.getInputStream();
        }
        catch (IOException e)
        {
            throw new RuntimeException("Unable to get Chicory's input stream", e);
        }

    }


  /**
   * Class used to specify the processor to use for sample data.  By
   * default, the internal process_sample routine will be called, once for
   * each sample.
   */
  public static class Processor {
    public void process_sample(
                               PptMap all_ppts,
                               PptTopLevel ppt,
                               ValueTuple vt,
                               Integer nonce) {
      FileIO.process_sample(all_ppts, ppt, vt, nonce);
    }
  }


  /**
   * Total number of samples passed to process_sample().
   * Not part of ParseState because it's global over all files
   * processed by Daikon.
   */
  public static int samples_processed = 0;


  /** The type of the record that was most recently read. */
  public enum ParseStatus {
    SAMPLE,             // got a sample

    DECL,               // got a ppt decl
    DECL_VERSION,       // got an indication of the ppt decl format
    COMPARABILITY,      // got a VarComparability declaration
    LIST_IMPLEMENTORS,  // got a ListImplementors declaration
    INPUT_LANGUAGE,     // got an input-language declaration

    NULL,               // haven't read anything yet
    COMMENT,            // got a comment
    EOF,                // reached end of file
    TRUNCATED,          // dkconfig_max_line_number reached (without error)
    ERROR,              // continuable error; fatal errors thrown as exceptions
  };

  /**
   * ParseState indicates:
   * <ol>
   * <li>
   *   Some global information about the state of the parser while reading
   *   a decl or dtrace file.
   * <li>
   *   The record that was most recently read; thus, ParseState is
   *   essentially a discriminated union whose tag is a ParseStatus.
   *   (TODO:  These are poor names that should probably be swapped!)
   *   ParseState is what is returned (actually, side-effected) by
   *   method read_data_trace_record when it reads a record.
   * </ol>
   **/
  public static class ParseState {

    //
    // This is the global information about the state of the parser.
    //

    /** Name of input file **/
    public String filename;

    /** True if the current file is a declaration file. **/
    public boolean is_decl_file;

    /** True if the current file is a binary file. **/
    public boolean is_bin_file;

    /** True if ppts may be new.  If a duplicate is seen, it must match
     * a previous point exactly.  If false, the previous ppt is used without
     * checking for a match.
     */
    public boolean ppts_are_new;

    /** All of the ppts seen so far **/
    public PptMap all_ppts;

    /** Input stream **/
    public LineNumberReader reader;

    /** Binary input stream **/
    public DataInputStream binReader;

    /** Total number of lines in the input file **/
    public long total_lines;

    /** Comparability format, either VarComparability.IMPLICIT or
     * VarComparability.NONE
     */
    public int varcomp_format;

    //
    // This is the discriminated-union part of the ParseState.
    // (Presumably this design was chosen for efficiency, to avoid creating
    // & garbage-collecting these values many times.)
    //

    public ParseStatus status;

    /** Current ppt.  Used when status=DECL or SAMPLE.  Can be null if this
     * declaration was skipped because of --ppt-select-pattern or
     * --ppt-omit-pattern.
     */
    public /*@Nullable*/ PptTopLevel ppt;

    /** The current nonce.  Used when status=SAMPLE. */
    public /*@Nullable*/ Integer nonce;

    /** The current set of values.  Used when status=SAMPLE. */
    public /*@Nullable*/ ValueTuple vt;

    /** Miscellaneous text in the parsed item **/
    public String payload;      // used when state=COMMENT


    /** Start parsing the given file. */
    public ParseState (String raw_filename, boolean decl_file_p,
                       boolean ppts_are_new, PptMap ppts) throws IOException {
      // Pretty up raw_filename for use in messages
      if (raw_filename.equals("-")) {
        filename = "standard input";
      }
      else if (raw_filename.equals("+")) {
        filename = "chicory socket";
      }
      else {
        // Remove directory parts, to make it shorter
        filename = raw_filename;
      }

      is_decl_file = decl_file_p;
      this.ppts_are_new = ppts_are_new;
      all_ppts = ppts;

      boolean is_url = raw_filename.startsWith ("file:")
        || raw_filename.startsWith ("jar:");

      is_bin_file = ( filename.indexOf( "dat" ) != -1 );

      // Do we need to count the lines in the file?
      total_lines = 0;
      boolean count_lines = dkconfig_count_lines;
      if (is_decl_file || is_bin_file ) {
        count_lines = false;
      } else if (dkconfig_dtrace_line_count != 0) {
        total_lines = dkconfig_dtrace_line_count;
        count_lines = false;
      } else if (filename.equals("-")) {
        count_lines = false;
      } else if (is_url) {
        count_lines = false;
      } else if (Daikon.dkconfig_progress_delay == -1) {
        count_lines = false;
      } else if ((new File(raw_filename)).length() == 0) {
        // Either it's actually empty, or it's something like a pipe.
        count_lines = false;
      }

      if (count_lines) {
        Daikon.progress = "Checking size of " + filename;
        total_lines = UtilMDE.count_lines(raw_filename);
      } else {
        // System.out.printf ("no count %b %d %s %d %d\n", is_decl_file,
        //                    dkconfig_dtrace_line_count, filename,
        //  Daikon.dkconfig_progress_delay, (new File(raw_filename)).length());
      }

      // Open the reader stream
      if (raw_filename.equals("-")) {
        // "-" means read from the standard input stream
        Reader file_reader = new InputStreamReader(System.in, "ISO-8859-1");
        reader = new LineNumberReader(file_reader);
      }
      else if (raw_filename.equals("+")) { //socket comm with Chicory
        InputStream chicoryInput = connectToChicory();
        InputStreamReader chicReader = new InputStreamReader(chicoryInput);
        reader = new LineNumberReader(chicReader);
      } else if (is_url) {
        URL url = new URL (raw_filename);
        InputStream stream = url.openStream();
        if (raw_filename.endsWith (".gz")) {
          GZIPInputStream gzip_stream = new GZIPInputStream (stream);
          reader = new LineNumberReader (new InputStreamReader (gzip_stream));
        } else {
          reader = new LineNumberReader (new InputStreamReader (stream));
        }
      } else if ( is_bin_file ) {
          if ( raw_filename.endsWith (".gz") ) {
            // binReader is an output stream and could be casted to
            // GZIP or Data stream accordingly
            // not supported yet
            //binReader = new GZIPInputStream( new FileInputStream( filename ) );
          }
          else
            binReader = new DataInputStream( new FileInputStream( filename ) );
      } else {
        reader = UtilMDE.lineNumberFileReader(raw_filename);
      }

      varcomp_format = VarComparability.IMPLICIT;
      status = ParseStatus.NULL;
      ppt = null;
    }

    /** Returns the current line number in the input file, or -1 if not available. **/
    public int get_linenum () {
      return reader.getLineNumber();
    }

    private static NumberFormat pctFmt;
    static {
      pctFmt = NumberFormat.getPercentInstance();
      pctFmt.setMinimumFractionDigits(2);
      pctFmt.setMaximumFractionDigits(2);
    }

    public String reading_message () {
      String line;
      if (reader == null) {
        line = "?";
      } else {
        long lineNum = reader.getLineNumber();
        line = String.valueOf(lineNum);
        if (total_lines > 0) {
          double frac =
            lineNum / (double) total_lines;
          String percent = pctFmt.format(frac);
          line = line + ", " + percent;
        }
      }
      return "Reading " + filename + " (line " + line + ") ...";
    }

    public String line_file_message() {
      return String.format (" at line %d in file %s",
                            reader.getLineNumber(), filename);
    }

  }

  /** Returns the current line number in the input file, or -1 if not available. **/
  public static int get_linenum () {
    if (FileIO.data_trace_state == null) {
      return -1;
    } else {
      return FileIO.data_trace_state.get_linenum();
    }
  }


  /**
   * Logically, this is a local variable in method read_data_trace_file.
   * It is used for status output, and to give the line number at which
   * a problem was detected.
   */
  // The @LazyNonNull property is not true globally, but within every
  // method it's true, so it is a useful annotation.
  public static /*@LazyNonNull*/ ParseState data_trace_state = null;
  @SuppressWarnings("nullness") // setting a LazyNonNull field to null
  private static void clear_data_trace_state() {
    data_trace_state = null;
  }


  /**
   * Read declarations or samples (not just sample data) from .dtrace
   * file, using standard data processor. **/
  static void read_data_trace_file(String filename, PptMap all_ppts)
    throws IOException {
    Processor processor = new Processor();
    read_data_trace_file(filename, all_ppts, processor, false, true);
  }

  /**
   * Read declarations OR samples (not just sample data as the name might
   * imply) from .dtrace file.
   **/
  static void read_data_trace_file(String filename, PptMap all_ppts,
                                   Processor processor,
                                   boolean is_decl_file, boolean ppts_are_new)
    throws IOException {

    if (debugRead.isLoggable(Level.FINE)) {
      debugRead.fine ("read_data_trace_file " + filename
                      + ((Daikon.ppt_regexp != null)
                         ? " " + Daikon.ppt_regexp.pattern() : "")
                      + ((Daikon.ppt_omit_regexp != null)
                         ? " " + Daikon.ppt_omit_regexp.pattern() : ""));
    }

    ParseState data_trace_state = new ParseState(filename, is_decl_file, ppts_are_new,
                                      all_ppts);
    FileIO.data_trace_state = data_trace_state;

    // Used for debugging: write new data trace file.
    if (Global.debugPrintDtrace) {
      Global.dtraceWriter
             = new PrintWriter(new FileWriter(new File(filename + ".debug")));
    }

    PrintWriter pWriter = null;
    if ( data_trace_state.is_bin_file )
	pWriter = null; //new PrintWriter( new FileWriter( new File( filename + ".dtrace" ) ), true );

    while (true) {
      assert data_trace_state != null;    // for nullness checker
      if ( data_trace_state.is_bin_file ) {
	read_binary_data_trace_record( data_trace_state, pWriter );
      }
      else
	read_data_trace_record (data_trace_state);

      if (data_trace_state.status == ParseStatus.SAMPLE) {
        assert data_trace_state.ppt != null; // nullness: dependent type
        assert data_trace_state.vt != null; // nullness: dependent type
        assert data_trace_state.nonce != null; // nullness: dependent type
        samples_processed++;
        // Add orig and derived variables; pass to inference (add_and_flow)
        try {
          processor.process_sample (data_trace_state.all_ppts,
                                    data_trace_state.ppt,
                                    data_trace_state.vt,
                                    data_trace_state.nonce);
        } catch (Error e) {
          if (! dkconfig_continue_after_file_exception) {
            throw new Daikon.TerminationMessage (e.toString(), data_trace_state);
          } else {
            System.out.println ();
            System.out.println ("WARNING: Error while processing "
                                + "trace file - record ignored");
            System.out.print ("Ignored backtrace:");
            e.printStackTrace(System.out);
            System.out.println ();
          }
        }
      }
      else if ((data_trace_state.status == ParseStatus.EOF)
               || (data_trace_state.status == ParseStatus.TRUNCATED)) {
        break;
      }
      else
        ;  // don't need to do anything explicit for other records found
    }

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.close();
    }

    Daikon.progress = "Finished reading " + data_trace_state.filename;

    clear_data_trace_state();
  }


  /**
   * Like read_data_trace_record, but sets global FileIO.data_trace_state
   * for the duration of the call then clears it before returning.
   * Intended for most external callers.
   */
  public static void read_data_trace_record_setstate (ParseState state)
    throws IOException {

    FileIO.data_trace_state = state;
    read_data_trace_record(state);
    clear_data_trace_state();
  }

  /**
   * Read a single record of ANY type (sample, declaration, comparability,
   * etc.) from a dtrace file.
   * The record is stored by side effect into the state argument.
   */
  // TODO:  For clarity, this should perhaps return its side-effected argument.
  public static void read_data_trace_record (ParseState state)
    throws IOException {

    @SuppressWarnings("interning")
        //boolean stateOK = ( state == FileIO.data_trace_state );
        //assert stateOK;

    LineNumberReader reader = state.reader;

    for (String line = reader.readLine(); line != null;
         line = reader.readLine()) {

      if (line.equals("")) {
        continue;
      }

      // This cleverness would not be necessary if every comment was followed by
      // a blank line.  We can't depend on that, though.
      if (isComment(line)) {
        StringBuilderDelimited commentLines = new StringBuilderDelimited(lineSep);
        commentLines.append(line);
        while (nextLineIsComment(reader)) {
          commentLines.append(reader.readLine());
        }
        state.payload = commentLines.toString();
        state.status = ParseStatus.COMMENT;
        return;
      }

      // stop at a specified point in the file
      if ((dkconfig_max_line_number > 0)
          && (reader.getLineNumber() > dkconfig_max_line_number))
        {
          state.status = ParseStatus.TRUNCATED;
          return;
        }

      // interning bugfix:  no need to intern "line" (after code change to is_declaration_header)

      // Check for the file format
      if (line.startsWith ("decl-version")) {
        read_decl_version (state, line);
        state.payload = (new_decl_format ? "2.0" : "1.0");
        state.status = ParseStatus.DECL_VERSION;
        return;
      }

      // Check for the input language
      if (line.startsWith ("input-language")) {
        String input_language = read_input_language (state, line);
        state.payload = input_language;
        state.status = ParseStatus.INPUT_LANGUAGE;
        return;
      }

      // If we have gotten to here and new_decl_format is not set, presume
      // it is the old format
      if (new_decl_format == null) {
        // System.out.printf ("setting new_decl_format to false%n");
        new_decl_format = new Boolean (false);
      }

      // First look for declarations in the dtrace stream
      if (is_declaration_header (line)) {
        if (new_decl_format)
          state.ppt = read_ppt_decl (state, line);
        else
          state.ppt = read_declaration(state);
        // ppt can be null if this declaration was skipped because of
        // --ppt-select-pattern or --ppt-omit-pattern.
        if (state.ppt != null) {
          if (!state.all_ppts.containsName (state.ppt.name())) {
            assert state.ppt != null; // for nullness checker
            state.all_ppts.add(state.ppt);
            assert state.ppt != null; // for nullness checker
            try {
              Daikon.init_ppt(state.ppt, state.all_ppts);
            } catch (Exception e) {
              decl_error (state, "unexpected error: %s", e);
            }
          }
        }
        state.status = ParseStatus.DECL;
        return;
      }
      if (line.equals ("VarComparability")
          || line.startsWith ("var-comparability")) {
        state.varcomp_format = read_var_comparability (state, line);
        state.status = ParseStatus.COMPARABILITY;
        return;
      }
      if (line.equals("ListImplementors")) {
        state.payload = read_list_implementors (reader);
        state.status = ParseStatus.LIST_IMPLEMENTORS;
        return;
      }
      String ppt_name = line;
      if (new_decl_format)
        ppt_name = unescape_decl(line); // interning bugfix: no need to intern
      if (!ppt_included (ppt_name)) {
        // System.out.printf ("skipping ppt %s\n", line);
        while ((line != null) && !line.equals(""))
          line = reader.readLine();
        continue;
      }
      // System.out.printf ("Not skipping ppt  %s\n", line);

      if (state.is_decl_file) {
        if ((! new_decl_format) && line.startsWith ("ppt ")) {
          throw new Daikon.TerminationMessage(String.format("Declaration file %s is not version 2.0, but line %d looks like a version 2.0 declaration: %s%nPerhaps the file is missing a \"decl-version 2.0\" record at the beginning", state.filename, state.reader.getLineNumber(), line)); }
        throw new Daikon.TerminationMessage(String.format("Declaration files should not contain samples, but file %s does at line %d: %s", state.filename, state.reader.getLineNumber(), line));
      }

      // Parse the ppt name
      try {
        new PptName(ppt_name);
      } catch (Throwable t) {
        String message = t.getMessage();
        assert message != null;
        if (t instanceof Daikon.TerminationMessage) {
          // XXX Why am I creating a new TerminationMessage here?
          throw new Daikon.TerminationMessage (
                      message, reader, state.filename);
        } else {
          throw new Daikon.TerminationMessage
          (String.format ("Illegal program point name '%s' (%s) in %s line %d",
             ppt_name, t.getMessage(), state.filename, reader.getLineNumber()));
        }
      }

      if (state.all_ppts.size() == 0) {
        throw new Daikon.TerminationMessage("No declarations were provided before the first sample.  Perhaps you did not supply the proper .decls file to Daikon.  (Or, there could be a bug in the front end that created the .dtrace file " + state.filename
                        + ".)");
      }

      PptTopLevel ppt = state.all_ppts.get(ppt_name);
      if (ppt == null) {
        throw new Daikon.TerminationMessage("No declaration was provided for program point " + ppt_name, state);
      }

      VarInfo[] vis = ppt.var_infos;

      // not vis.length, as that includes constants, derived variables, etc.
      // Actually, we do want to leave space for _orig vars.
      // And for the time being (and possibly forever), for derived variables.
      int num_tracevars = ppt.num_tracevars;
      int vals_array_size = ppt.var_infos.length - ppt.num_static_constant_vars;

      // Read an invocation nonce if one exists
      Integer nonce = null;

      boolean nonce_exists;
      {
        String nonce_header_peekahead;
        // arbitrary number, hopefully big enough; catch exceptions
        reader.mark(100);
        try {
          nonce_header_peekahead = reader.readLine();
        } catch (Exception e) {
          nonce_header_peekahead = null;
        }
        reader.reset();
        nonce_exists = NONCE_HEADER.equals(nonce_header_peekahead);
      }
      if (nonce_exists) {
        String nonce_header = reader.readLine();   // read & discard header
        assert NONCE_HEADER.equals(nonce_header);
        String nonce_number = reader.readLine();
        if (nonce_number == null) {
          throw new Daikon.TerminationMessage("File ended while trying to read nonce",
                                    state);
        }
        nonce = new Integer(nonce_number);

        if (Global.debugPrintDtrace) {
          to_write_nonce = true;
          nonce_value = nonce.toString();
        }
      }

      /*@Nullable*/ Object[] vals = new /*@Nullable*/ Object[vals_array_size];
      int[] mods = new int[vals_array_size];

      // Read a single record from the trace file;
      // fills up vals and mods arrays by side effect.
      try {
        read_vals_and_mods_from_trace_file (reader, state.filename,
                                            ppt, vals, mods);
      } catch (IOException e) {
        String nextLine = reader.readLine();
        if ((e instanceof EOFException) || (nextLine == null)) {
          System.out.println ();
          System.out.println ("WARNING: Unexpected EOF while processing "
                        + "trace file - last record of trace file ignored");
          state.status = ParseStatus.EOF;
          return;
        } else if (dkconfig_continue_after_file_exception) {
          System.out.println ();
          System.out.println ("WARNING: IOException while processing "
                              + "trace file - record ignored");
          System.out.print ("Ignored backtrace:");
          e.printStackTrace(System.out);
          System.out.println ();
          while (nextLine != null && ! nextLine.equals("")) {
            // System.out.println("Discarded line " + reader.getLineNumber()
            //                     + ": " + nextLine);
            nextLine = reader.readLine();
          }
          continue;
        } else {
          throw e;
        }
      }

      state.ppt = ppt;
      state.nonce = nonce;
      state.vt = ValueTuple.makeUninterned(vals, mods);
      state.status = ParseStatus.SAMPLE;
      return;
    }

    state.status = ParseStatus.EOF;
    return;
  }

  public static void echoMsg( PrintWriter pWriter, String message ) {
    if ( pWriter != null ) {
      pWriter.println( message );
    }
  }

    // Read a single binary record (declaration or sample) from a binary dtrace file.
    // pWriter( test print writer ): writes all the information written in binary
    // as human readable text
    // ParseStatus.TRUNCATED is assumed when the end of the file is reached,
    // when data is expected
    // Each record, read by the method has a byte length
    public static void read_binary_data_trace_record( ParseState state, PrintWriter pWriter )
      throws IOException {

      DataInputStream binReader = state.binReader;

      try {
        while ( true ) {
          int strLen = 0;
          try {
            strLen = binReader.readByte( );
          }
          // Only EOFException caught at the beginning of the loop
          // indicates ParseStatus.EOF
          catch ( EOFException e ) {
            state.status = ParseStatus.EOF;
            return;
          }

          try {
            // get the first (word) record from the file
            byte [] buf = new byte [strLen];
            binReader.read( buf, 0, strLen );

            String record = new String( buf );

            // Parse comments
            if ( record.equals( "//" ) || record.equals( "#" ) ) {
              int comLen = binReader.readInt( ); // gets the length of the string
              byte comBuf [] = new byte [comLen];
              binReader.read( comBuf, 0, comLen ); // gets the string
              String comment = new String( comBuf );

              if ( comment.startsWith( "#" ) ) {
                comment = "\n\n" + comment;
                echoMsg( pWriter, comment );
              }
              else
                echoMsg( pWriter, comment + "\n" );

              state.status = ParseStatus.COMMENT;
              return;
            }
            // First look for declarations
            if ( record.equals( "ppt" ) ) {
              if ( new_decl_format ) {
                state.ppt = read_binaryDeclaration( binReader, state, pWriter );
              }
              else
                state.ppt = read_declaration( state );
              // ppt can be null if this declaration was skipped because of
              // --ppt-select-pattern or --ppt-omit-pattern.
              if ( state.ppt != null ) {
                if ( !state.all_ppts.containsName (state.ppt.name()) ) {
                  state.all_ppts.add( state.ppt );
                  try {
                    Daikon.init_ppt( state.ppt, state.all_ppts );
                  } catch (Exception e) {
                    decl_error (state, "unexpected error: %s", e);
                  }
                }
              }
              state.status = ParseStatus.DECL;
              return;
            }
            if ( record.equals( "var-comparability" ) ) {

              String comp [] = get_binaryRecords( binReader, 1 );
              String line = record + " " + comp[0] + "\n";

              state.varcomp_format = read_var_comparability ( state, line );
              state.status = ParseStatus.COMPARABILITY;
              echoMsg( pWriter, (record + " " + comp[0]) );
              return;
            }
            if ( record.equals( "input-language" ) ) {

              String input_lang [] = get_binaryRecords( binReader, 1 );
              String line = record + " " + input_lang[0] + "\n";

              read_input_language ( state, line );
              state.status = ParseStatus.INPUT_LANGUAGE;
              echoMsg( pWriter, (record + " " + input_lang[0]) );
              return;
            }
            if ( record.equals( "decl-version" ) ) {

              String version [] = get_binaryRecords( binReader, 1 );
              String line = record + " " + version[0] + "\n";

              read_decl_version( state, line );
              state.status = ParseStatus.DECL_VERSION;
              echoMsg( pWriter, (record + " " + version[0]) );
              return;
            }
            if ( record.equals( "ListImplementors" ) ) {
              read_binary_list_implementors ( binReader );
              state.status = ParseStatus.LIST_IMPLEMENTORS;
              return;
            }
            // we got a program point
            // The binary file contains indexes instead of full ppt_names
            String ppt_name = pptDeclMap.get( new Integer( record ) );
            if ( ppt_name == null) {
              throw new Daikon.TerminationMessage( "read_binary_data_trace_record() " +
                                                   " not found ppt_name in file " + state.filename );
            }
            echoMsg( pWriter, "" );
            String msg = ppt_name;

            if ( bin2ascii_dbg )
              msg += " " + record;
            echoMsg( pWriter, msg );

            // If we got here, we're looking at a sample and not a declaration.
            // Start processing the data sample
            // Parse the ppt name
            try {
              new PptName( ppt_name );
            } catch (Throwable t) {
              if ( t instanceof Daikon.TerminationMessage )
                throw new Daikon.TerminationMessage ( t.getMessage() + ": in " + state.filename );
              else
                throw new Daikon.TerminationMessage
                    ( String.format ( "Illegal program point name '%s' (%s) in %s ",
                                      ppt_name, t.getMessage(), state.filename ) );
            }

            if ( state.all_ppts.size() == 0 ) {
              throw new Daikon.TerminationMessage("No declarations were provided before the first sample. " +
                                                  "Perhaps you did not supply the proper .decls file to Daikon. " +
                                                  "(Or, there could be a bug in the front end that created the .dtrace file "
                                                  + state.filename
                                                  + ".)");
            }

            PptTopLevel ppt = state.all_ppts.get( ppt_name );
            if ( ppt == null ) {
              throw new Daikon.TerminationMessage( "No declaration was provided for program point " + ppt_name
                                                   + " which appears in binary dtrace file " + state.filename );
            }

            // not vis.length, as that includes constants, derived variables, etc.
            // Actually, we do want to leave space for _orig vars.
            // And for the time being (and possibly forever), for derived variables.
            int num_tracevars = ppt.num_tracevars;
            int vals_array_size = ppt.var_infos.length - ppt.num_static_constant_vars;

            // Read an invocation nonce if one exists
            Integer nonce = null;
            int nonce_maybe;
            try {
              nonce_maybe = binReader.readInt( );
            } catch ( IOException e ) {
              nonce_maybe = 0;
            }

            echoMsg( pWriter, ("this_invocation_nonce\n" + nonce_maybe) );
            nonce = new Integer( nonce_maybe );

            if (Global.debugPrintDtrace) {
                to_write_nonce = true;
                nonce_value = nonce.toString();
            }

            Object[] vals = new Object[vals_array_size];
            int[] mods = new int[vals_array_size];

            // Read a single record from the trace file;
            // fills up vals array by side effect.
            read_vals_from_binary_dtrace_file( binReader, state.filename,
                                               ppt, vals, mods, pWriter );

            if ( ppt_included( ppt_name ) ) {
              state.ppt = ppt;
              state.nonce = nonce;
              state.vt = ValueTuple.makeUninterned(vals, mods);
              state.status = ParseStatus.SAMPLE;
              return;
            }
            else
              continue;

          } catch ( EOFException e ) {
            state.status = ParseStatus.TRUNCATED;
            return;
          }
        }

      } catch ( IOException e ) {
        e.printStackTrace( System.out );
      }
      return;
    }


    // Returns an array of bytes, where the mod bits are stored
    // for example, an int mods array =  { 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0 }
    // with length of 22 will be represented as an array of 3 bytes (i.e. 24 bits)
    // byteBuf[0] = LSB 10100111 MSB
    // byteBuf[1] = LSB 00000011 MSB
    // byteBuf[2] = LSB 10100011 MSB, where the last two bits (two extra 1s) are added to form a byte
    // MISSING_NONSENSICAL is recorded as 0s
    private static byte [] get_modBits( int mods [] ) {

	int arrLen = mods.length;
	int bits;

	if ( ( bits = arrLen%8 ) != 0 ) {
	    arrLen += 8 - bits;
	}

	byte ext_mods [] = new byte [arrLen];
	int i = 0;
	for ( ; i < mods.length; i++ ) {

	  if ( mods[i] == ValueTuple.MISSING_NONSENSICAL )
	      ext_mods[i] = 1;
	  else
	      ext_mods[i] = 0;
	}

	while ( i < arrLen )
	    ext_mods[i++] = 0;

	int bufLen = arrLen/8;
	byte buf [] = new byte [bufLen];
	byte masks [] = { (byte)0xfe, (byte)0xfd, (byte)0xfb, (byte)0xf7, (byte)0xef, (byte)0xdf, (byte)0xbf, (byte)0x7f };

	for ( int k = 0; k < arrLen ; k += 8 ) {
	    int j = k/8;
	    buf[j] = ( byte )0xff;
	    for ( int l = 0; l < 8; l++ ) {
		byte val = ( byte ) ( ( byte )( ~ext_mods[k+l] ) << l );
		val |= masks[l];
		buf[j] &= val;
	    }
        }

	return buf;
    }

    // Returns a buffer containing the individual bits of the byte array
    private static StringBuffer recordBits( byte mod_bits [] ) {
	int len = mod_bits.length;
	byte bits [] = new byte [len];
	System.arraycopy( mod_bits, 0, bits, 0, len );
	byte mask = 1;
        StringBuffer str_buf = new StringBuffer( 8 * bits.length );

        for ( int i = 0; i < bits.length; i++ ) {
            for ( int j = 0; j < 8; j++ ) {
                str_buf.append( ( bits[i] & mask ) == 0 ? '0' : '1' );
                bits[i] >>= 1;
            }
        }
	return str_buf;
    }

    static Map<String, Integer> pptNamesMap = new HashMap<String, Integer>( );
    static int pptNamesIndex = 0;
    static boolean bin2ascii_dbg = false;

   /**
    * Converts a dtrace file sample into binary data as follows:
    * Records the ppt_name as an integer index (these indexes are hashed by ppt_names)
    * The integer index is recorded as one byte length, followed by a sequence of bytes
    * (i.e. index 23 occupies 3 bytes: 1 byte length (length=2) and 2 bytes for the chars (char1=2, char2=3)
    * Records the nonce value as 4 byte integer
    *
    * Records the mod bits, stored in an array of bytes (one byte contains 8 mod bits)
    * The array length is recorded first as one byte, then the array itself
    * Variables, whose values are MISSING_NONSENSICAL aren't recorded in the binary file
    *
    * Records the String value as 1 word (4 bytes) for the number of characters,
    * then the characters as a sequence of bytes
    * if String value = null, number of bytes = -1 is recorded
    * Records the Array length as 1 word,
    * then the Array elements, depending on their type
    * Records boolean, char as one byte
    * Records int, Integer, Hashcode as one word (4 bytes)
    * Records long, Long, Double, Object as two words (8 bytes)
   **/
    public static void write_binarySample( PrintWriter pWriter, DataOutputStream binWriter,
					   PptMap all_ppts,
					   PptTopLevel ppt,
					   ValueTuple vt,
					   Integer nonce ) {

      Collection<PptTopLevel> allPptTopLevels = all_ppts.asCollection( );
      echoMsg( pWriter, "" ); // new line

      String ppt_index = "";
      if ( bin2ascii_dbg )
        ppt_index = " " +  pptNamesMap.get( ppt.ppt_name.getName( ) );

      echoMsg( pWriter, (ppt.ppt_name.getName( ) + ppt_index) );


      // uses the key ( ppt_name ) to retrieve the integer index
      String hash_pptName = "" + pptNamesMap.get( ppt.ppt_name.getName( ) );

      try {
	// records the program point index
      	binWriter.writeByte( hash_pptName.length( ) );
	binWriter.writeBytes( hash_pptName );

        if ( nonce != null ) {
          echoMsg( pWriter, ("this_invocation_nonce\n" + nonce.intValue( )) );
	  binWriter.writeInt( nonce.intValue( ) );
        }
        else {
          // the binReader will always expect a 4 byte value for the nonce
	  binWriter.writeInt( 0 );
        }

        int mods [] = vt.mods; // get modes from the ValueTuple to check for "nonsensical"

        if ( bin2ascii_dbg ) {
	  String vt_mods = "";
	  for ( int k = 0; k < mods.length; k++ )
	    vt_mods += mods[k];
      	  echoMsg( pWriter, "vt_mods: " + vt_mods );
      }

      // This is true if there are static constants, because
      // their mods aren't recorded
      //if ( mods.length != ppt.var_infos.length )
      //  echoMsg( pWriter, "ERROR: FileIO: write_binarySample: mods.length != var_infos.length : "
      //
      //	   + mods.length + " != " + ppt.var_infos.length );

        byte mod_bits [] = get_modBits( mods );
        StringBuffer mod_str = recordBits( mod_bits );
        if ( bin2ascii_dbg )
          echoMsg( pWriter, "recorded_mods: " + mod_str.toString( ) );

        // records the mod bits
        binWriter.writeByte( mod_bits.length );
        binWriter.write( mod_bits, 0, mod_bits.length );

        int index_mods = 0; // index in mods[]

        // go through all variables in the given program point
        for ( int i = 0; i < ppt.var_infos.length; i++, index_mods++ ) {

	  VarInfo vInfo = ppt.var_infos[i];
	  ProglangType repType = vInfo.file_rep_type;

	  // if a variable is a static const
	  // then it is not included in the dtrace file
	  if ( vInfo.is_static_constant ) {
	    index_mods--;
	    continue;
	  }

	  if ( mods[index_mods] != ValueTuple.MISSING_NONSENSICAL ) {

	    Object value = ppt.var_infos[i].getValue( vt );
	    String valid_mod = "1";

            if ( value instanceof String ) {
              String vs = ( String ) value;
              echoMsg( pWriter, (vInfo.name( ) + "\n" + "\"" + vs + "\"") );
              echoMsg( pWriter, valid_mod );

              if ( vs != null ) {
                binWriter.writeInt( vs.length( ) );
                binWriter.writeBytes( vs );
              }
              else
                binWriter.writeInt( -1 );
            }
            /*
            else if ( value instanceof char [][] ) {
              char cha [][] = ( char [][] ) value;
              echoMsg( pWriter, vInfo.name( ) );
              binWriter.writeInt( cha.length );
              String msg = "[";

              for ( int k = 0; k < cha.length; k++ ) {
                binWriter.writeByte( ( byte ) cha[k].length );
                for ( int l = 0; l < cha[k].length; l++ ) {
                  msg += cha[k][l] + " ";
                  binWriter.writeByte( ( char ) cha[k][l] );
                }
              }
              msg = msg.trim() + "]";
              echoMsg( pWriter, msg );
              echoMsg( pWriter, valid_mod );
              }*/
            else if ( value instanceof long [] ) {
              long va [] = ( long [] ) value;
	      echoMsg( pWriter, vInfo.name( ) );
              binWriter.writeInt( va.length );
              String msg = "[";

              if ( repType == ProglangType.INT_ARRAY ) {
                for ( int j = 0; j < va.length; j++ ) {
                  msg += va[j] + " ";
                  binWriter.writeInt( ( int ) va[j] );
                }
              }
              else if ( repType == ProglangType.CHAR_ARRAY ) {
                for ( int j = 0; j < va.length; j++ ) {
                  msg += va[j] + " ";
                  binWriter.writeByte( ( char ) va[j] );
                }
              }
              else if ( repType == ProglangType.HASHCODE_ARRAY ) {
                for ( int j = 0; j < va.length; j++ ) {
                  binWriter.writeInt( ( int ) va[j] );
                  if ( va[j] != 0 )
                    msg += va[j] + " ";
                  else
                    msg += "null" + " ";
                }
              }
              else if ( repType == ProglangType.BOOLEAN_ARRAY ) {
                for ( int j = 0; j < va.length; j++ ) {
                    binWriter.writeByte( ( byte ) va[j] );
                    if ( va[j] == 1 )
                        msg += "true" + " ";
                    else
                        msg += "false" + " ";
                }
              }
              else {
                for ( int j = 0; j < va.length; j++ ) {
                  msg += va[j] + " ";
                  binWriter.writeLong( va[j] );
                }
              }
              msg = msg.trim() + "]";
              echoMsg( pWriter, msg );
              echoMsg( pWriter, valid_mod );
            }
            else if ( value instanceof double [] ) {
              double va [] = ( double [] ) value;
              echoMsg( pWriter, vInfo.name( ) );
              binWriter.writeInt( va.length );
              String msg = "[";

              for ( int j = 0; j < va.length; j++ ) {
                msg += va[j] + " ";
                binWriter.writeDouble( va[j] );
              }
              msg = msg.trim() + "]";
              echoMsg( pWriter, msg );
              echoMsg( pWriter, valid_mod );
            }
	    else if ( value instanceof String [] ) {
              String va [] = ( String [] ) value;
              echoMsg( pWriter, vInfo.name( ) );
              binWriter.writeInt( va.length );
              String msg = "[";

              for ( int j = 0; j < va.length; j++ ) {
                if ( va[j] != null ) {
                  msg += "\"" + va[j] + "\"" + " ";
                  binWriter.writeByte( va[j].length( ) );
                  binWriter.writeBytes( va[j] );
                }
                else {
                  msg += "null" + " ";
                  // zero length, means that the string is null;
                  binWriter.writeByte( -1 );
                }
              }
              msg = msg.trim() + "]";
              echoMsg( pWriter, msg );
              echoMsg( pWriter, valid_mod );
            }
            else if ( value != null ) {
              echoMsg( pWriter, vInfo.name( ) );

              String sVal = value.toString( );

              if ( repType == ProglangType.BOOLEAN ) {
                binWriter.writeByte( sVal.charAt( 0 ) );

                sVal = ( sVal.charAt( 0 ) == '1' ) ? "true" : "false";
                echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.CHAR ) {
                binWriter.writeByte( sVal.charAt( 0 ) );
                echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.DOUBLE ) {
                binWriter.writeDouble( Double.valueOf( sVal ) );
                echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.LONG_PRIMITIVE ) {
                binWriter.writeLong( Long.valueOf( sVal ) );
                echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.INT ) {
                binWriter.writeInt( Integer.valueOf( sVal ) );
                echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.INTEGER ) {
                binWriter.writeInt( Integer.valueOf( sVal ) );
                echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.LONG_OBJECT ) {
                binWriter.writeLong( Long.valueOf( sVal ) );
                echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.OBJECT ) {
                binWriter.writeLong( Long.valueOf( sVal ) );
                echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.HASHCODE ) {
	        binWriter.writeInt( Integer.valueOf( sVal ) );
                if ( Integer.valueOf( sVal ) == 0 )
                    sVal = "null";
                echoMsg( pWriter, sVal + "\n" + valid_mod );
	       }
              else {
                throw new Daikon.TerminationMessage( "value of variable " +
                                                     vInfo.name( ) +
                                                     " was of unspecified ProglangType" );
                //echoMsg( pWriter, "was of unspecified type" );
              }
            }
            else {
              // There are vars, whose values, which are null
              // and aren't recorded in the dtrace file
              // probably these values have no mod bits
              index_mods--;
              // echoMsg( pWriter, "value = null variable name = " + vInfo.name( ) );
            }
          }
          else {
           echoMsg( pWriter, vInfo.name( ) );
           echoMsg( pWriter, "nonsensical" );
           echoMsg( pWriter, "" + ValueTuple.MISSING_NONSENSICAL );
          }
        }

      } catch ( IOException e ) {
	  e.printStackTrace( System.out );
      }
  }


  /**
   * Add orig() and derived variables to vt (by side effect), then
   * supply it to the program point for flowing.
   * @param vt trace data only; modified by side effect to add derived vars
   **/
  public static void process_sample(
                                    PptMap all_ppts,
                                    PptTopLevel ppt,
                                    ValueTuple vt,
                                    Integer nonce) {

    // Add orig variables.  This must be above the check below because
    // it saves away the orig values from enter points for later use
    // by exit points.
    boolean ignore = add_orig_variables(ppt, vt.vals, vt.mods, nonce);
    if (ignore)
      return;

    // Only process the leaves of the ppt tree.
    // This test assumes that all leaves are numbered exit program points
    // -- that is, points of the form foo:::EXIT22 for which isExitPoint()
    // is true and isCombinedExitPoint() is false.  "Combined" exit points
    // of the form foo:::EXIT are not processed -- they are assumed to be
    // non-leaves.
    if (Daikon.use_dataflow_hierarchy) {

      // Rather than defining leaves as :::EXIT54 (numbered exit)
      // program points define them as everything except
      // ::EXIT (combined), :::ENTER, :::THROWS, :::OBJECT, ::GLOBAL
      //  and :::CLASS program points.  This scheme ensures that arbitrarly
      //  named program points such as :::POINT (used by convertcsv.pl)
      //  will be treated as leaves.

      if (ppt.ppt_name.isEnterPoint() ||
          ppt.ppt_name.isThrowsPoint() ||
          ppt.ppt_name.isObjectInstanceSynthetic() ||
          ppt.ppt_name.isClassStaticSynthetic() ||
          ppt.ppt_name.isGlobalPoint()) {
        return;
      }

      if (ppt.ppt_name.isExitPoint() && ppt.ppt_name.isCombinedExitPoint()) {
        // not Daikon.TerminationMessage; caller has more info (e.g., filename)
        throw new RuntimeException("Bad program point name " + ppt.name
                                   + " is a combined exit point name");
      }
    }

    // Add derived variables
    add_derived_variables(ppt, vt.vals, vt.mods);

    // Causes interning
    vt = new ValueTuple(vt.vals, vt.mods);

    if (debugRead.isLoggable(Level.FINE)) {
      debugRead.fine ("Adding ValueTuple to " + ppt.name());
      debugRead.fine ("  length is " + vt.vals.length);
    }

    // If we are only reading the sample, don't process them
    if (dkconfig_read_samples_only) {
      return;
    }

    // If this is an unitialized basic block ppt that is part of a
    // function, initialize the relationships between basic blocks
    // in the same function.  Note that all declarations for basic
    // block ppts must have been received before data is recieved for
    // any block in the same function
    if (ppt.is_basic_block() && !ppt.combined_ppts_init
        && (ppt.function_id != null)) {
      if (!dkconfig_merge_basic_blocks) {
        List<PptTopLevel> ppts = func_ppts.get (ppt.function_id);
        assert ppts != null;
        for (PptTopLevel p : ppts) {
          p.combined_subsumed = false;
          p.combined_ppts_init = true;
        }
      } else {
        // Sanity check the ppts in this function
        List<PptTopLevel> ppts = func_ppts.get (ppt.function_id);
        assert ppts != null : ppt.name() + " func id " + ppt.function_id;
        assert ppts.size() > 0 : ppt.name();
        for (PptTopLevel p : ppts) {
          assert !p.combined_ppts_init : p.name();
          if (p.ppt_successors != null) {
            for (Iterator<String> it = p.ppt_successors.iterator();
                 it.hasNext(); ) {
              String successor = it.next();
              PptTopLevel sp = all_ppts.get (successor);
              if (sp == null) {
                System.out.printf ("Warning: successor %s in ppt %s does not "
                                   + "exist, removing\n", successor, p.name());
                assert false;
                it.remove();
              } else {
                assert sp != null : successor;
                @SuppressWarnings("interning") // PptTopLevel
                boolean same_function_id = (sp.function_id == p.function_id);
                if (! same_function_id) {
                  System.out.printf ("Warning: successor %s (func %s) in "
                            + "ppt %s (func %s) is not in same function\n",
                            sp.name(), sp.function_id, p.name(), p.function_id);
                  assert false;
                  it.remove();
                } else {
                  assert same_function_id
                    : sp.function_id + " " + p.function_id;
                }
              }
            }
          }
        }
        if (true) {
          System.out.printf ("Building combined ppts for func %s [ppt %s]\n",
                             ppt.function_id, ppt.name());
          System.out.println ("Successor graph:");
          for (PptTopLevel p : ppts) {
            System.out.printf ("  %s\n", p.name());
            if (p.ppt_successors != null) {
              for (String successorName : p.ppt_successors) {
                @SuppressWarnings("nullness") // because successorName is in p.ppt_successors
                /*@NonNull*/ PptTopLevel successorPpt = all_ppts.get (successorName);
                System.out.printf ("    %s\n", successorPpt.name());
              }
            }
            p.combined_ppts_init = true;
          }
        }

        // Compute predecessors for each basic block.
        for (PptTopLevel p : ppts) {
          p.predecessors = new ArrayList<PptTopLevel>();
        }
        for (PptTopLevel p : ppts) {
          if (p.ppt_successors != null) {
            for (String succName : p.ppt_successors) {
              PptTopLevel succPpt = all_ppts.get(succName);
              assert succPpt != null;
              assert succPpt.predecessors != null;
              succPpt.predecessors.add(p);
            }
          }
        }
        if (dkconfig_check_bb_connections) {
          // The function entry should be able should be strongly connected
          for (int i = 1; i < ppts.size(); i++) {
            PptTopLevel p = ppts.get(i);
            if (!ppts.get(0).connected (p))
              System.out.printf ("ERROR: ppt %s in func %s is not connected\n",
                                 p, ppts.get(0));
          }
          // Every block except the first should have at least one predecessor
          for (int i = 1; i < ppts.size(); i++) {
            PptTopLevel p = ppts.get(i);
            assert p.predecessors != null;
            if (p.predecessors.size() == 0)
              System.out.printf ("ERROR: ppt %s has no predecessors\n", p);
          }
        }

        // Build any combined program points and add them to the global map
        System.out.printf ("Calling combine_func_ppts for function %s:\n",
                           ppts.get(0).name());
        PptCombined.combine_func_ppts (all_ppts, ppts);

        System.out.printf ("Basic blocks in function %s:\n",
                           ppts.get(0).name());
        PptCombined.dump (ppts);
        assert PptCombined.check_func_ppts (ppts);
        // PptCombined.check_func_ppts (ppts);
        for (PptTopLevel p : ppts) {
          if (p.combined_subsumed)
            continue;
          if (p.combined_ppt == null) {
            System.out.printf ("ERROR: no combined ppt for %s\n", p);
            continue;
          }
          assert all_ppts.get (p.combined_ppt.name()) == null
            : p.combined_ppt.name();
          all_ppts.add (p.combined_ppt);
          p.combined_ppt.dump();
          assert p.combined_ppt.check();
          // p.combined_ppt.check();
        }
      }
    }

    // merging basic blocks
    if (dkconfig_merge_basic_blocks) {

      // If this is a basic block, remember its values
      if (ppt.is_basic_block()) {
        ppt.last_values = vt;
      }

      // Add the sample to the ppt.  Ppts that are part of a combined ppt
      // are handled as part of the combined ppt.
      if (!ppt.combined_subsumed && (ppt.combined_ppt != null)) {
        ppt.combined_ppt.add_combined();
      }
    } else {
      ppt.add_bottom_up (vt, 1);
    }


    if (debugVars.isLoggable (Level.FINE))
      debugVars.fine (ppt.name() + " vars: " + Debug.int_vars (ppt, vt));

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.close();
    }
  }


  /** Returns true if this procedure has an unmatched entry. **/
  @SuppressWarnings("interning") // PptTopLevel
  static boolean has_unmatched_procedure_entry(PptTopLevel ppt) {
    for (Invocation invok : call_hashmap.values()) {
      if (invok.ppt == ppt) {
        return true;
      }
    }
    for (Invocation invok : call_stack) {
      if (invok.ppt == ppt) {
        return true;
      }
    }
    return false;
  }


  /**
   * Print each call that does not have a matching exit
   */
  public static void process_unmatched_procedure_entries() {

    if (dkconfig_unmatched_procedure_entries_quiet)
      return;

    int unmatched_count = call_stack.size() + call_hashmap.size();

    if ((!call_stack.empty()) || (!call_hashmap.isEmpty())) {
      System.out.println();
      System.out.print(
        "No return from procedure observed "
          + UtilMDE.nplural(unmatched_count, "time") + ".");
      if (Daikon.use_dataflow_hierarchy) {
        System.out.print("  Unmatched entries are ignored!");
      }
      System.out.println();
      if (!call_hashmap.isEmpty()) {
        System.out.println("Unterminated calls:");
        if (dkconfig_verbose_unmatched_procedure_entries) {
          // Print the invocations in sorted order.
          // (Does this work?  The keys are integers. -MDE 7/1/2005.)
          ArrayList<Invocation> invocations = new ArrayList<Invocation>();
          TreeSet<Integer> keys = new TreeSet<Integer>(call_hashmap.keySet());
          for (Integer i : keys) {
            Invocation invok = call_hashmap.get(i);
            assert invok != null; // for nullness checker; sorted keyset
            invocations.add(invok);
          }
          print_invocations_verbose(invocations);
        } else {
          print_invocations_grouped(call_hashmap.values());
        }
      }

      if (!call_stack.empty()) {
        if (dkconfig_verbose_unmatched_procedure_entries) {
          System.out.println("Remaining " +
                             UtilMDE.nplural(unmatched_count, "stack")
                             + " call summarized below.");
          print_invocations_verbose(call_stack);
        } else {
          print_invocations_grouped(call_stack);
        }
      }
      System.out.print("End of report for procedures not returned from.");
      if (Daikon.use_dataflow_hierarchy) {
        System.out.print("  Unmatched entries are ignored!");
      }
      System.out.println();
    }
  }

  /** Print all the invocations in the collection, in order. **/
  static void print_invocations_verbose(Collection<Invocation> invocations) {
    for (Invocation invok : invocations) {
      System.out.println(invok.format());
    }
  }

  /**
   * Print the invocations in the collection, in order, and
   * suppressing duplicates.
   **/
  static void print_invocations_grouped(Collection<Invocation> invocations) {
    Map</*@Interned*/ Invocation,Integer> counter = new HashMap</*@Interned*/ Invocation,Integer>();

    for (Invocation invok_noncanonical : invocations) {
      /*@Interned*/ Invocation invok = invok_noncanonical.canonicalize();
      if (counter.containsKey(invok)) {
        Integer oldCount = counter.get(invok);
        Integer newCount = new Integer(oldCount.intValue() + 1);
        counter.put(invok, newCount);
      } else {
        counter.put(invok, new Integer(1));
      }
    }

    // Print the invocations in sorted order.
    TreeSet</*@Interned*/ Invocation> keys = new TreeSet</*@Interned*/ Invocation>(counter.keySet());
    for (/*@Interned*/ Invocation invok : keys) {
      Integer count = counter.get(invok);
      assert count != null;     // for nullness checker:  use of sorted keyset
      System.out.println(invok.format(false) + " : "
                         + UtilMDE.nplural(count.intValue(), "invocation"));
    }
  }

  // This procedure reads a single record from a trace file and
  // fills up vals and mods by side effect.  The ppt name and
  // invocation nonce (if any) have already been read.
  private static void read_vals_and_mods_from_trace_file
                        (LineNumberReader reader, String filename,
                         PptTopLevel ppt, /*@Nullable*/ Object[] vals, int[] mods)
    throws IOException
  {
    // Note:  global variable data_trace_state may be null (at least in the
    // unit tests...).
    //assert data_trace_state != null; // added to test actual execution

    VarInfo[] vis = ppt.var_infos;
    int num_tracevars = ppt.num_tracevars;

    String[] oldvalue_reps = ppt_to_value_reps.get(ppt);
    if (oldvalue_reps == null) {
      // We've not encountered this program point before.  The nulls in
      // this array will compare non-equal to whatever is in the trace
      // file, which is the desired behavior.
      oldvalue_reps = new String[num_tracevars];
    }

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.println(ppt.name());

      if (to_write_nonce) {
        Global.dtraceWriter.println(NONCE_HEADER);
        Global.dtraceWriter.println(nonce_value);
        to_write_nonce = false;
      }
    }

    for (int vi_index = 0, val_index = 0;
      val_index < num_tracevars;
      vi_index++) {
      assert vi_index < vis.length
        : "Got to vi_index " + vi_index + " after " + val_index + " of " + num_tracevars + " values";
      VarInfo vi = vis[vi_index];
      assert (!vi.is_static_constant) || (vi.value_index == -1)
        // : "Bad value_index " + vi.value_index + " when static_constant_value = " + vi.static_constant_value + " for " + vi.repr() + " at " + ppt_name
        ;
      if (vi.is_static_constant)
        continue;
      assert val_index == vi.value_index
        // : "Differing val_index = " + val_index
        // + " and vi.value_index = " + vi.value_index
        // + " for " + vi.name + lineSep + vi.repr()
        ;

      // In errors, say "for program point", not "at program point" as the
      // latter confuses Emacs goto-error.

      String line = reader.readLine();
      if (line == null) {
        throw new Daikon.TerminationMessage(
          "Unexpected end of file at "
            + data_trace_state.filename
            + " line "
            + reader.getLineNumber() + lineSep
            + "  Expected variable "
            + vi.name()
            + ", got "
            + "null" // line
            + " for program point "
            + ppt.name());
      }

      // Read lines until an included variable is found
      while ((line != null)
             && !line.equals("")
             && !var_included(line)) {
        line = reader.readLine(); // value (discard it)
        line = reader.readLine(); // modbit
        if (line == null
            || !((line.equals("0") || line.equals("1") || line.equals("2")))) {
          throw new Daikon.TerminationMessage("Bad modbit '" + line + "'",
                                              data_trace_state);
        }
        line = reader.readLine(); // next variable name
      }

      if (!line.trim().equals (vi.str_name())) {
        throw new Daikon.TerminationMessage(
          "Mismatch between .dtrace file and .decls file.  Expected variable "
            + vi.name()
            + ", got "
            + line
            + " for program point "
          + ppt.name(),
          data_trace_state);
      }
      line = reader.readLine();
      if (line == null) {
        throw new Daikon.TerminationMessage(
          "Unexpected end of file at "
            + data_trace_state.filename
            + " line "
            + reader.getLineNumber() + lineSep
            + "  Expected value for variable "
            + vi.name()
            + ", got "
            + "null" // line
            + " for program point "
            + ppt.name());
      }
      String value_rep = line;
      line = reader.readLine();
      if (line == null) {
        throw new Daikon.TerminationMessage(
          "Unexpected end of file at "
            + data_trace_state.filename
            + " line "
            + reader.getLineNumber() + lineSep
            + "  Expected modbit for variable "
            + vi.name()
            + ", got "
            + "null" // line
            + " for program point "
            + ppt.name());
      }
      if (!((line.equals("0") || line.equals("1") || line.equals("2")))) {
        throw new Daikon.TerminationMessage("Bad modbit `" + line + "'",
                                  data_trace_state);
      }
      int mod = ValueTuple.parseModified(line);

      // System.out.println("Mod is " + mod + " at " + data_trace_state.filename + " line " + reader.getLineNumber());
      // System.out.pringln("  for variable " + vi.name()
      //                   + " for program point " + ppt.name());

      // MISSING_FLOW is only found during flow algorithm
      assert mod != ValueTuple.MISSING_FLOW
        : "Data trace value can't be missing due to flow";

      if (mod != ValueTuple.MISSING_NONSENSICAL) {
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
        Global.dtraceWriter.println(vi.name());
        Global.dtraceWriter.println(value_rep);
        Global.dtraceWriter.println(mod);
      }
      Debug dbg = Debug.newDebug(FileIO.class, ppt, Debug.vis(vi));
      if (dbg != null)
        dbg.log(
          "Var " + vi.name() + " has value " + value_rep + " mod " + mod);

      // Both uninit and nonsensical mean missing modbit 2, because
      // it doesn't make sense to look at x.y when x is uninitialized.
      if (ValueTuple.modIsMissingNonsensical(mod)) {
        if (!(value_rep.equals("nonsensical")
              // Kvasir still uses "uninit" (it distinguishes between
              // uninit and nonsensical), though the Daikon manual does not
              // officially permit "uninit" as a value and has not since at
              // least 2002.  This is fixed in the Kvasir repository as of
              // 5/2009, so the following two lines should be removed at
              // some point not too long after that.  Then Daikon should
              // print a warning (or even terminate execution) about uses
              // of "uninit".
              || value_rep.equals("uninit")
              || value_rep.equals("missing"))) {
          throw new Daikon.TerminationMessage(
            "Modbit indicates nonsensical value for variable "
              + vi.name() + " with value \"" + value_rep + "\";" + lineSep
              + "  text of value should be \"nonsensical\"",
              data_trace_state);
        } else {
          // Keep track of variables that can be missing
          if (debug_missing && !vi.canBeMissing) {
              System.out.printf ("Var %s ppt %s at line %d missing%n",
                               vi, ppt.name(),
                               FileIO.get_linenum());
              System.out.printf ("val_index = %d, mods[val_index] = %d%n",
                                 val_index, mods[val_index]);
          }
          vi.canBeMissing = true;
        }
        vals[val_index] = null;
      } else {
        // System.out.println("Mod is " + mod + " (missing=" +
        // ValueTuple.MISSING + "), rep=" + value_rep +
        // "(modIsMissing=" + ValueTuple.modIsMissing(mod) + ")");

        try {
          vals[val_index] = vi.rep_type.parse_value(value_rep);
          if (vals[val_index] == null) {
            mods[val_index] = ValueTuple.MISSING_NONSENSICAL;
            if (debug_missing && !vi.canBeMissing)
              System.out.printf ("Var %s ppt %s at line %d null-not missing%n",
                               vi, ppt.name(),
                               FileIO.get_linenum());
            vi.canBeMissing = true;
          }
        } catch (Exception e) {
          throw new Daikon.TerminationMessage(e,
            "Error while parsing value "
              + value_rep
              + " for variable "
              + vi.name()
              + " of type "
              + vi.rep_type
              + ": "
              + e.toString(),
            reader,
            filename);
        }
      }
      val_index++;

    }

    ppt_to_value_reps.put(ppt, oldvalue_reps);

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.println();
    }

    // Expecting the end of a block of values.
    String line = reader.readLine();
    // First, we might get some variables that ought to be omitted.
    while ((line != null)
           && !line.equals("")
           && !var_included(line)) {
      line = reader.readLine(); // value
      line = reader.readLine(); // modbit
      line = reader.readLine(); // next variable name
    }
    assert (line == null) || (line.equals(""))
      : "Expected blank line at line " + reader.getLineNumber() + ": " + line;
  }


  // This procedure reads a single record from a trace file and
  // fills up vals and mods by side effect.  The ppt name and
  // invocation nonce (if any) have already been read.
  private static void read_vals_from_binary_dtrace_file
    ( DataInputStream binReader, String filename,
      PptTopLevel ppt, Object[] vals, int[] mods, PrintWriter pWriter )
      throws IOException {

    VarInfo[] vis = ppt.var_infos;
    int num_tracevars = ppt.num_tracevars;

    String[] oldvalue_reps = ppt_to_value_reps.get( ppt );
    if (oldvalue_reps == null) {
      // We've not encountered this program point before.  The nulls in
      // this array will compare non-equal to whatever is in the trace
      // file, which is the desired behavior.
      oldvalue_reps = new String[num_tracevars];
    }

    // read the mod bytes
    int mods_len = binReader.readByte( );
    byte mod_bits [] = new byte [mods_len];

    // these are the mods for all variables
    // including the skipped ones
    // so in order to get the mod for the right variable
    // we need all variables, not just the not-skipped ones
    // contained in var_infos

    binReader.read( mod_bits, 0, mods_len );
    StringBuffer mods_buf = recordBits( mod_bits );
    if ( bin2ascii_dbg )
      echoMsg( pWriter, mods_buf.toString( ) );

    //mods_buf.charAt( allVar_index ) == 0 )
    int allVar_index = 0;

    for (int vi_index = 0, val_index = 0; val_index < num_tracevars; vi_index++) {

      assert vi_index < vis.length
                        // , "Got to vi_index " + vi_index + " after " + val_index
                        // + " of " + num_tracevars + " values"
                        ;
      VarInfo vi = vis[vi_index];
      assert (!vi.is_static_constant) || (vi.value_index == -1)
                        // , "Bad value_index " + vi.value_index + " when static_constant_value = "
                        // + vi.static_constant_value + " for " + vi.repr() + " at " + ppt_name
                        ;
      // static constants aren't written neither in DTrace, nor in Binary files
      // this check seems unnecessary
      if ( vi.is_static_constant )
        continue;

      assert val_index == vi.value_index
                        // , "Differing val_index = " + val_index
                        // + " and vi.value_index = " + vi.value_index
                        // + " for " + vi.name + lineSep + vi.repr()
                        ;

      String elements = null;

      while ( true ) {
        // only variables with sensical mods are written in the binary file
        for ( ; allVar_index < allVars.size( ); allVar_index++ ) {
          if( var_included( allVars.get( allVar_index ) ) ) {
            break;
          }
          if ( mods_buf.charAt( allVar_index ) != '0' ) {
            break;
           }
         }
        if ( allVar_index >= allVars.size( ) ) {
          elements = null;
          break;
        }
        if ( mods_buf.charAt( allVar_index ) == '0' ) {
          elements = "";
          break;
        }
        // read the variable from the file
        elements = get_variableValue( vi, binReader, pWriter );

        if( var_included( allVars.get( allVar_index ) ) ) {
          break;
        }
        allVar_index++;
      }

      if ( elements == null) {
        throw new Daikon.TerminationMessage(
                                            "Unexpected end of file at "
                                            + data_trace_state.filename
                                            + "  Expected value for variable "
                                            + vi.name()
                                            + ", got "
                                            + "null"
                                            + " for program point "
                                            + ppt.name());
      }

      String value_rep = elements;
      int mod = 1;

      if ( mods_buf.charAt( allVar_index ) == '0' ) {
        mod = 2;
        value_rep = "nonsensical";
        echoMsg( pWriter, (vi.name( ) + "\n" + value_rep) );
      }

      if ( mod != ValueTuple.MISSING_NONSENSICAL ) {
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

      echoMsg( pWriter, "" + mod );

      Debug dbg = Debug.newDebug(FileIO.class, ppt, Debug.vis(vi));
      if (dbg != null)
        dbg.log(
		"Var " + vi.name() + " has value " + value_rep + " mod " + mod);

      // Both uninit and nonsensical mean missing modbit 2, because
      // it doesn't make sense to look at x.y when x is uninitialized.
      if (ValueTuple.modIsMissingNonsensical(mod)) {
        if (!(value_rep.equals("nonsensical")
          || value_rep.equals("uninit") // backward compatibility (9/27/2002)
          || value_rep.equals("missing"))) {
          throw new Daikon.TerminationMessage(
            "Modbit indicates missing value for variable "
              + vi.name() + " with value \"" + value_rep + "\";" + lineSep
            + "  text of value should be \"nonsensical\" or \"uninit\" at "
              + data_trace_state.filename );
        } else {
          // Keep track of variables that can be missing
          if (debug_missing && !vi.canBeMissing) {
	      System.out.printf ("Var %s ppt %s", vi, ppt.name() );

              System.out.printf ("val_index = %d, mods[val_index] = %d%n",
                                 val_index, mods[val_index]);
          }
          vi.canBeMissing = true;
        }
	vals[val_index] = null;
      }
      else {
        try {
          vals[val_index] = vi.rep_type.parse_value(value_rep);
          if (vals[val_index] == null) {
            mods[val_index] = ValueTuple.MISSING_NONSENSICAL;
            if (debug_missing && !vi.canBeMissing)
              System.out.printf ("Var %s ppt %s : null-not missing%n",
				 vi, ppt.name() );
            vi.canBeMissing = true;
          }
        } catch (Exception e) {
          throw new Daikon.TerminationMessage(
            "Error while parsing value "
              + value_rep
              + " for variable "
              + vi.name()
              + " of type "
              + vi.rep_type
              + ": "
              + e.toString() );
        }
      }
      allVar_index++;
      val_index++;
    }
    ppt_to_value_reps.put(ppt, oldvalue_reps);
  }

  // Used in read_vals_from_binary_dtrace_file( )
  // Currently this method recreates the string value, read from a dtrace file
  // So that VarInfo : rep_type.parse_value( var_value )
  // receives as a parameter the String format it expects
  // It recreates the null string as "null"
  private static String get_variableValue( VarInfo vi, DataInputStream binReader,
						   PrintWriter pWriter )
  throws IOException {
    String elements = null;
    ProglangType repType = vi.file_rep_type;

    if ( repType == ProglangType.STRING ) {
      elements = "";
      int strLen = binReader.readInt( );

      if ( strLen != -1 ) {
        for ( int j = 0; j < strLen; j++ ) {
          elements += ( char ) binReader.readByte( );
        }
        elements = "\"" + elements + "\"";
      }
      else {
        elements = "null";
      }

      echoMsg( pWriter, (vi.name( ) + "\n" + elements) );
   }

   else if ( repType == ProglangType.STRING_ARRAY ) {
     echoMsg( pWriter, vi.name( ) );
     int al = binReader.readInt( );
     elements = "[";

     for ( int j = 0; j < al; j++ ) {
       int sl = binReader.readByte( );
       String rs = "\"";
       if ( sl != -1 ) {
         for ( int k = 0; k < sl; k++ ) {
           char c = ( char ) binReader.readByte( );
           rs += c;
         }
         elements += rs + "\" ";
       }
       else {
           elements  += "null ";
       }
     }
     elements =  elements.trim( ) + "]";
     echoMsg( pWriter, elements );
   }

   else if ( repType == ProglangType.INT_ARRAY
             || repType == ProglangType.LONG_PRIMITIVE_ARRAY
             || repType == ProglangType.HASHCODE_ARRAY
             || repType == ProglangType.CHAR_ARRAY
             || repType == ProglangType.BOOLEAN_ARRAY ) {

     echoMsg( pWriter, vi.name( ) );
     elements = "[";
     int al = binReader.readInt( );

     if ( repType == ProglangType.INT_ARRAY ) {
       for ( int j = 0; j < al; j++ ) {
         int aval = binReader.readInt( );
         elements += aval + " ";
       }
     }
     else if ( repType == ProglangType.CHAR_ARRAY ) {
       for ( int j = 0; j < al; j++ ) {
         String sval = Byte.toString( binReader.readByte( ) );
         elements += sval + " ";
       }
     }
     else if ( repType == ProglangType.HASHCODE_ARRAY ) {
       for ( int j = 0; j < al; j++ ) {
         int hval = binReader.readInt( );
         if ( hval != 0 ) {
           elements += hval + " ";
         }
         else {
           elements += "null" + " ";
         }
       }
     }
     else if ( repType == ProglangType.BOOLEAN_ARRAY ) {
       for ( int j = 0; j < al; j++ ) {
         byte vb =  binReader.readByte( );
         if ( vb == 1 )
           elements += "true" + " ";
         else
           elements += "false" + " ";
       }
     }
     else {
       for ( int j = 0; j < al; j++ ) {
         long lval = binReader.readLong( );
         elements += lval + " ";
       }
     }
     elements = elements.trim( );
     elements += "]";
     echoMsg( pWriter, elements );
   }
   else if ( repType == ProglangType.DOUBLE_ARRAY ) {
     echoMsg( pWriter, vi.name( ) );
     int al = binReader.readInt( );
     elements = "[";

     for ( int j = 0; j < al; j++ ) {
       double dval = binReader.readDouble( );
       elements += dval + " ";
     }
     elements = elements.trim( );
     elements += "]";
     echoMsg( pWriter, elements );
   }
    /*else if ( repType == ProglangType.CHAR_ARRAY_ARRAY ) {
     echoMsg( pWriter, vi.name( ) );
     int al = binReader.readInt( );
     elements = "[";

     for ( int k = 0; k < al; k++ ) {
       String row = "[";
       int rl = binReader.readByte( );

       for ( int l = 0; l < rl; l++ ) {
         char vc = ( char ) binReader.readByte( );
         row += vc + " ";
       }
       elements += row.trim( ) + "]";
     }
     elements += "]";
     echoMsg( pWriter, elements );
     }*/
   else {
     echoMsg( pWriter, vi.name( ) );

     if ( repType == ProglangType.BOOLEAN ) {
       elements = "";
       char vb = ( char ) binReader.readByte( );
       elements = ( vb == '1' ) ? "true" : "false" ;
     }

     else if ( repType == ProglangType.CHAR ) {
       elements = "";
       char vc = ( char ) binReader.readByte( );
       elements += vc;
     }

     else if ( repType == ProglangType.DOUBLE ) {
       elements = "";
       double vd = binReader.readDouble( );
       elements += vd;
     }

     else if ( repType == ProglangType.LONG_PRIMITIVE || repType == ProglangType.LONG_OBJECT
               || repType == ProglangType.OBJECT ) {
       elements = "";
       long vl = binReader.readLong( );
       elements += vl;
     }

     else if ( repType == ProglangType.INT || repType == ProglangType.INTEGER
               || repType == ProglangType.HASHCODE ) {
       elements = "";
       int vint = binReader.readInt( );
       if ( repType == ProglangType.HASHCODE && vint == 0 )
         elements = "null";
       else
         elements += vint;
     }

     else {
       return null;
     }
     echoMsg( pWriter, elements );
   }
    return elements;
  }

  /**
   * If this is an function entry ppt, stores the values of all of the
   * variables away for use at the exit.  If this is an exit, finds the
   * values at enter and adds them as the value sof the orig variables.
   * Normally returns false.  Returns true if this is an exit without
   * a matching enter.  See dkconfig_ignore_missing_enter for more info.
   * If true is returned, this ppt should be ignored by the caller
   **/
  public static boolean add_orig_variables(PptTopLevel ppt,
                                     // HashMap cumulative_modbits,
                                     /*@Nullable*/ Object[] vals, int[] mods, Integer nonce) {
    VarInfo[] vis = ppt.var_infos;
    /*@Interned*/ String fn_name = ppt.ppt_name.getNameWithoutPoint();
    String ppt_name = ppt.name();
    if (ppt_name.endsWith(enter_tag)) {
      Invocation invok = new Invocation(ppt, vals, mods);
      if (nonce == null) {
        call_stack.push(invok);
      } else {
        call_hashmap.put(nonce, invok);
      }
      return false;
    }

    if (ppt.ppt_name.isExitPoint() || ppt.ppt_name.isThrowsPoint()) {
      Invocation invoc;
      // Set invoc
      {
        if (nonce == null) {
          if (call_stack.empty()) {
            // Not Daikon.TerminationMessage:  caller knows context such as
            // file name and line number.
            throw new Error(
              "Function exit without corresponding entry: " + ppt.name());
          }
          invoc = call_stack.pop();
          while (invoc.ppt.ppt_name.getNameWithoutPoint() != fn_name) {
            // Should also mark as a function that made an exceptional exit
            // at runtime.
            System.err.println(
              "Exceptional exit from function "
                + fn_name
                + ", expected to first exit from "
                + invoc.ppt.ppt_name.getNameWithoutPoint()
                + ((data_trace_state.filename == null)
                  ? ""
                  : "; at "
                    + data_trace_state.filename
                    + " line "
                    + data_trace_state.reader.getLineNumber()));
            invoc = call_stack.pop();
          }
        } else {
          // nonce != null
          invoc = call_hashmap.get(nonce);
          if (dkconfig_ignore_missing_enter && (invoc == null)) {
            //System.out.printf ("Didn't find call with nonce %d to match %s" +
            //                   " ending at %s line %d\n", nonce, ppt.name(),
            //                   data_trace_state.filename,
            //                   data_trace_state.reader.getLineNumber());
            return true;
          } else if (invoc == null) {
            assert data_trace_state != null; // nullness application invariant?
            // Not Daikon.TerminationMessage:  caller knows context such as
            // file name and line number.
            throw new Error(
              "Didn't find call with nonce "
                + nonce
                + " to match "
                + ppt.name()
                + " ending at "
                + data_trace_state.filename
                + " line "
                + data_trace_state.reader.getLineNumber());
          }
          invoc = call_hashmap.get(nonce);
          call_hashmap.remove(nonce);
        }
      }
      assert invoc != null;

      // Loop through each orig variable and get its value/mod bits from
      // the ENTER point.  vi_index is the index into var_infos at the
      // ENTER point.  val_index is the index into vals[] and mods[] at
      // ENTER point.  Note that vis[] includes static constants but
      // vals[] and mods[] do not.  Also that we don't create orig versions
      // of static constants
      int vi_index = 0;
      for (int val_index = 0; val_index < ppt.num_orig_vars; val_index++) {
        VarInfo vi = vis[ppt.num_tracevars + ppt.num_static_constant_vars
                         + val_index];
        assert (!vi.is_static_constant) : "orig constant " + vi;

        // Skip over constants in the entry point
        while (invoc.ppt.var_infos[vi_index].is_static_constant)
          vi_index++;

        // Copy the vals and mod bits from entry to exit
        vals[ppt.num_tracevars + val_index] = invoc.vals[val_index];
        int mod = invoc.mods[val_index];
        mods[ppt.num_tracevars + val_index] = mod;

        // If the value was missing, mark this variable as can be missing
        // Carefully check that we have orig version of the variable from
        // the ENTER point.
        if (ValueTuple.modIsMissingNonsensical (mod)) {
          if (debug_missing && !vi.canBeMissing) {
            System.out.printf ("add_orig: var %s missing[%d/%d]%n", vi,
                               val_index, vi_index);
          }
          vi.canBeMissing = true;
          assert invoc.vals[val_index] == null;
          assert vi.name() == invoc.ppt.var_infos[vi_index].prestate_name()
            : vi.name() + " != "+ invoc.ppt.var_infos[vi_index];
          assert invoc.ppt.var_infos[vi_index].canBeMissing
            : invoc.ppt.var_infos[vi_index];
        }
        vi_index++;
      }
    }
    return false;
  }

  /** Add derived variables **/
  public static void add_derived_variables(PptTopLevel ppt,
                                           /*@Nullable*/ Object[] vals,
                                            int[] mods) {
    // This ValueTuple is temporary:  we're temporarily suppressing interning,
    // which we will do after we have all the values available.
    ValueTuple partial_vt = ValueTuple.makeUninterned(vals, mods);
    int filled_slots =
      ppt.num_orig_vars + ppt.num_tracevars + ppt.num_static_constant_vars;
    for (int i = 0; i < filled_slots; i++) {
      assert !ppt.var_infos[i].isDerived();
    }
    int num_const = ppt.num_static_constant_vars;
    for (int i = filled_slots; i < ppt.var_infos.length; i++) {
      assert ppt.var_infos[i].derived != null :
        // repr() can be slow
        "variable not derived: " + ppt.var_infos[i].repr();
      // Add this derived variable's value
      ValueAndModified vm =
        ppt.var_infos[i].derived.computeValueAndModified(partial_vt);
      vals[i - num_const] = vm.value;
      mods[i - num_const] = vm.modified;
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
  static final class SerialFormat implements Serializable {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20060905L;

    public SerialFormat(PptMap map, Configuration config) {
      this.map = map;
      this.config = config;
      assert FileIO.new_decl_format != null; // nullness application invariant
      this.new_decl_format = FileIO.new_decl_format;

    }
    public PptMap map;
    public Configuration config;
    public boolean new_decl_format = false;
  }

  public static void write_serialized_pptmap(PptMap map, File file)
    throws IOException {
    SerialFormat record = new SerialFormat(map, Configuration.getInstance());
    UtilMDE.writeObject(record, file);
  }

  /**
   * Read either a serialized PptMap or a InvMap and return a
   * PptMap.  If an InvMap is specified, it is converted to a PptMap
   */
  public static PptMap read_serialized_pptmap(
    File file,
    boolean use_saved_config)
    throws IOException {

    try {
      Object obj = UtilMDE.readObject(file);
      if (obj instanceof FileIO.SerialFormat) {
        SerialFormat record = (SerialFormat) obj;
        if (use_saved_config) {
          Configuration.getInstance().overlap(record.config);
        }
        FileIO.new_decl_format = record.new_decl_format;
        // System.err.printf ("Setting FileIO.new_decl_format to %b%n",
        //                   FileIO.new_decl_format);
        return (record.map);
      } else if (obj instanceof InvMap) {
        // System.err.printf ("Restoring an InvMap%n");
        InvMap invs = (InvMap) obj;
        PptMap ppts = new PptMap();
        for (Iterator<PptTopLevel> i = invs.pptIterator(); i.hasNext();) {
          PptTopLevel ppt = i.next();
          PptTopLevel nppt = new PptTopLevel(ppt.name, ppt.var_infos);
          nppt.set_sample_number(ppt.num_samples());
          ppts.add(nppt);
          List<Invariant> inv_list = invs.get(ppt);
          for (Invariant inv : inv_list) {
            PptSlice slice = nppt.get_or_instantiate_slice(inv.ppt.var_infos);
            inv.ppt = slice;
            slice.addInvariant(inv);
          }
        }
        return (ppts);
      } else {
        throw new IOException(
          "Unexpected serialized file type: " + obj.getClass());
      }
    } catch (ClassNotFoundException e) {
      throw (IOException)(new IOException("Error while loading inv file").initCause(e));
    } catch (InvalidClassException e) {
      throw new IOException(
        "It is likely that the .inv file format has changed, because a Daikon data structure has been modified, so your old .inv file is no longer readable by Daikon.  Please regenerate your .inv file."
        // + lineSep + e.toString()
        );
    }
    // } catch (StreamCorruptedException e) { // already extends IOException
    // } catch (OptionalDataException e) {    // already extends IOException
  }

  /**
   * Returns whether or not the specified ppt name should be included
   * in processing.  Ppts can be excluded because they match the omit_regexp,
   * don't match ppt_regexp, or are greater than ppt_max_name.
   */
  public static boolean ppt_included(String ppt_name) {

    // System.out.println ("ppt_name = '" + ppt_name + "' max name = '"
    //                     + Daikon.ppt_max_name + "'");
    if (((Daikon.ppt_omit_regexp != null)
         && Daikon.ppt_omit_regexp.matcher(ppt_name).find())
        || ((Daikon.ppt_regexp != null)
            && !Daikon.ppt_regexp.matcher(ppt_name).find())
        || ((Daikon.ppt_max_name != null)
            && ((Daikon.ppt_max_name.compareTo(ppt_name) < 0)
                && (ppt_name.indexOf(global_suffix) == -1)))) {
      return (false);
    } else {
      return (true);
    }
  }

  /**
   * Returns true if the given variable is included, according to Daikon's
   * --var-select-pattern and --var-omit-pattern flags.
   **/
  public static boolean var_included(String var_name) {
    assert ! var_name.equals("");
    if (((Daikon.var_omit_regexp != null)
         && Daikon.var_omit_regexp.matcher(var_name).find())
        || ((Daikon.var_regexp != null)
            && !Daikon.var_regexp.matcher(var_name).find())) {
      return (false);
    } else {
      return true;
    }
  }

  /**
   * Checks the specified array of variables to see if it matches
   * exactly thevariables in the existing ppt.  Throws an error if
   * there are any differences.  Used to ensure that a new ppt with the
   * same name as an existing ppt is exactly the same
   */
  static void check_decl_match (ParseState state, PptTopLevel existing_ppt,
                                VarInfo[] vi_array) {

    VarInfo[] existing_vars = existing_ppt.var_infos;
    if (existing_ppt.num_declvars!=vi_array.length) {
      throw new Daikon.TerminationMessage
        ("Duplicate declaration of program point \"" + existing_ppt.name()
         + "\" with a different number of VarInfo objects: "
         + "old VarInfo number=" + existing_ppt.num_declvars
         + ", new VarInfo number="+vi_array.length,
         state);
        }

    for (int i=0; i<vi_array.length; i++) {
      String oldName = existing_vars[i].str_name();
      String newName = vi_array[i].str_name();
      if (!oldName.equals(newName)) {
        throw new Daikon.TerminationMessage
          ("Duplicate declaration of program point \""
           + existing_ppt.name() + "\" with two different VarInfo: old VarInfo="
           + oldName+", new VarInfo="+newName, state);
      }
    }
  }

  /**
   * Skips over a decl.  Essentially reads in everything up to and including
   * the next blank line.
   */
  private static void skip_decl (LineNumberReader reader) throws IOException {
    String line = reader.readLine();
    // This fails if some lines of a declaration (e.g., the comparability
    // field) are empty.
    while ((line != null) && !line.equals("")) {
      line = reader.readLine();
    }
  }

  /**
   * Converts the declaration record versoin of a name into its correct
   * version.  In the declaration record, blanks are encoded as \_ and
   * backslashes as \\.
   */
  private static String unescape_decl (String orig) {
    StringBuilder sb = new StringBuilder(orig.length());
    // The previous escape character was seen just before this position.
    int post_esc = 0;
    int this_esc = orig.indexOf('\\');
    while (this_esc != -1) {
      if (this_esc == orig.length()-1) {
        sb.append(orig.substring(post_esc, this_esc+1));
        post_esc = this_esc+1;
        break;
      }
      switch (orig.charAt(this_esc+1)) {
      case 'n':
        sb.append(orig.substring(post_esc, this_esc));
        sb.append('\n');        // not lineSep
        post_esc = this_esc+2;
        break;
      case 'r':
        sb.append(orig.substring(post_esc, this_esc));
        sb.append('\r');
        post_esc = this_esc+2;
        break;
      case '_':
        sb.append (orig.substring(post_esc, this_esc));
        sb.append (' ');
        post_esc = this_esc+2;
        break;
      case '\\':
        // This is not in the default case because the search would find
        // the quoted backslash.  Here we incluce the first backslash in
        // the output, but not the first.
        sb.append(orig.substring(post_esc, this_esc+1));
        post_esc = this_esc+2;
        break;

      default:
        // In the default case, retain the character following the
        // backslash, but discard the backslash itself.  "\*" is just
        // a one-character string.
        sb.append(orig.substring(post_esc, this_esc));
        post_esc = this_esc+1;
        break;
      }
      this_esc = orig.indexOf('\\', post_esc);
    }
    if (post_esc == 0)
      return orig;
    sb.append(orig.substring(post_esc));
    return sb.toString();
  }

  /**
   * Class that holds all of the information from the declaration record
   * concerning a particular variable
   */
  @SuppressWarnings("nullness") // needs documentation before annotating with nullness
  public static class VarDefinition implements java.io.Serializable, Cloneable{
    static final long serialVersionUID = 20060524L;
    transient ParseState state;
    public String name;
    public VarKind kind = null;
    public String enclosing_var;
    public String relative_name = null;
    public RefType ref_type = RefType.POINTER;
    public int arr_dims = 0;
    public /*@Nullable*/ List<String> function_args = null; // non-null iff (vardef.kind == VarKind.FUNCTION)
    public ProglangType rep_type = null;
    public ProglangType declared_type = null;
    public EnumSet<VarFlags> flags = EnumSet.noneOf (VarFlags.class);
    public EnumSet<LangFlags> lang_flags = EnumSet.noneOf (LangFlags.class);
    public VarComparability comparability = null;
    public /*@Interned*/ String parent_ppt = null;
    public int parent_relation_id = 0;
    public String parent_variable = null;
    public /*@Interned*/ Object static_constant_value = null;

    public VarDefinition clone() {
      try {
        return (VarDefinition) super.clone();
      } catch (CloneNotSupportedException e) {
        throw new Error("This can't happen: ", e);
      }
    }

    public VarDefinition copy () {
      try {
        VarDefinition copy = this.clone();
        copy.flags = flags.clone();
        copy.lang_flags = lang_flags.clone();
        return copy;
      } catch (Throwable t) {
        throw new RuntimeException (t);
      }
    }

    /** Clears the parent relation if one existed **/
    public void clear_parent_relation() {
      parent_ppt = null;
      parent_relation_id = 0;
      parent_variable = null;
    }

    /**
     * Initialize from the 'variable <name>' record.  Scanner should be
     * pointing at name.
     */
    public VarDefinition (ParseState state, Scanner scanner) throws DeclError {
      this.state = state;
      name = need (scanner, "name");
      need_eol (scanner);
      if (state.varcomp_format == VarComparability.IMPLICIT)
        comparability = VarComparabilityImplicit.unknown;
      else
        comparability = VarComparabilityNone.it;
    }

    public VarDefinition (String name, VarKind kind, ProglangType type) {
      this.state = null;
      this.name = name;
      this.kind = kind;
      this.rep_type = type;
      this.declared_type = type;
      comparability = VarComparabilityNone.it;
    }

    /**
     * Parse a var-kind record.  Scanner should be pointing at the variable
     * kind.
     */
    public void parse_var_kind (Scanner scanner) throws DeclError {
      VarKind kind_local = parse_enum_val (scanner, VarKind.class, "variable kind");
      kind = kind_local;

      if ((kind == VarKind.FIELD) || (kind == VarKind.FUNCTION)) {
        relative_name = need (scanner, "relative name");
      }
      need_eol (scanner);
    }

    /** Parses the enclosing-var record **/
    public void parse_enclosing_var (Scanner scanner) throws DeclError {
      enclosing_var = need (scanner, "enclosing variable name");
      need_eol(scanner);
    }

    /** Parses the reference-type record **/
    public void parse_reference_type (Scanner scanner) throws DeclError {
      RefType ref_type_local = parse_enum_val (scanner, RefType.class, "reference type");
      ref_type = ref_type_local;
      need_eol (scanner);
    }

    /** Parses the array record **/
    public void parse_array (Scanner scanner) throws DeclError {
      /*@Interned*/ String arr_str = need (scanner, "array dimensions");
      if (arr_str == "0")       // interned
        arr_dims = 0;
      else if (arr_str == "1")  // interned
        arr_dims = 1;
      else
        decl_error (state, "%s found where 0 or 1 expected", arr_str);
    }

    /** Parses the function-args record **/
    public void parse_function_args (Scanner scanner) throws DeclError {

      function_args = new ArrayList<String>();
      while (scanner.hasNext()) {
        function_args.add (unescape_decl (scanner.next()).intern());
      }
    }

    public void parse_rep_type (Scanner scanner) throws DeclError {
      /*@Interned*/ String rep_type_str = need (scanner, "rep type");
      need_eol (scanner);
      rep_type = ProglangType.rep_parse (rep_type_str);
    }

    public void parse_dec_type (Scanner scanner) throws DeclError {
      /*@Interned*/ String declared_type_str = need (scanner, "declaration type");
      need_eol (scanner);
      declared_type = ProglangType.parse (declared_type_str);
    }

    /** Parse the flags record.  Multiple flags can be specified **/
    public void parse_flags (Scanner scanner) throws DeclError {

      flags.add (parse_enum_val (scanner, VarFlags.class, "Flag"));
      while (scanner.hasNext())
        flags.add (parse_enum_val (scanner, VarFlags.class, "Flag"));
      // System.out.printf ("flags for %s are %s%n", name, flags);
    }

    /**
     * Parse the langauge specific flags record.  Multiple flags can
     * be specified **/
    public void parse_lang_flags (Scanner scanner) throws DeclError {

      lang_flags.add (parse_enum_val (scanner, LangFlags.class,
                                      "Language Specific Flag"));
      while (scanner.hasNext())
        lang_flags.add (parse_enum_val (scanner, LangFlags.class,
                                        "Language Specific Flag"));
    }

    /** Parses a comparability record **/
    public void parse_comparability (Scanner scanner) throws DeclError {
      /*@Interned*/ String comparability_str = need (scanner, "comparability");
      need_eol (scanner);
      comparability = VarComparability.parse (state.varcomp_format,
                                            comparability_str, declared_type);
    }

    /** Parse a parent ppt record **/
    public void parse_parent (Scanner scanner,
                       List<ParentRelation> ppt_parents) throws DeclError {

     parent_ppt = need (scanner, "parent ppt");
     parent_relation_id = Integer.parseInt (need (scanner, "parent id"));
     boolean found = false;
     for (ParentRelation pr : ppt_parents) {
       if ((pr.parent_ppt_name == parent_ppt) && (pr.id == parent_relation_id)) {
         found = true;
         break;
       }
     }
     if (!found) {
       decl_error (state, "specified parent ppt '%s[%d]' for variable '%s' "
                   + "is not a parent to this ppt", parent_ppt,
                   parent_relation_id, name);
     }
     if (scanner.hasNext())
       parent_variable = need (scanner, "parent variable");
     need_eol (scanner);
    }

    /** Parse a constant record **/
    public void parse_constant (Scanner scanner) throws DeclError {
      /*@Interned*/ String constant_str = need (scanner, "constant value");
      need_eol (scanner);
      try {
        static_constant_value = rep_type.parse_value (constant_str);
      } catch (Error e) {
        decl_error (state, e.getMessage());
      }
    }

    /**
     * Helper function, returns the next string token unescaped and
     * interned.  Throw a DeclError if there is no next token
     */
    public /*@Interned*/ String need (Scanner scanner, String description) throws DeclError {
      return (FileIO.need (state, scanner, description));
    }

    /** Throws a DeclError if the scanner is not at end of line */
    public void need_eol (Scanner scanner) throws DeclError {
      FileIO.need_eol (state, scanner);
    }

    /**
     * Looks up the next token as a member of enum_class.  A DeclError
     * is thrown if there is no token or if it is not valid member of
     * the class.  Enums are presumed to be in in upper case
     */
    public <E extends Enum<E>> E parse_enum_val (Scanner scanner,
           Class<E> enum_class, String descr) throws DeclError {
      return FileIO.parse_enum_val (state, scanner, enum_class, descr);
    }
  }

  /**
   * Helper function, returns the next string token unescaped and
   * interned.  Throw a DeclError if there is no next token
   */
  public static /*@Interned*/ String need (ParseState state, Scanner scanner,
                             String description) throws DeclError {
    if (!scanner.hasNext())
      decl_error (state, "end-of-line found where %s expected", description);
    return unescape_decl (scanner.next()).intern();
  }

  /** Throws a DeclError if the scanner is not at end of line */
  public static void need_eol (ParseState state, Scanner scanner)
    throws DeclError {
    if (scanner.hasNext())
      decl_error (state, "'%s' found where end-of-line expected",
                  scanner.next());
  }

  /**
   * Looks up the next token as a member of enum_class.  A DeclError
   * is thrown if there is no token or if it is not valid member of
   * the class.  Enums are presumed to be in in upper case
   */
  public static <E extends Enum<E>> E parse_enum_val (ParseState state,
         Scanner scanner, Class<E> enum_class, String descr) throws DeclError {

    /*@Interned*/ String str = need (state, scanner, descr);
    try {
      E e = Enum.valueOf (enum_class, str.toUpperCase());
      return (e);
    } catch (Exception exception) {
      E[] all = enum_class.getEnumConstants();
      assert all != null;       // getEnumConstants returs non-null because enum_class is an enum class
      StringBuilderDelimited msg = new StringBuilderDelimited(", ");
      for (E e : all) {
        msg.append(String.format ("'%s'", e.name().toLowerCase()));
      }
      decl_error (state, "'%s' found where %s expected", str, msg);
      throw new Error("execution cannot get to here, previous line threw an error");
    }
  }

  private static void decl_error (ParseState state, String format,
                                  /*@Nullable*/ Object... args) throws DeclError {
    throw DeclError.detail (state, format, args);
  }

  /** Returns whether the line is the start of a ppt declaration **/
  private static boolean is_declaration_header (String line) {
    if (new_decl_format)
      return (line.startsWith ("ppt "));
    else
      return (line.equals(declaration_header));
  }


}
