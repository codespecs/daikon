package daikon.tools;
import java.io.*;
import java.util.*;
import utilMDE.*;
import daikon.*;
import daikon.config.Configuration;
import java.util.regex.*;
import java.util.zip.*;
import gnu.getopt.*;

import static daikon.PptTopLevel.PptFlags;
import static daikon.VarInfo.VarFlags;
import static daikon.VarInfo.LangFlags;


/**
 * This tool converts between the textual and binary Daikon file formats.
 * Its input is any number of files in either format.  By default it
 * converts each file to binary format.  If the --textoutput argument is
 * supplied, it converts each file to textual format.
 */
public class DtraceConvert {

  @Option("-d produce txt output to the terminal")
  public static boolean dbg = false;

  @Option("-b convert the given dtrace to a binary file")
  public static boolean binary = false;

  @Option("-t convert the given dtrace to a txt file")
  public static boolean text = false;

  @Option("-c produce a compressed output file")
  public static boolean compressed = false;

  private static boolean bin2txt = false;
  private static boolean txt2bin = false;

  public static void main (String[] args) {
    try {
      mainHelper(args);
    } catch (daikon.Daikon.TerminationMessage e) {
      System.err.println(e.getMessage());
      System.exit(1);
    }
    // Any exception other than daikon.Daikon.TerminationMessage gets
    // propagated.  This simplifies debugging by showing the stack trace.
  }

  /**
   * This does the work of main, but it never calls System.exit, so it
   * is appropriate to be called progrmmatically.
   * Termination of the program with a message to the user is indicated by
   * throwing daikon.Daikon.TerminationMessage.
   * @see #main(String[])
   * @see daikon.Daikon.TerminationMessage
   **/
  public static void mainHelper( final String[] args ) {
    DtraceConvert myInstance = new DtraceConvert( );
    Options options = new Options( "DtraceConvert [options] infiles...",
                                  DtraceConvert.class );
    String [] file_args = options.parse_or_usage (args);

    if ( file_args.length == 0 ) {
      throw new daikon.Daikon.TerminationMessage( "No trace files specified." );
    }
    for ( String filename : file_args ) {
      if ( binary )
        dtraceConvert2Binary( filename );
      if ( text )
        dtraceConvert2ASCII( filename );
    }
  }

  public static void dtraceConvert2Binary( String filename ) {
    txt2bin = ( filename.endsWith("dat.gz") || filename.endsWith("dat") ) ?
              false : true;
    dtraceConvert2Binary( filename, dbg );
  }

  public static void dtraceConvert2ASCII( String filename ) {
    bin2txt = ( filename.endsWith("dat.gz") || filename.endsWith("dat") ) ?
              true : false;
    dtraceConvert2ASCII( filename, dbg );
  }

  // BIN2TXT Conversion
  // TXT2TXT Conversion
  // Writes the dtracefile data in a txt file
  // dtracefile can be textual or binary
  public static void dtraceConvert2ASCII( String dtracefile, boolean dbg ) {
    PptMap ppts = new PptMap();

    PrintWriter pWriter = null;
    String fileDTrace;

    if ( dtracefile.endsWith("gz") )
        fileDTrace = dtracefile.substring( 0, dtracefile.length()-3 );
    else
        fileDTrace = dtracefile;
    fileDTrace += ( dtracefile.endsWith("dtrace") ) ? ".copy.dtrace" : ".dtrace";

    try {
      pWriter = new PrintWriter( new FileOutputStream( fileDTrace ), true );

      FileIO.ParseState state =
        new FileIO.ParseState( dtracefile, false, true, ppts );

      while( state.status != FileIO.ParseStatus.EOF ) {
        if ( bin2txt ) {
            FileIO.read_binary_data_trace_record( state, null );
        }
        else { // txt2txt
          FileIO.read_data_trace_record_setstate( state );
        }
        switch( state.status ){

        case SAMPLE:
          write_sample( pWriter, null, state.all_ppts, state.ppt, state.vt, state.nonce );
          stdoutMsg( "Got a sample", dbg );
          break;
        case DECL:
          FileIO.write_binaryDeclaration( pWriter, null, state );
          stdoutMsg( "Got a decl", dbg );
          break;
        case DECL_VERSION: {
          String [] identifiers = new String [2];
          identifiers[0] = "decl-version";
          identifiers[1] = "";

          if ( FileIO.new_decl_format )
              identifiers[1] += "2.0";
          else
              identifiers[1] += "1.0";

          stdoutMsg( "Got a version", dbg );
          pWriter.println( "\ndecl-version " + identifiers[1] );
          break;
        }
        case COMPARABILITY: {
          String [ ] identifiers = new String [2];
          identifiers[0] =  "var-comparability";
          identifiers[1] = "implicit";

          if ( state.varcomp_format == VarComparability.NONE ) {
            identifiers[1] = "none";
          }

          stdoutMsg( "Got comparability", dbg );
          pWriter.println( identifiers[0] + " " +  identifiers[1] );
          break;
        }
        case LIST_IMPLEMENTORS: {
          int size = ProglangType.list_implementors.size( );
          size++;
          int index = 0;

          String [] identifiers = new String [size];
          identifiers[index] = "ListImplementors";

          // interspersed comments aren't supported
          String implementors = "ListImplementors\n";
          for ( String impl : ProglangType.list_implementors ) {
              implementors += impl + "\n" ;
              identifiers[++index] = impl;
          }

          stdoutMsg( "Got list", dbg );
          pWriter.println( implementors );
          break;
        }
        case INPUT_LANGUAGE: {
          String [] identifiers = new String [2];
          identifiers[0] = "input_language";
          identifiers[1] = FileIO.input_lang;

          stdoutMsg( "Got input language", dbg );
          pWriter.println( identifiers[0] + identifiers[1] );
          break;
        }
        case NULL:
          System.err.println( "Got a null" );
          break;
        case COMMENT: {
          String [] identifiers = new String [2];
          identifiers[0] = "" + (state.payload.trim()).charAt( 0 );
          if ( identifiers[0].equals( "/" ) )
              identifiers[0] += state.payload.charAt( 1 );
          else
              pWriter.println( "\n" );

          identifiers[1] = state.payload;
          stdoutMsg( "Got a comment line", dbg );
          pWriter.println( state.payload );
          break;
        }
        case EOF:
          stdoutMsg( "Got a eof", dbg );
          break;
        case TRUNCATED:
          System.err.println( "Got a truncated" );
          break;
        case ERROR:
          System.err.println( "Got a error" );
          break;

        default:
          throw new Error( );
        }
      }
    } catch (IOException e) {
      e.printStackTrace( );
      throw new Error( e );
    }
  }

    public static void stdoutMsg( String msg, boolean dbg ) {
        if ( dbg )
            System.out.println( msg );
    }

    // TXT2BIN Conversion
    // BIN2BIN Conversion
    // Writes the dtracefile data in a binary file
    // dtracefile can be textual or binary
    public static void dtraceConvert2Binary( String dtracefile, boolean dbg  ) {
	PptMap ppts = new PptMap();

	DataOutputStream binWriter = null;
	PrintWriter pWriter = null;

        String binfile;
        if ( dtracefile.endsWith("gz") )
            binfile = dtracefile.substring( 0, dtracefile.length()-3 );
        else
            binfile = dtracefile;

        binfile += ( dtracefile.endsWith("dat") ) ? ".copy.dat" : ".dat";
        String testDTrace = binfile + ".txt"; //.dat.txt

        try {
	    binWriter = new DataOutputStream( new FileOutputStream( binfile ) );
	}
	catch ( IOException e ) {
	    e.printStackTrace( );
	}

	try {
	    FileIO.ParseState state =
		new FileIO.ParseState ( dtracefile, false, true, ppts );

	    while( state.status != FileIO.ParseStatus.EOF ) {
                if ( txt2bin ) {
                    FileIO.read_data_trace_record( state );
                }
                else { //bin2bin
                    FileIO.read_binary_data_trace_record( state, null );
                }
		switch( state.status ){
		case NULL:
		    System.err.println( "Got null" );
		    break;
		case TRUNCATED:
		    System.err.println( "Got truncated" );
		    break;
		case ERROR:
		    System.err.println( "Got an error" );
		    break;
		case EOF:
		    stdoutMsg( "Got EOF", dbg );
		    break;
		case DECL: {
		    stdoutMsg( "Got a decl", dbg );
                    FileIO.write_binaryDeclaration( pWriter, binWriter, state );
		    break;
		}
		case SAMPLE: {
		    write_sample( pWriter, binWriter, state.all_ppts, state.ppt, state.vt, state.nonce );
		    stdoutMsg( "Got a sample", dbg );
		    break;
		}
		case COMPARABILITY: {
		    String [ ] identifiers = new String [2];
		    identifiers[0] =  "var-comparability";
		    identifiers[1] = "implicit";

		    if ( state.varcomp_format == VarComparability.NONE ) {
			identifiers[1] = "none";
		    }

		    stdoutMsg( "Got comparability", dbg );
		    FileIO.echoMsg( pWriter, identifiers[0] + " " +  identifiers[1] );

		    for ( int i = 0; i < identifiers.length; i++ ) {
                        echoBinMsg( binWriter, identifiers[i],
                                    identifiers[i].length( ), 1 );
		    }
		    break;
		}
		case LIST_IMPLEMENTORS: {
		    int size = ProglangType.list_implementors.size( );
		    size++;
		    int index = 0;

		    String [] identifiers = new String [size];
		    identifiers[index] = "ListImplementors";

                    // interspersed comments aren't supported
		    String implementors = "ListImplementors\n";
		    for ( String impl : ProglangType.list_implementors ) {
			implementors += impl + "\n" ;
			identifiers[++index] = impl;
		    }

		    stdoutMsg( "Got list", dbg );
		    FileIO.echoMsg( pWriter, implementors );

		    for ( int i = 0; i < size; i++ ) {
                      echoBinMsg( binWriter, identifiers[i], identifiers[i].length( ), 1 );
		    }
		    break;
		}
		case INPUT_LANGUAGE: {
		    String [] identifiers = new String [2];
		    identifiers[0] = "input_language";
		    identifiers[1] = FileIO.input_lang;

		    stdoutMsg( "Got input language", dbg );
		    FileIO.echoMsg( pWriter, identifiers[0] + identifiers[1] );

		    for ( int i = 0; i < identifiers.length; i++ ) {
                        echoBinMsg( binWriter, identifiers[i],
                                    identifiers[i].length( ), 1 );
		    }
		    break;
		}
		case DECL_VERSION: {
		    String [] identifiers = new String [2];
		    identifiers[0] = "decl-version";
		    identifiers[1] = "";

		    if ( FileIO.new_decl_format )
			identifiers[1] += "2.0";
		    else
			identifiers[1] += "1.0";

		    stdoutMsg( "Got a version", dbg );
		    FileIO.echoMsg( pWriter, "\ndecl-version " + identifiers[1] );

		    for ( int i = 0; i < identifiers.length; i++ ) {
                        echoBinMsg( binWriter, identifiers[i],
                                    identifiers[i].length( ), 1 );
		    }
		    break;
		}
		case COMMENT: {
		    String [] identifiers = new String [2];
		    identifiers[0] = "" + (state.payload.trim()).charAt( 0 );
		    if ( identifiers[0].equals( "/" ) )
			identifiers[0] += state.payload.charAt( 1 );
                    else
                        FileIO.echoMsg( pWriter, "\n" );

                    binWriter.writeByte( identifiers[0].length( ) );
                    binWriter.writeBytes( identifiers[0] );

		    identifiers[1] = state.payload;

                    // comments can be long (a byte for their length appears insufficient)
                    echoBinMsg( binWriter, identifiers[1],
                                identifiers[1].length( ), 4 );

		    stdoutMsg( "Got a comment line", dbg );
		    FileIO.echoMsg( pWriter, state.payload );
		    break;
		}
		default:
		    throw new Error( );
		}
	    }
	} catch( IOException e ) {
	    e.printStackTrace( );
	    throw new Error( e );
	}
    }

    // Uses the symbolic info provided by the parameters:
    // all_ppts, ppt, vt, nonce to write:
    // binary sample, if binWriter != null
    // txt sample, if pWriter != null
    public static void write_sample( PrintWriter pWriter, DataOutputStream binWriter,
					   PptMap all_ppts,
					   PptTopLevel ppt,
					   ValueTuple vt,
					   Integer nonce ) {

      Collection<PptTopLevel> allPptTopLevels = all_ppts.asCollection( );
      FileIO.echoMsg( pWriter, "" ); // new line

      String ppt_index = "";
      if ( FileIO.bin2ascii_dbg )
        ppt_index = " " +  FileIO.pptNamesList.indexOf( ppt.ppt_name.getName( ) );

      FileIO.echoMsg( pWriter, (ppt.ppt_name.getName( ) + ppt_index) );

      // uses the key ( ppt_name ) to retrieve the integer index
      String hash_pptName = "" + FileIO.pptNamesList.indexOf( ppt.ppt_name.getName( ) );

      try {
	// records the program point index
        echoBinMsg( binWriter, hash_pptName, hash_pptName.length( ), 1 );

        if ( nonce != null ) {
          FileIO.echoMsg( pWriter, ("this_invocation_nonce\n" + nonce.intValue( )) );
          echoBinMsg( binWriter, null, nonce.intValue( ), 4 );
        }
        else {
          // the binReader will always expect a 4 byte value for the nonce
          echoBinMsg( binWriter, null, 0, 4 );
        }

        int mods [] = vt.mods; // get modes from the ValueTuple to check for "nonsensical"

        if ( FileIO.bin2ascii_dbg ) {
	  String vt_mods = "";
	  for ( int k = 0; k < mods.length; k++ )
	    vt_mods += mods[k];
      	  FileIO.echoMsg( pWriter, "vt_mods: " + vt_mods );
        }

        byte mod_bits [] = FileIO.get_modBits( mods );
        StringBuffer mod_str = FileIO.recordBits( mod_bits );
        if ( FileIO.bin2ascii_dbg )
          FileIO.echoMsg( pWriter, "recorded_mods: " + mod_str.toString( ) );

        // records the mod bits
        if ( binWriter != null ) {
          binWriter.writeByte( mod_bits.length );
          binWriter.write( mod_bits, 0, mod_bits.length );
        }
        int index_mods = 0; // index in mods[]

        // go through all variables in the given program point
        for ( int i = 0; i < ppt.num_declvars; i++, index_mods++ ) {

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
            assert value != null;

            if ( value instanceof String ) {
              String vs = ( String ) value;
              FileIO.echoMsg( pWriter, (vInfo.name( ) + "\n" + "\"" + vs + "\"") );
              FileIO.echoMsg( pWriter, valid_mod );

              if ( vs != null ) {
                echoBinMsg( binWriter, vs, vs.length( ), 4 );
              }
              else {
                echoBinMsg( binWriter, vs, -1, 4 );
              }
            }
            else if ( value instanceof long [] ) {
              long va [] = ( long [] ) value;
	      FileIO.echoMsg( pWriter, vInfo.name( ) );
              echoBinMsg( binWriter, null, va.length, 4 );
              //binWriter.writeInt( va.length );
              String msg = "[";

              if ( repType == ProglangType.INT_ARRAY ) {
                for ( int j = 0; j < va.length; j++ ) {
                  msg += va[j] + " ";
                  echoBinMsg( binWriter, null, ( int )va[j], 4 );
                }
              }
              else if ( repType == ProglangType.CHAR_ARRAY ) {
                for ( int j = 0; j < va.length; j++ ) {
                  msg += va[j] + " ";
                  echoBinMsg( binWriter, null, ( char )va[j], 1 );
                }
              }
              else if ( repType == ProglangType.HASHCODE_ARRAY ) {
                for ( int j = 0; j < va.length; j++ ) {
                  echoBinMsg( binWriter, null, ( int )va[j], 4 );
                  if ( va[j] != 0 )
                    msg += va[j] + " ";
                  else
                    msg += "null" + " ";
                }
              }
              else if ( repType == ProglangType.BOOLEAN_ARRAY ) {
                for ( int j = 0; j < va.length; j++ ) {
                    echoBinMsg( binWriter, null, ( byte )va[j], 1 );
                    if ( va[j] == 1 )
                        msg += "true" + " ";
                    else
                        msg += "false" + " ";
                }
              }
              else {
                for ( int j = 0; j < va.length; j++ ) {
                  msg += va[j] + " ";
                  echoBinMsg( binWriter, null, va[j], 8 );
                }
              }
              msg = msg.trim() + "]";
              FileIO.echoMsg( pWriter, msg );
              FileIO.echoMsg( pWriter, valid_mod );
            }
            else if ( value instanceof double [] ) {
              double va [] = ( double [] ) value;
              FileIO.echoMsg( pWriter, vInfo.name( ) );
              echoBinMsg( binWriter, null, ( int )va.length, 4 );
              String msg = "[";

              for ( int j = 0; j < va.length; j++ ) {
                msg += va[j] + " ";
                if ( binWriter != null )
                  binWriter.writeDouble( va[j] );
              }
              msg = msg.trim() + "]";
              FileIO.echoMsg( pWriter, msg );
              FileIO.echoMsg( pWriter, valid_mod );
            }
	    else if ( value instanceof String [] ) {
              String va [] = ( String [] ) value;
              FileIO.echoMsg( pWriter, vInfo.name( ) );
              echoBinMsg( binWriter, null, ( int )va.length, 4 );
              String msg = "[";

              for ( int j = 0; j < va.length; j++ ) {
                if ( va[j] != null ) {
                  msg += "\"" + va[j] + "\"" + " ";
                  String str = ( String )va[j];
                  echoBinMsg( binWriter, str, str.length( ), 4 );
                }
                else {
                  msg += "null" + " ";
                  // -1 length, means that the string is null;
                  echoBinMsg( binWriter, null, -1, 4 );
                }
              }
              msg = msg.trim() + "]";
              FileIO.echoMsg( pWriter, msg );
              FileIO.echoMsg( pWriter, valid_mod );
            }
            else {
              FileIO.echoMsg( pWriter, vInfo.name( ) );
              String sVal = value.toString( );

              if ( repType == ProglangType.BOOLEAN ) {
                echoBinMsg( binWriter, null, sVal.charAt( 0 ), 1 );
                sVal = ( sVal.charAt( 0 ) == '1' ) ? "true" : "false";
                FileIO.echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.CHAR ) {
                echoBinMsg( binWriter, null, sVal.charAt( 0 ), 1 );
                FileIO.echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.DOUBLE ) {
                if ( binWriter != null )
                  binWriter.writeDouble( Double.valueOf( sVal ) );
                FileIO.echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
               else if ( repType == ProglangType.LONG_PRIMITIVE ) {
                 echoBinMsg( binWriter, null, Long.valueOf( sVal ), 8 );
                 FileIO.echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.INT ) {
                echoBinMsg( binWriter, null, Integer.valueOf( sVal ), 4 );
                FileIO.echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.INTEGER ) {
                echoBinMsg( binWriter, null, Integer.valueOf( sVal ), 4 );
                FileIO.echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.LONG_OBJECT ) {
                echoBinMsg( binWriter, null, Long.valueOf( sVal ), 8 );
                FileIO.echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.OBJECT ) {
                echoBinMsg( binWriter, null, Long.valueOf( sVal ), 8 );
                FileIO.echoMsg( pWriter, sVal + "\n" + valid_mod );
              }
              else if ( repType == ProglangType.HASHCODE ) {
                echoBinMsg( binWriter, null, Integer.valueOf( sVal ), 4 );
                if ( Integer.valueOf( sVal ) == 0 )
                  sVal = "null";
                FileIO.echoMsg( pWriter, sVal + "\n" + valid_mod );
                }
              else {
                throw new Daikon.TerminationMessage( "value of variable " +
                                                     vInfo.name( ) +
                                                     " was of unspecified ProglangType" );
              }
            }
          }
          else {
           FileIO.echoMsg( pWriter, vInfo.name( ) );
           FileIO.echoMsg( pWriter, "nonsensical" );
           FileIO.echoMsg( pWriter, "" + ValueTuple.MISSING_NONSENSICAL );
           }
        }
      } catch ( IOException e ) {
	  e.printStackTrace( System.out );
      }
  }

  // Writes to DataOutputStream a byte, a char, an int, or a long
  // Writes the string msg to the stream, if msg != null
  public static void echoBinMsg( DataOutputStream binWriter, String msg,
                                 long num, int bytes ) {

    if ( binWriter != null ) {
      try {
        switch ( bytes ) {
        case 1: {
          binWriter.writeByte( ( int )num );
          break;
        }
        case 2: {
          binWriter.writeChar( ( int )num );
          break;
        }
        case 4: {
          binWriter.writeInt( ( int )num );
          break;
        }
        case 8: {
          binWriter.writeLong( num );
          break;
        }
        default: {
            throw new Error( );
        }
        }

        if ( msg != null ) {
            binWriter.writeBytes( msg );
        }

      }catch ( IOException e ) {
          e.printStackTrace( );
      }
    }
  }

}
