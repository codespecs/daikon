package daikon.tools;
import java.io.*;
import java.util.*;
import utilMDE.*;
import daikon.*;
import daikon.config.Configuration;
import java.util.regex.*;
import gnu.getopt.*;

import java.util.zip.GZIPOutputStream;

/**
 * This tool converts between the uncompressed and compressed Daikon file
 * formats.
 */
public class DTraceConverter {

    public static final String toBinary = "-2Bin";
    public static final String toASCII = "-2ASCII";

    static String usage = "USAGE: java daikon.DTraceConverter [-2Bin | -2ASCII] [-o] <filenames> \n" +
	                  "<filenames> alternate between [.dat] and [.dtrace] files," +
	                  " if -2ASCII option is specified \n" +
	                  "<filenames> are just .dtrace files, if -2Bin option is specified";


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
     * This entry point is useful for testing.  It returns a boolean to indicate
     * return status instead of croaking with an error.
     **/

    /**
     * This does the work of main, but it never calls System.exit, so it
     * is appropriate to be called progrmmatically.
     * Termination of the program with a message to the user is indicated by
     * throwing daikon.Daikon.TerminationMessage.
     * @see #main(String[])
     * @see daikon.Daikon.TerminationMessage
     **/
    public static void mainHelper( final String[] args ) {
	if (args.length < 2 ) {
	    throw new daikon.Daikon.TerminationMessage( usage );
	}
        boolean dbg = false;
        int fn = 1;
        if ( args[fn].equals( "-o" ) ) {
            fn++;
            dbg = true;
        }

	if ( args[0].equals( toASCII ) ) {
	    for ( ; fn < args.length; fn++ ) {
		if ( args[fn].substring( args[fn].length( ) - 4 ).equals(".dat" ) ) {
		    dtraceConvert2ASCII( args[fn], dbg );
		}
		else {
		    throw new daikon.Daikon.TerminationMessage( usage );
		}
	    }
	}
	else if ( args[0].equals( toBinary ) ) {
	    for ( ; fn < args.length; fn++ ) {
                if ( args[fn].substring( args[fn].length( ) - 7 ).equals( ".dtrace" )
                    || args[fn].substring( args[fn].length( ) - 10 ).equals( ".dtrace.gz" ) ) {
                    dtraceConvert2Binary( args[fn], dbg );
                }
                else {
                    throw new daikon.Daikon.TerminationMessage( usage );
                }
	    }
	}
	else {
	    throw new daikon.Daikon.TerminationMessage( usage );
	}
    }

    public static void stdoutMsg( String msg, boolean dbg ) {
        if ( dbg )
            System.out.println( msg );
    }

    // Doesn't read gzipped binary files
    // Creates a text dtrace file equivalent to the input binary file
    public static void dtraceConvert2ASCII( String binfile,  boolean dbg ) {
	PptMap ppts = new PptMap();

	PrintWriter pWriter = null;
        OutputStream oStream = null;

        String testDTrace = binfile + ".dtrace";
        String testBin = binfile.replaceAll( "dat", "test.dat" );
	try {
	    pWriter = new PrintWriter( new FileOutputStream( testDTrace ), true );
            oStream = new GZIPOutputStream( new FileOutputStream( testBin ) );
	}
	catch ( IOException e ) {
	    e.printStackTrace( );
	}

	try {
            FileIO.ParseState state =
                new FileIO.ParseState( binfile, false, true, ppts );

            while (state.status != FileIO.ParseStatus.EOF) {
                FileIO.read_binary_data_trace_record ( state, pWriter );
                switch(state.status) {
                case NULL:
                    stdoutMsg( "Got null", dbg );
                    break;
                case TRUNCATED:
                    stdoutMsg( "Got truncated", dbg );
                    break;
                case ERROR:
                    stdoutMsg( "Got an error", dbg );
                    break;
                case EOF:
                    stdoutMsg( "Got EOF", dbg );
                    break;
                case DECL: {
                    stdoutMsg( "Got a decl", dbg );
                    break;
                }
                case SAMPLE: {
                    stdoutMsg( "Got a sample", dbg );
                    break;
                }
                case COMPARABILITY: {
                    stdoutMsg( "Got comparability", dbg );
                    break;
                }
                case LIST_IMPLEMENTORS: {
                    stdoutMsg( "Got list", dbg );
                    break;
                }
                case INPUT_LANGUAGE: {
                    stdoutMsg( "Got input language", dbg );
                    break;
                }
                case DECL_VERSION: {
                    stdoutMsg( "Got version", dbg );
                    break;
                }
                case COMMENT: {
                    stdoutMsg( "Got a comment line", dbg );
                    break;
                }
                default:
                    throw new Error();
                }
            }
	} catch (IOException e) {
	    System.out.println();
	    e.printStackTrace();
	    throw new Error(e);
	}
    }

    // Writes the dtracefile data in a binary file
    // Creates a text dtrace file equivalent to the input dtrace.gz file
    public static void dtraceConvert2Binary( String dtracefile, boolean dbg  ) {
	PptMap ppts = new PptMap();

	DataOutputStream binWriter = null;
	PrintWriter pWriter = null;

	String binfile = dtracefile.substring( 0, dtracefile.length( )-3 ) + ".dat";
        String testDTrace = binfile + ".txt"; //.dat.txt
	try {
	    binWriter = new DataOutputStream( new FileOutputStream( binfile ) );
            pWriter = new PrintWriter( new FileOutputStream( testDTrace ), true );
	}
	catch ( IOException e ) {
	    e.printStackTrace( );
	}

	try {
	    FileIO.ParseState state =
		new FileIO.ParseState ( dtracefile, false, true, ppts );

	    while (state.status != FileIO.ParseStatus.EOF) {
		FileIO.read_data_trace_record (state);
		switch(state.status) {
		case NULL:
		    stdoutMsg( "Got null", dbg );
		    break;
		case TRUNCATED:
		    stdoutMsg( "Got truncated", dbg );
		    break;
		case ERROR:
		    stdoutMsg( "Got an error", dbg );
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
		    FileIO.write_binarySample( pWriter, binWriter, state.all_ppts, state.ppt, state.vt, state.nonce );
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
		    pWriter.println( identifiers[0] + " " +  identifiers[1] );

		    for ( int i = 0; i < identifiers.length; i++ ) {
			binWriter.writeByte( identifiers[i].length( ) );
			binWriter.writeBytes( identifiers[i] );
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
		    String implementors = "ListImplementors ";
		    for ( String impl : ProglangType.list_implementors ) {
			implementors += impl + " " ;
			identifiers[++index] = impl;
		    }

		    stdoutMsg( "Got list", dbg );
		    pWriter.println( implementors );

		    for ( int i = 0; i < size; i++ ) {
			binWriter.writeByte( identifiers[i].length( ) );
			binWriter.writeBytes( identifiers[i] );
		    }
		    break;
		}
		case INPUT_LANGUAGE: {
		    String [] identifiers = new String [2];
		    identifiers[0] = "input_language";
		    identifiers[1] = FileIO.input_lang;

		    stdoutMsg( "Got input language", dbg );
		    pWriter.println( identifiers[0] + identifiers[1] );

		    for ( int i = 0; i < identifiers.length; i++ ) {
			binWriter.writeByte( identifiers[i].length( ) );
			binWriter.writeBytes( identifiers[i] );
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
		    pWriter.println( "\ndecl-version " + identifiers[1] );

		    for ( int i = 0; i < identifiers.length; i++ ) {
			binWriter.writeByte( identifiers[i].length( ) );
			binWriter.writeBytes( identifiers[i] );
		    }
		    break;
		}
		case COMMENT: {
		    String [] identifiers = new String [2];
		    identifiers[0] = "" + (state.payload.trim()).charAt( 0 );
		    if ( identifiers[0].equals( "/" ) )
			identifiers[0] += state.payload.charAt( 1 );
                    else
                        pWriter.println( "\n" );

                    binWriter.writeByte( identifiers[0].length( ) );
                    binWriter.writeBytes( identifiers[0] );

		    identifiers[1] = state.payload;

                    // comments can be long (a byte for their length appears insufficient)
                    binWriter.writeInt( identifiers[1].length( ) );
                    binWriter.writeBytes( identifiers[1] );

		    stdoutMsg( "Got a comment line", dbg );
		    pWriter.println( state.payload );

		    break;
		}
		default:
		    throw new Error();
		}
	    }
	} catch (IOException e) {
	    System.out.println();
	    e.printStackTrace();
	    throw new Error(e);
	}
    }
}