package daikon.test;

import daikon.*;

import daikon.config.Configuration;

import daikon.inv.Invariant;
import daikon.inv.Invariant.OutputFormat;
import daikon.inv.unary.UnaryInvariant;
import daikon.inv.binary.BinaryInvariant;
import daikon.inv.ternary.threeScalar.ThreeScalar;

import java.io.*;
import java.lang.reflect.*;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Vector;

import junit.framework.*;

import utilMDE.Assert;
import utilMDE.ArraysMDE;
import utilMDE.Intern;

/**
 * This is a tester for the formatting of invariants in different
 * modes that is configurable by file input. It can test practically
 * any invariant in the Daikon system given the appropriate commands.
 * The test are configured from the InvariantFormatTest.commands file
 * and errors that occur are written to the InvariantFormatTest.diffs
 * file. More detail on the expected formats of these files is in
 * InvariantFormatTester.Description.
 */
public class InvariantFormatTester extends TestCase
{

  /**
   * Maximum file size that can currently be examined by the program, at this
   * point it is essentially arbitrary, but a length had to be chosen
   */
  private static final int MAX_FILE_SIZE = 262144;

  /**
   * Indicates a string that when it starts a line signifies that the
   * line is a comment
   */
  public static final String COMMENT_STARTER_STRING = ";";

  /**
   * A list containing all of the test formats
   */
  public static final List TEST_FORMAT_LIST = getTestFormatList();

  /**
   * Allows for the configuring of Daikon options
   */
  static Configuration config;

  /**
   * Determines whether the next instance of this object will have a
   * true generateGoals variable
   */
  private static boolean goalGenerationForNext;

  /**
   * Determines whether the object will generate goal statements
   */
  private boolean generateGoals;

  /**
   * This function allows this test to be run from the command line
   * instead of its usual method, which is through the Daikon
   * MasterTester.
   *
   * @param args arguments to the main function, which as of now do nothing
   */
  public static void main(String[] args) {
    daikon.Logger.setupLogs (daikon.Logger.INFO);

    goalGenerationForNext = false;
    if (args.length == 1 && args[0].equalsIgnoreCase("--generate_goals"))
      goalGenerationForNext = true;
    else if (args.length > 0) {
      System.out.println("Usage: java daikon.test.InvariantFormatTester" +
			 " [--generate_goals]");
      System.exit(0);
    }

    junit.textui.TestRunner.run(new TestSuite(InvariantFormatTester.class));
  }

  /**
   * This constructor allows the test to be created from the
   * MasterTester class.
   *
   * @param name the desired name of the test case
   */
  public InvariantFormatTester(String name) {
    super(name);
    config = Configuration.getInstance();
    generateGoals = goalGenerationForNext;
  }

  /**
   * This function produces the format list for intialization of the
   * static format list variable
   */
  static List getTestFormatList() {
    List result = new Vector();

    // Add test formats - hard coded in
    result.add("daikon");
    result.add("java");
    result.add("esc");
    result.add("ioa");
    result.add("jml");
    result.add("simplify");

    return result;
  }

  /**
   * This function is the actual function performed when this class is
   * run through JUnit.
   */
  public void testFormats() {

    // Don't care about comparability info because we are only
    // creating variables for the purpose of being compared (thus they
    // should all be comparable)
    Daikon.ignore_comparability = true;

    // Not being configured by a config file anymore

//      // Get the configuration file, each line details a format
//      InputStream configFile =
//        InvariantFormatTester.class.getResourceAsStream("InvariantFormatTester.config");

//      if (configFile == null) {
//        throw new RuntimeException("Configuration file not found," +
//                                   " invariant format test cannot be performed");
//      }

//      BufferedReader inputReader =
//        new BufferedReader(new InputStreamReader(configFile));
//      String currentLine = "";

//      boolean noTestFailed = true;
//      boolean runResult;

//      try {

//        while (currentLine != null) {
//          currentLine = inputReader.readLine();
//          if (currentLine != null) {
//            runResult = run(); // Perform one format's test

//            // Note: do not write: noTestFailed = noTestFailed && run(currentLine)
//            // or else run will not execute when a test has already failed
//            noTestFailed = noTestFailed && runResult;
//          }
//        }
//      }
//      catch (IOException e) {
//        throw new RuntimeException(e.toString());
//      }

    // run the actual test

    if (!execute()) {
      fail("At least one test failed." +
	   " Inspect InvariantFormatTest.diffs for error report.");
    }
  }

  /**
   * Returns the next non-comment, non-whitespace line of the input buffer
   *
   * @param input the input buffer
   * @return the next non-comment, non-whitespace line of the input buffer or
   *         null if the end of the buffer is reached before such a line can be found
   */
  static String getNextRealLine(BufferedReader input) {
    String currentLine = "";

    try {
      while (currentLine != null) {
	currentLine = input.readLine();
	if (currentLine != null && !isComment(currentLine) && !isWhitespace(currentLine))
	  return currentLine;
      }
    }
    catch (IOException e) {
      throw new RuntimeException(e.toString());
    }
    return null;
  }

  /**
   * This function performs the testing for a particular format
   * indicated by the format string. It subsequently sets up
   * appropriate input and output streams for the format test,
   * performs the test, and the compares the test results to the
   * goals.  If the goals differ from the actual results the test
   * fails.
   */
  private boolean execute() {
    // Calculate input file locations
    String inputFile = "InvariantFormatTest.commands";
    String diffsFile = "InvariantFormatTest.diffs";
    InputStream inputStream =
      InvariantFormatTester.class.getResourceAsStream(inputFile);

    if (inputStream == null) {
      // Missing commands file
      System.out.println("Input file for invariant format tests missing." +
			 " (Should be in file " + inputFile + ")");
      System.out.print("Skipping ");
      if (!generateGoals)
	System.out.print("invariant format tests");
      else
	System.out.print("goal generation");
      System.out.println(" due to missing file.");
    } else {

      LineNumberReader commandReader =
	new LineNumberReader(new InputStreamReader(inputStream));

      OutputStream out = new ByteArrayOutputStream();
      boolean result;

      try {
	result = performTest(commandReader, new PrintStream(out));
      }
      catch (RuntimeException e) {
	System.err.println("Error detected on line " +
			   commandReader.getLineNumber() + " of " +
			   inputFile + ":");
	throw e;
      }

      try {
	inputStream.close();
      }
      catch (IOException e) {
	// Can't write the goals into the commands file if it can't be cleared,
	// otherwise not important
	if (generateGoals)
	  throw new RuntimeException("Can't close commands file," +
				   " and so goals cannot be written in");
      }

      String output = out.toString();

      if (generateGoals) {
	File theOutputFile = new File(inputFile);
	theOutputFile.delete();

	try {
	theOutputFile.createNewFile();
	}
	catch (IOException e) {
	  throw new RuntimeException("Cannot recreate the commands file," +
				     " file is now deleted (get it from CVS)");
	}

	FileWriter goalGenerationOutput;

	try {
	  goalGenerationOutput = new FileWriter(theOutputFile);
	}
	catch (IOException e) {
	  throw new RuntimeException("Cannot write output into the recreated commands file," +
				     " file is now deleted (get it from CVS)");
	}

	try {
	  goalGenerationOutput.write(output, 0, output.length());
	  goalGenerationOutput.close();
	}
	catch (IOException e) {
	  throw new RuntimeException("Could not output generated goals");
	}

	System.out.println("Goals generated");
      } else {
	File theOutputFile = new File(diffsFile);
	theOutputFile.delete();

	if (!result) {
	  try {
	    theOutputFile.createNewFile();

	    FileWriter diffsOutput = new FileWriter(theOutputFile);

	    diffsOutput.write(output, 0, output.length());
	    diffsOutput.close();
	  }
	  catch (IOException e) {
	    throw new RuntimeException("Could not output generated diffs");
	  }

	  return false;
	}
      }
    }
    return true;
  }

  /**
   * This function performs an individual formatting test after the
   * input and output streams have been created.
   *
   * @param commands the input that decides which tests to perform
   * @param output the place to where the test output is written
   */
  private boolean performTest(LineNumberReader commands, PrintStream output) {
    List invariantTestCases = new Vector();
    FormatTestCase currentCase;
    boolean stop = false, noTestFailed = true;

    // Need to be able to go to beginning of buffer for combining goals with the input
    if (generateGoals) {
      try {
	commands.mark(MAX_FILE_SIZE);
      }
      catch (IOException e) {
	throw new RuntimeException("Cannot mark file in order to generate goals");
      }
    }

    while (!stop) {
      // Create a new test case
      currentCase = FormatTestCase.instantiate(commands, generateGoals);
      if (currentCase == null)
	stop = true;
      else {
	invariantTestCases.add(currentCase);
	if (!generateGoals && !currentCase.passes()) {
	  output.print(currentCase.createDiffString());
	  noTestFailed = false;
	}
      }
    }

    if (generateGoals) {

      // Go to beginning of the commands buffer
      try {
	commands.reset();
      }
      catch (IOException e) {
	throw new RuntimeException("Cannot reset to mark, thus cannot write goals");
      }

      String debugTemp;

      try {
	for (int i=0; i<invariantTestCases.size(); i++) {
	  currentCase = (FormatTestCase)invariantTestCases.get(i);
	  // System.out.println("Goal output #" + i);
	  debugTemp = currentCase.generateGoalOutput(commands);
	  // System.out.println(debugTemp);

	  output.println(debugTemp);
	}

	String currentLineOfText = commands.readLine();

	while (currentLineOfText != null) {
	  if (FormatTestCase.parseGoal(currentLineOfText) == null)
	    output.println(currentLineOfText);
	  currentLineOfText = commands.readLine();
	}
      }
      catch (IOException e) {
	throw new RuntimeException("Writing goal output failed");
      }
    }
    return noTestFailed;
  }

  /**
   * Determines whether a line is a comment or not
   *
   * @param line the line in question
   * @return true if the line is a comment (that is, not to be interpretted as a command)
   *         false otherwise
   */
  static boolean isComment(String line) {
    return line.startsWith(COMMENT_STARTER_STRING);
  }

  /**
   * Determines whether a given line is made only of whitespcae
   *
   * @param line the line in question
   * @return true if the line is made up only of whitespace, false otherwise
   */
  static boolean isWhitespace(String line) {
    for (int x=0; x<line.length(); x++) {
      if (!isWhitespace(line.charAt(x)))
	return false;
    }
    return true;
  }

  /**
   * Determines whether a given character is considered whitespace
   *
   * @param c the character in question
   * @return true if the character is whitespace, false otherwise
   */
  private static boolean isWhitespace(char c) {
    return (c == ' ' || c == '\t');
  }
}










