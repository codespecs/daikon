package daikon.split;

import java.io.*;
import java.util.*;
import utilMDE.*;
import jtb.ParseException;



/**
 * SpinfoFileParser parses a .spinfo file.  This file uses the term "ppt
 * section" and "replace section" to describe what is refered to as a
 * "Program Point Section" and "replacement sections" in Daikon User's
 * Manual, respectively.  A "ppt statement" is a single line from a
 * "ppt section."
 */
class SpinfoFileParser {

  /** The path of the file being parsed. */
  private String spinfoFileName;

  /**
   * The path to directory in which the java files for the
   * splitters are kept.
   */
  private String tempDir;

  /**
   * A Replacer constructed out of the replace statements from the spinfo file.
   */
  private StatementReplacer statementReplacer;

  /**
   * splitterObjects is an array of arrays of SplitterObjects where
   * each array of SplitterObjects contains all the SplitterObjects
   * for a single Ppt.
   */
  // for all i, j, and k:
  // splitterObject[i][j].getPptName() == splitterObject[i][k].getPptName()
  // splitterObject[i][k].getPptName() != splitterObject[j][k].getPptName() || i = j
  private SplitterObject[][] splitterObjects;

  private static String lineSep = System.getProperty("line.separator");

  /**
   * Creates a new SpinfoFileParser to parse spinfoFileName.
   * @param spinfoFile the file to be parsed.
   * @param tempDir the directory in which the splitters' java files are created.
   */
  SpinfoFileParser(File spinfoFile, String tempDir) {
    this.tempDir = tempDir;
    this.spinfoFileName = spinfoFile.toString();
    try {
      LineNumberReader reader = UtilMDE.LineNumberFileReader(spinfoFile.toString());
      parseFile(reader);
    } catch (FileNotFoundException e) {
      throw new RuntimeException(e);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Gets the StatementReplacer for the replace statements in the file parsed.
   * @return the Replacer for the replace statements in the file parsed.
   */
  public StatementReplacer getReplacer() {
    return statementReplacer;
  }

  /**
   * Gets the SplitterObjects for the file parsed.  For each Ppt in the
   * spinfo file, one array of SplitterObjects is created. This method
   * returns an array of those arrays.
   * @return the SplitterObjects for the file parsed.
   */
  public SplitterObject[][] getSplitterObjects() {
    return splitterObjects;
  }

  /**
   * parseFile uses a LineNumberReader of the spinfoFile
   * to read the file, parse it, then set the member fields
   * of this, to the apporiate values.  The member fields
   * set are statementReplacer and splitterObjects.
   * @param spinfoFile a LineNumberReader for the spinfo file being parsed.
   * @throws IOException if an I/O error occurs
   */
  public void parseFile(LineNumberReader spinfoFile)
    throws IOException {
    List /*ReplaceStatement*/ replaceStatements = new ArrayList();
    List /*List<String>*/ pptSections = new ArrayList();
      try {
        String line = spinfoFile.readLine();
        while(line != null) {
          line = line.trim();
          if (line.equals("REPLACE")) {
            handleReplaceStatements(spinfoFile, replaceStatements);
          } else if (line.startsWith("PPT_NAME")) {
            line = line.substring("PPT_NAME".length()).trim();
            handlePptStatements(spinfoFile, pptSections, line);
          } else if (! (isComment(line) || line.equals(""))) {
            throw new
              RuntimeException("Illegal file format in: " + spinfoFileName + lineSep +
                               "at: " + spinfoFile.getLineNumber() + lineSep +
                               line);
          }
          line = spinfoFile.readLine();
        }
      } catch (IOException ioe) {
        //  System.err.println(ioe);
        System.err.println("Error in " +  spinfoFileName + lineSep +
                           " at line number " + spinfoFile.getLineNumber() +
                           " of .spinfo file");
        throw new RuntimeException(ioe);
      } catch (ParseException e) {
        //  System.err.println(ioe);
        System.err.println("Error in " +  spinfoFileName + lineSep +
                           " at line number " + spinfoFile.getLineNumber() +
                           " of .spinfo file");
        throw new RuntimeException(e);
      }
      statementReplacer = new StatementReplacer(replaceStatements);
      splitterObjects = createSplitterObjects(pptSections);
  }




  /**
   * Handles a group of replace statement lines. The method declaration
   * and the return statement of a replace statement is placed in a
   * ReplaceStatement. The ReplaceStatements are then placed into
   * replaceStatements.
   * @param spinfoFile a LineNumberReader for the spinfo file being parsed.
   * @param replaceStatements the List into which the ReplaceStatements
   *  are added.
   */
  private void handleReplaceStatements(LineNumberReader spinfoFile,
                                       List replaceStatements)
    throws IOException, ParseException {
    String methodDeclaration = spinfoFile.readLine();
    while (methodDeclaration != null &&
           (! methodDeclaration.trim().equals(""))) {
      String returnStatement = spinfoFile.readLine();
      if (returnStatement == null || returnStatement.trim().equals("")) {
        throw new RuntimeException("MalFormed .spinfo file in: " +
                                   spinfoFileName + lineSep +
                                   (spinfoFile.getLineNumber() - 1) + lineSep +
                                   methodDeclaration + lineSep +
                                   "Each replace statement must be a pair of lines.");
      }
      ReplaceStatement replaceStatement =
        new ReplaceStatement(methodDeclaration.trim(), returnStatement.trim());
      replaceStatements.add(replaceStatement);
      methodDeclaration = spinfoFile.readLine();
    }
  }

  /**
   * Handles a group of Ppt statements (statements following the line "PPT_NAME...").
   * All the lines between firstLine and the first empty line are placed into a
   * list and then this list is placed in to pptSections.
   * @param spinfoFile a LineNumberReader for the spinfo file being parsed.
   * @param pptSections the List into which the List of lines
   *  for this pptSection are to be added.
   * @param firstLine the first line of the pptSection.
   * @throws IOException if an I/O error occurs.
   */
  private void handlePptStatements(LineNumberReader spinfoFile,
                                   List pptSections,
                                   String pptName)
    throws IOException {
    List /*String*/ pptSection = new ArrayList();
    pptSection.add(pptName);
    String line = spinfoFile.readLine();
    while ((line != null) && (! line.trim().equals(""))) {
      pptSection.add(line);
      line = spinfoFile.readLine();
    }
    pptSections.add(pptSection);
  }

  /**
   * Creates the SplitterObjects for the Ppt section in pptSections.
   * @param pptSections is a List of Lists of Strings. Each list
   *  should include all the lines from a single Ppt Section.
   *  The prefix "PPT_NAME" should not be included in the first line.
   * @return an array of arrays with each array containing the
   *  SplitterObjects for one of lists of ppt statements found in pptSections.
   */
  private SplitterObject[][] createSplitterObjects(List /*List<String>*/ pptSections) {
    List /*SplitterObject[]*/ splittersForAllPpts = new ArrayList();
    for (int i = 0; i < pptSections.size(); i++) {
      List /*String*/ pptSection = (List) pptSections.get(i);
      List /*SplitterObject*/ splittersForThisPpt = new ArrayList();
      if (pptSection.size() > 0) {
        String pptName = ((String) pptSection.get(0)).trim();
        SplitterObject splitObj = null;
        for (int j = 1; j < pptSection.size(); j++) {
          String pptStatement = (String) pptSection.get(j);
          if (isFormatting(pptStatement)) {
            if (splitObj == null) {
              throw new
                RuntimeException("Malformed Spinfo file: " + spinfoFileName + lineSep +
                                 "Format specication, " + pptStatement +
                                 ", must follow a condition");
            } else {
              setFormatting(splitObj, pptStatement.trim());
            }
          } else if (! isComment(pptStatement)) {
            splitObj = new SplitterObject(pptName, pptStatement.trim(), tempDir);
            splittersForThisPpt.add(splitObj);
          }
        }
      }
      splittersForAllPpts.add(splittersForThisPpt.toArray(new SplitterObject[0]));
    }
    return (SplitterObject[][]) splittersForAllPpts.toArray(new SplitterObject[0][0]);
  }

  /**
   * Updates obj's fields to take in account the formatting
   * command given by command.  If the command is invalid an
   * error message is given.  Extra white space is ignored.
   * @param obj the splitterObject for which command is intended.
   * @param command the formatting command to be applied to obj.
   */
  private void setFormatting(SplitterObject obj, String command) {
    command = command.trim();
    if (command.startsWith("DAIKON_FORMAT")) {
      obj.daikonFormat = command.substring("DAIKON_FORMAT".length()).trim();
    } else if (command.startsWith("JAVA_FORMAT")) {
      obj.javaFormat = command.substring("JAVA_FORMAT".length()).trim();
    } else if (command.startsWith("ESC_FORMAT")) {
      obj.escFormat = command.substring("ESC_FORMAT".length()).trim();
    } else if (command.startsWith("SIMPLIFY_FORMAT")) {
      obj.simplifyFormat = command.substring("SIMPLIFY_FORMAT".length()).trim();
    } else if (command.startsWith("IOA_FORMAT")) {
      obj.ioaFormat = command.substring("IOA_FORMAT".length()).trim();
    } else {
      System.err.println("Unrecognized format spec in .spinfo: "
                                 + command);
    }
  }

  /**
   * Returns whether the line is a formatting command.
   * line is a formatting command by if line is indented with
   * a tab, "\t", or spaces, " ".  Formatting command are described
   * in the daikon spinfo file specification and are always indented.
   */
  private static boolean isFormatting(String line) {
    return (line.startsWith("\t") || line.startsWith(" "));
  }

  /**
   * Returns whether the line is a comment line.
   * line is a comment by if line starts with a "#".
   * Comments are provided for in the daikon spinfo
   * file specification and always begin with "#".
   */
  private static boolean isComment(String line) {
    String test = line.trim();
    return (test.startsWith("#"));
  }

}
