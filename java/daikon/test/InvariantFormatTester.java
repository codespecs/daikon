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
 * This is a tester for the formatting of invariants in different modes that is configurable by file
 * input. It can test practically any invariant in the Daikon system given the appropriate commands.
 * The tester is first configured by the InvariantFormatTester.conig file, which details the different
 * formatting modes to be tested, and then accepts commands from the according
 * InvariantFormatTest.<format name> and InvariantFormatTest.<format name>.goal files for input and
 * desired results respectively. More detail on the expected formats of these files is in
 * InvariantFormatTester.Description.
 */
public class InvariantFormatTester extends TestCase
{
  /**
   * Allows for the configuring of Daikon options
   */
  private Configuration config;

  /**
   * Determines whether the next instance of this object will have a true generateGoals variable
   */
  private static boolean goalGenerationForNext;

  /**
   * Determines whether the object will generate goal files when run
   */
  private boolean generateGoals;

  /**
   * This function allows this test to be run from the command line instead of its usual method, which
   * is through the Daikon MasterTester.
   *
   * @param args arguments to the main function, which as of now do nothing
   */
  public static void main(String[] args) {
    daikon.Logger.setupLogs (daikon.Logger.INFO);

    goalGenerationForNext = false;
    if (args.length == 1 && args[0].equalsIgnoreCase("--generate_goals"))
      goalGenerationForNext = true;
    else if (args.length > 0)
      System.out.println("Usage: java daikon.test.InvariantFormatTester [--generate_goals]");

    junit.textui.TestRunner.run(new TestSuite(InvariantFormatTester.class));
  }

  /**
   * This constructor allows the test to be created from the MasterTester class.
   *
   * @param name the desired name of the test case
   */
  public InvariantFormatTester(String name) {
    super(name);
    config = Configuration.getInstance();
    generateGoals = goalGenerationForNext;
  }

  /**
   * This function is the actual function performed when this class is run through JUnit.
   */
  public void testFormats() {

    // Don't care about comparability info because we are only creating variables for the purpose of being
    // compared (thus they should all be comparable)
    Daikon.ignore_comparability = true;

    // Get the configuration file, each line details a format
    InputStream configFile = InvariantFormatTester.class.getResourceAsStream("InvariantFormatTester.config");

    if (configFile == null) {
      throw new RuntimeException("Configuration file not found, invariant format test cannot be performed");
    }

    BufferedReader inputReader = new BufferedReader(new InputStreamReader(configFile));
    String currentLine = "";

    try {

      while (currentLine != null) {
	currentLine = inputReader.readLine();
	if (currentLine != null)
	  run(currentLine); // Perform one format's test
      }
    }
    catch (IOException e) {
      throw new RuntimeException(e.toString());
    }
  }

  /**
   * This function performs the testing for a particular format indicated by the format string. It subsequently
   * sets up appropriate input and output streams for the format test, performs the test, and the compares the
   * test results to the goal file. If the goal file differs from the actual result the test fails.
   *
   * @param format a string representing the format to be tested
   */
  private void run(String format) {
    String inputFile = "InvariantFormatTest." + (format.equalsIgnoreCase("java") ? "testJava" : format);
    String goalFile = inputFile + ".goal";
    InputStream inputStream = InvariantFormatTester.class.getResourceAsStream(inputFile);
    InputStream goalStream = null;
    FileWriter goalGenerationOutput = null;

    if (!generateGoals) {
      goalStream = InvariantFormatTester.class.getResourceAsStream(goalFile);
    }

    if (inputStream == null || (goalStream == null && !generateGoals)) {
      if (inputStream == null)
	System.out.println("Input file for " + format + " invariant format test missing. (Should be in file " + inputFile + ")");
      if (goalStream == null && !generateGoals)
	System.out.println("Goal file for " + format + " invariant format test missing. (Should be in file " + goalFile + ")");
      System.out.print("Skipping " + format + " ");
      if (!generateGoals)
	System.out.print("invariant format test");
      else
	System.out.print("goal generation");
      System.out.println(" due to missing files.");
    } else {
      if (generateGoals) {
	try {
	  goalGenerationOutput = new FileWriter(new File(goalFile));
	}
	catch (IOException e) {
	  throw new RuntimeException(e.toString());
	}
      }

      BufferedReader commandReader = new BufferedReader(new InputStreamReader(inputStream));

      ByteArrayOutputStream out = new ByteArrayOutputStream();
      performTest(commandReader,new PrintStream(out),format);

      if (generateGoals) {
	String output = out.toString();

	try {
	  goalGenerationOutput.write(output,0,output.length());
	  goalGenerationOutput.close();
	}
	catch (IOException e) {
	  throw new RuntimeException("Could not output generated goals");
	}

	System.out.println(format + " goals generated");
      } else {
	// put output into actual
	List actualList = new ArrayList();
	StringTokenizer tok = new StringTokenizer(out.toString(), "\r\n");
	while (tok.hasMoreTokens()) {
	  actualList.add(tok.nextToken());
	}
	String[] actual = (String[]) actualList.toArray(new String[actualList.size()]);

	// put desired into goal
	List goalList = new ArrayList();
	try {
	  BufferedReader buf = new BufferedReader(new InputStreamReader(goalStream));
	  while (buf.ready()) {
	    String line = buf.readLine();
	    goalList.add(line);
	  }
	} catch (IOException e) {
	  throw new RuntimeException(e.toString());
	}

	String[] goal = (String[]) goalList.toArray(new String[goalList.size()]);

	diff(goal,actual);
      }
    }
  }

  /**
   * This function performs an individual formatting test after the input and output streams have been created.
   *
   * @param commands the input that decides which tests to perform
   * @param output the place to where the output is written
   * @param format the format string that represents the test
   */
  private void performTest(BufferedReader commands,PrintStream output,String format) {
    while (performIndividualTest(commands,output,format)) {}
  }

  /**
   * This function performs a format test on a specific invariant which is decided by the data in the
   * commands variable
   *
   * @param commands the input that decides which test to perform
   * @param output the place to where the outut is written
   * @param format a string representing the format
   * @return true if the end of the input file was reached, false otherwise
   */
  private boolean performIndividualTest(BufferedReader commands,PrintStream output,String format) {
    String className = getNextRealLine(commands);

    if (className == null) return false;

    Class classToTest = getClass(className); // Load the class from file

    try {
      classToTest.getField("dkconfig_enabled"); // Enable if needs to be done
      config.apply(className+".enabled","true");
    }
    catch (NoSuchFieldException e) { // Otherwise do nothing
    }

    // Instantiate variables to be used as the names in the invariants, variables are
    // labelled a,b,c and so on as they appear
    VarInfo vars[] = getVarInfos(classToTest,getTypes(getNextRealLine(commands)));
    PptSlice sl = createSlice(vars,Common.makePptTopLevel("Test",vars));

    // Create an actual instance of the class
    Invariant inv = instantiateClass(classToTest,sl);

    if (inv == null)
      throw new RuntimeException("Could not instantiate invariant");

    List samples = new Vector();

    // Get samples if they are needed to determine invariant data
    // e.g. to determine the exact nature of a linear relationship between
    // variables x and y we need two data points
    boolean returnValue = getSamples(classToTest,commands,samples);

    // System.out.println(inv);
    // System.out.println(samples);

    // Use the add_modified function of the appropriate invariant to add the data to the instance
    populateWithSamples(inv,samples);

    Method outputMethod = null;
    int method = 0;

    // Get the method used to perform the formatting
    try {
      outputMethod = classToTest.getMethod("format_" + format,null);
    }
    catch (NoSuchMethodException e) {
      try {
	outputMethod = classToTest.getMethod("format_using",new Class [] {OutputFormat.class});
	method = 1;
      }
      catch (NoSuchMethodException e2) {
	throw new RuntimeException("Could not find format method");
      }
    }

    output.println(className);

    // Call the formatting function, write out the output
    try {
      if (method == 0)
	output.println((String)outputMethod.invoke(inv,null));
      else if (method == 1)
	output.println((String)outputMethod.invoke(inv,new Object [] {getOutputFormat(format)}));
      else
	throw new RuntimeException("Invalid format method detected");
    }
    catch (IllegalAccessException e) {
      throw new RuntimeException("format method is not accessible on the invariant");
    }
    catch (IllegalArgumentException e) {
      throw new RuntimeException("Unexpected exception regarding the arguments of format function (there are no arguments, so this is strange)");
    }
    catch (InvocationTargetException e) {
      throw new RuntimeException("Format method threw an exception");
    }
    catch (ExceptionInInitializerError e) {
      throw new RuntimeException("Initialization error within invoke during printing");
    }

    return returnValue;
  }

  /**
   * This function determines the corresponding OutputFormat variable given a format string such that
   * the format_using function can be used.
   *
   * @param format a string representing the format
   * @return an OutputFormat object representing the output type if format corresponds to any known formats
   *         null otherwise
   */
  private static OutputFormat getOutputFormat(String format) {
    if (format.equalsIgnoreCase("daikon"))
      return OutputFormat.DAIKON;
    else if (format.equalsIgnoreCase("java"))
      return OutputFormat.JAVA;
    else if (format.equalsIgnoreCase("esc"))
      return OutputFormat.ESCJAVA;
    else if (format.equalsIgnoreCase("ioa"))
      return OutputFormat.IOA;
    else if (format.equalsIgnoreCase("simplify"))
      return OutputFormat.SIMPLIFY;
    else if (format.equalsIgnoreCase("jml"))
      return OutputFormat.JML;
    return null;
  }

  /**
   * This function fills the samples list with samples that can be used by the populateWithSamples
   * function to add data to the Invariant. It reads the appropriate sample data from the
   * commands reader and parses the data appropriately
   *
   * @param classToTest the class of the invariant being tested
   * @param commands the input file for the commands
   * @param samples the list to which the samples are to be added
   * @return false if the end of commands was reached, true otherwise
   */
  private static boolean getSamples(Class classToTest,BufferedReader commands,List samples) {
    Class paramTypes[] = getAddModified(classToTest).getParameterTypes();

    Object sample[];

    String currentLine;

    try {
      currentLine = commands.readLine(); // Get first line
    }
    catch (IOException e) {
      throw new RuntimeException(e.toString());
    }

    while (currentLine != null && !isWhitespace(currentLine) && !isComment(currentLine)) {
      sample = new Object [paramTypes.length-1];
      for (int i=0;i<paramTypes.length-1;i++) {
	// Parse each line according to a type in the paramTypes array
	parse(paramTypes[i],i,sample,currentLine);
	// System.out.println("Debug: current sample: sample[" + i + "] == " + sample[i]);
	try {
	  currentLine = commands.readLine();
	}
	catch (IOException e) {
	  throw new RuntimeException(e.toString());
	}
      }
      samples.add(sample);
    }

    return (currentLine != null);
  }

  /**
   * This function parses a string or a section of a string into an appropriate data type
   * as indicated by the type class. The result is stored into sample[sampleIndex]
   *
   * @param type the type of the object to create
   * @param sampleIndex the index in the sample array in which the result is to be stored
   * @param sample the array in which the result is to be stored
   * @param toBeParsed the String to be parsed for the result
   */
  public static void parse(Class type,int sampleIndex,Object sample[],String toBeParsed) {
    Method parser;
    String typeName = type.getName();
    String arrayFunctionName;
    Method arrayFunction;

    try {
      if (type.isPrimitive()) { // Primitive types
	Class wrapper = getWrapperClass(type);

	if (wrapper != null) {
	  // Use the valueOf function in the wrapper of the primitive type to create the result
	  parser = wrapper.getMethod("valueOf",new Class [] {String.class});

	  // Use the Array.set to set the appropriate spot in the array to the result
	  // of the parser method

	  arrayFunction = Array.class.getMethod("set",new Class [] {Object.class,int.class,Object.class});

	  Object arrayFunctionParams[] = new Object [3];
	  arrayFunctionParams[0] = (Object)sample;
	  arrayFunctionParams[1] = new Integer(sampleIndex);
	  try {
	    // Get the result
	    arrayFunctionParams[2] = parser.invoke(null,new Object [] {toBeParsed});
	  }
	  catch (Exception e) {
	    throw new RuntimeException("Error in invoking parser for primitive object");
	  }

	  try {
	    // Put the result into the array
	    arrayFunction.invoke(null,arrayFunctionParams);
	  }
	  catch (Exception e) {
	    throw new RuntimeException("Error in invoking arrayFunction to put result in sample");
	  }
	}
	else
	  throw new RuntimeException("Could not find wrapper class for primitive");
      } else if (type.isArray()) { // Array type - only parses single dimensional now
	Class subType = type.getComponentType();

	StringTokenizer stok = new StringTokenizer(toBeParsed);
	int arrayLength = stok.countTokens();
	Object temp[] = new Object [arrayLength];

	// Recursively call parse on the substrings to get the array entries
	for (int i=0;i<arrayLength;i++) {
	  parse(subType,i,temp,stok.nextToken());
	}

	Object result = Array.newInstance(subType,arrayLength);

	// If primitive subtype, must use appropriate set function to get entries into the
	// result array
	if (subType.isPrimitive()) {
	  String subTypeName = subType.getName();
	  String capsSubTypeName = new String(new char [] {Character.toUpperCase(subTypeName.charAt(0))}) + subTypeName.substring(1,subTypeName.length());

	  arrayFunctionName = "set" + capsSubTypeName;
	  arrayFunction = Array.class.getMethod(arrayFunctionName,new Class [] {Object.class,int.class,subType});

	  for (int i=0;i<arrayLength;i++) {
	    arrayFunction.invoke(null,new Object [] {result,new Integer(i),temp[i]});
	  }
	} else {
	  for (int i=0;i<arrayLength;i++) {
	    Array.set(result,i,temp[i]);
	  }
	}

	sample[sampleIndex] = result;

      } else { // Non-array (non-primitive) objects
	try {
	  if (type != String.class) {
	    parser = type.getMethod("valueOf",new Class [] {String.class});
	    sample[sampleIndex] = parser.invoke(null,new Object [] {toBeParsed});
	  }
	  else
	    sample[sampleIndex] = toBeParsed;
	}
	catch (Exception e) {
	  throw new RuntimeException("Error in invoking parser on complex type - no way to create one from a String");
	}
      }
    }
    catch (IllegalAccessException e) {
      throw new RuntimeException(e.toString());
    }
    catch (InvocationTargetException e) {
      throw new RuntimeException(e.toString());
    }
    catch (NoSuchMethodException e) {
      throw new RuntimeException(e.toString());
    }
    catch (SecurityException e) {
      throw new RuntimeException("SecurityException generated that indicates reflection has been disallowed");
    }
  }

  /**
   * This function generates the wrapper class for a primitive class
   *
   * @param type the type for which the wrapper class is to be generated
   * @return the corresponding wrapper class if type is a primitive type,
   *         null otherwise
   */
  private static Class getWrapperClass(Class type) {
    if (type.equals(int.class))
      return Integer.class;
    else if (type.equals(long.class))
      return Long.class;
    else if (type.equals(double.class))
      return Double.class;
    else if (type.equals(float.class))
      return Float.class;
    else if (type.equals(boolean.class))
      return Boolean.class;
    else if (type.equals(byte.class))
      return Byte.class;
    else if (type.equals(char.class))
      return Character.class;
    else if (type.equals(short.class))
      return Short.class;
    return null;
  }

  /**
   * This function adds the samples in the samples list to the passed in invariant
   * by use of the appropriate add_modified function (determined by reflection).
   *
   * @param inv an invariant to which samples are added
   * @param samples a list of samples (Object []) that can be added to the variables
   *        involved
   */
  private static void populateWithSamples(Invariant inv,List samples) {
    if (samples == null || samples.size() == 0) return;

    // System.out.println(inv.getClass().getName());
    // System.out.println(samples.size());

    Method addModified = getAddModified(inv.getClass());
    Object params[],currentSample[];
    int sampleSize = ((Object [])samples.get(0)).length;
    Class currentClass;

    // System.out.println(sampleSize);

    for (int i=0;i<samples.size();i++) {
      params = new Object [sampleSize+1];
      currentSample = (Object [])samples.get(i);

      for (int j=0;j<sampleSize;j++) {
	currentClass = currentSample[j].getClass();

	// Intern all objects that can be interned
	if (currentClass.equals(String.class)) {
	  currentSample[j] = Intern.intern(currentSample[j]);
	} else if (currentClass.isArray()) {
	  currentSample[j] = Intern.intern(currentSample[j]);
	  if (currentClass.getComponentType().equals(String.class)) {
	    for (int k=0;k<((String [])(currentSample[j])).length;k++) {
	      ((String [])currentSample[j])[k] = Intern.intern(((String [])currentSample[j])[k]);
	    }
	  }
	}

	params[j] = currentSample[j];
      }

      // Set count to 1
      Array.set(params,sampleSize,new Integer(1));

      // Debug code

      //        System.out.println("Sample #" + (i+1) + " of " + samples.size());
      //        System.out.println("P0: " + params[0] + "\nP1: " + params[1]);
      //        System.out.println("P0 is array: " + params[0].getClass().isArray() + " type: " + params[0].getClass().getComponentType());
      //        System.out.println("P1 is array: " + params[1].getClass().isArray() + " type: " + params[1].getClass().getComponentType());

      //        for (int y=0;y<sampleSize;y++) {
      //          try {
      //            if (params[y].getClass().isArray()) {
      //              System.out.print("P" + y + " array representation: ");
      //              for (int x=0; ; x++) {
      //                System.out.print(Array.get(params[y],x) + " ");
      //              }
      //            }
      //          }
      //          catch (ArrayIndexOutOfBoundsException e) {
      //            System.out.println();
      //          }
      //        }

      try {
	addModified.invoke(inv,params);
      }
//        catch (Exception e) {
//    	throw new RuntimeException(e.toString());
//        }
      catch (Exception e) {
    	throw new RuntimeException("Error in populating invariant with add_modified");
      }
    }
  }

  /**
   * This function gets the appropriate add_modified function from the class type
   * provided and returns it as a Method
   *
   * @param theClass the class in which to find the add_modified method
   * @return the add_modified method if it exists, null otherwise
   */
  private static Method getAddModified(Class theClass) {
    Method methods[] = theClass.getMethods();

    Method currentMethod;
    for (int i=0;i<methods.length; i++) {
      currentMethod = methods[i];
      if (currentMethod.getName().lastIndexOf("add_modified") != -1) { // Method should be called add_modified
	return currentMethod;
      }
    }
    return null;
  }

  /**
   * This function creates an appropriate PptSlice for a given set of VarInfos and
   * a PptTopLevel
   *
   * @param vars an array of VarInfo objects for which the slice is to be created
   * @param ppt the PptTopLevel object representing the program point
   * @return a new PptSlice object if the creation of one is possible,
   *         else null
   */
  private static PptSlice createSlice(VarInfo vars[],PptTopLevel ppt) {
    if (vars.length == 1)
      return new PptSlice1(ppt,vars);
    else if (vars.length == 2)
      return new PptSlice2(ppt,vars);
    else if (vars.length == 3)
      return new PptSlice3(ppt,vars);
    else
      return null;
  }

  /**
   * This function determines the arity of a given invariant given its class
   *
   * @param classToTest the invariant type in question
   * @return the arity of the invariant if it can be determined, -1 otherwise
   */
  private static int getArity(Class classToTest) {
    if (UnaryInvariant.class.isAssignableFrom(classToTest))
      return 1;
    else if (BinaryInvariant.class.isAssignableFrom(classToTest))
      return 2;
    if (ThreeScalar.class.isAssignableFrom(classToTest))
      return 3;

    return -1;
  }

  /**
   * This function creates an array of VarInfo objects that can represent a set
   * of program language types provided by the caller. These objects are meant
   * explicitly for the purpose of this testing and have no real other significance.
   * Their names carry no meaning except for the type.
   *
   * @param classToTest the invariant class for which the VarInfos must be determined
   * @param types the types that the VarInfos must have
   * @return an array of VarInfo objects that have the types corresponding to those
   *         in types
   */
  private static VarInfo [] getVarInfos(Class classToTest,ProglangType types[]) {
    int numInfos = getArity(classToTest);

    if (numInfos == -1)
      throw new RuntimeException("Class arity cannot be determined.");

    VarInfo result[] = new VarInfo [numInfos];

    for(int i=0;i<numInfos;i++) {
      result[i] = getVarInfo(types[i],i);
    }

    return result;
  }

  /**
   * This function determines a unique VarInfo for an invariant given a type and
   * a number that is unique for that particular instance of the invariant.
   * (Produces variables such that i=0 -> name=a, i=1 -> name=b, ...)
   *
   * @param type the desired type that the VarInfo will represent
   * @param i a unique identifier that determines the name to be used
   * @return a VarInfo object that described the type
   */
   private static VarInfo getVarInfo(ProglangType type,int i) {
    if (type == null)
      return null;

    String arrayModifier = "";

    if (type == ProglangType.INT_ARRAY ||
	type == ProglangType.DOUBLE_ARRAY ||
	type == ProglangType.STRING_ARRAY) { // Is it an array ?
      arrayModifier = "[]";
    }

    return new VarInfo (VarInfoName.parse(new String(new char [] {(char)('a' + i)})
                                          + arrayModifier),
                        type, type, null, VarInfoAux.getDefault());
  }

  /**
   * This function parses a format string and determines the types of objects to be collected
   *
   * @param typeNames the type string for an invariant
   * @return an array of ProglangTypes representing the data in typeNames
   */
  private static ProglangType [] getTypes(String typeNames) {
    StringTokenizer stok = new StringTokenizer(typeNames);
    ProglangType result[] = new ProglangType [stok.countTokens()];
    String typeName;

    for (int i=0;i<result.length;i++) {
      typeName = stok.nextToken();

      if (typeName.equalsIgnoreCase("int"))
	result[i] = ProglangType.INT;
      else if (typeName.equalsIgnoreCase("double"))
	result[i] = ProglangType.DOUBLE;
      else if (typeName.equalsIgnoreCase("string"))
	result[i] = ProglangType.STRING;
      else if (typeName.equalsIgnoreCase("int_array"))
	result[i] = ProglangType.INT_ARRAY;
      else if (typeName.equalsIgnoreCase("double_array"))
	result[i] = ProglangType.DOUBLE_ARRAY;
      else if (typeName.equalsIgnoreCase("string_array"))
	result[i] = ProglangType.STRING_ARRAY;
      else
	return null;
    }

    return result;
  }

  /**
   * Returns the next non-comment, non-whitespace line of the input buffer
   *
   * @param input the input buffer
   * @return the next non-comment, non-whitespace line of the input buffer or
   *         null if the end of the buffer is reached before such a line can be found
   */
  private static String getNextRealLine(BufferedReader input) {
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
   * Determines whether a line is a comment or not
   *
   * @param line the line in question
   * @return true if the line is a comment (that is, not to be interpretted as a command)
   *         false otherwise
   */
  private static boolean isComment(String line) {
    return line.startsWith(";");
  }

  /**
   * Determines whether a given line is made only of whitespcae
   *
   * @param line the line in question
   * @return true if the line is made up only of whitespace, false otherwise
   */
  private static boolean isWhitespace(String line) {
    for (int x=0;x<line.length();x++) {
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

  /**
   * This function loads a class from file into the JVM given its fully-qualified
   * name.
   *
   * @param classInfo the fully-qualified class name
   * @return the class if it exists, else null
   */
  private static Class getClass(String classInfo) {
    try {
      return ClassLoader.getSystemClassLoader().loadClass(classInfo);
    }
    catch (ClassNotFoundException e) {
      throw new RuntimeException(e.toString());
    }
  }

  /**
   * This function instantiates an invariant class by using the <type>(PptSlice) constructor.
   *
   * @param theClass the invariant class to be instantiated
   * @param sl the PptSlice representing the variables about which an invariant is determined
   * @return an instance of the class in theClass if one can be constructed,
   *         null otherwise
   */
  private static Invariant instantiateClass(Class theClass,PptSlice sl) {
    Method instanceCreator;
    try {
      instanceCreator = theClass.getMethod("instantiate",new Class [] {PptSlice.class});
    } catch (Exception e) {
      throw new RuntimeException("Error while instantiating " + theClass.getName() + ": " + e.toString());
    }

    if (instanceCreator == null) return null;

    try {
      return (Invariant)instanceCreator.invoke(null,new Object [] {sl});
    } catch (Exception e) {
      throw new RuntimeException("Error while invoking \"instantiate\" on " + theClass.getName() + ": " + e.toString());
    }
//      catch (Exception e) {
//        throw new RuntimeException("Could not instantiate class");
//      }
  }

  /**
   * This function is taken from the VarInfoNameTest code, and it calculates the different
   * between two String arrays and asserts a failute if the difference is non-empty
   *
   * @param goal one of the two String arrays
   * @param actual the other of the two String arrays
   */
  private void diff(String[] goal, String[] actual) {
    for (int i=0; i < goal.length; i++) {
      String goal_line = goal[i];
      if (i >= actual.length) {
	fail("Diff error:\nActual had too few lines, starting with goal line:\n\t" + goal_line);
      }
      String actual_line = actual[i];
      if (!goal_line.equals(actual_line)) {
	String goals = "";
	String actuals = "";
	int low = Math.max(0, i-3);
	int high = Math.min(Math.min(i+3, actual.length-1), goal.length-1);
	for (int j = low; j <= high; j++) {
	  if (!goal[j].equals(actual[j])) {
	    goals += ">";
	    actuals += ">";
	  }
	  goals += "\t" + goal[j] + "\n";
	  actuals += "\t" + actual[j] + "\n";
	}
	fail("Diff error:\nDifferent output encountered.  Expected:\n" +
	     goals + "Received:\n" + actuals);
      }
    }
    if (actual.length > goal.length) {
      fail("Diff error:\nActual had extra lines, starting with:\n\t" + actual[goal.length]);
    }
  }
}










