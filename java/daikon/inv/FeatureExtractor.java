package daikon.inv;
/*********************************************
 * An invariant feature extractor.
 * This class creates a labeling of invariants.
 * That is, it extracts features from invariants and then
 * classifies the invariants as "good" or a "bad" based
 * on which of the two input files the invariant came from.
 * The output goes to file in one of the following formats:
 * SVM-Light, SVMfu, or C5 uses.
 *********************************************/

import java.lang.reflect.*;
import java.io.*;
import java.util.*;
import java.text.*;
import daikon.*;
import daikon.diff.*;
import utilMDE.*;

public final class FeatureExtractor {
  private FeatureExtractor() { throw new Error("do not instantiate"); }

  private static final String lineSep = Global.lineSep;

  // See end of file for static variable declaration

  // Main reads the input files, extracts features and then
  //   outputs the labeling in SVM-Light, SVMfu, or C5.0 format.
  //   Arguments:
  //   -u FileName:   an InvMap inv file with useful invariants
  //   -n FileName:   an InvMap inv file with nonuseful invariants
  //   -o FileName:   output file name *Required
  //   -t Type:       Type is one of {SVMlight, SVMfu, C5} *Required
  //   -s FileName:   name of output file for invariant descriptions
  //   -r repeats:    number of combinations of feature vectors
  //   -p             do not output if no positive feature vectors are present

  private static String USAGE =
    UtilMDE.joinLines(
        "Arguments:",
        "-u FileName:\tan invMap inv file with useful invariants",
        "-n FileName:\tan invMap inv file with nonuseful invariants",
        "-o FileName:\toutput file name *Required",
        "-t Type:\tType is one of {SVMlight, SVMfu, C5}",
        "-s FileName:\tname of output file for invariant descriptions",
        "[-r] repeats:\tnumber of combinations of feature vectors (DISABLED)",
        "[-p] \t\tdo not output if no positive feature vectors are present");


  public static void main(String[] args)
    throws IOException, ClassNotFoundException, IllegalAccessException,
           InvocationTargetException {
    try {
      mainHelper(args);
    } catch (Daikon.TerminationMessage e) {
      System.err.println(e.getMessage());
      System.exit(1);
    }
    // Any exception other than Daikon.TerminationMessage gets propagated.
    // This simplifies debugging by showing the stack trace.
  }

  /**
   * This does the work of main, but it never calls System.exit, so it
   * is appropriate to be called progrmmatically.
   * Termination of the program with a message to the user is indicated by
   * throwing Daikon.TerminationMessage.
   * @see #main(String[])
   * @see daikon.Daikon.TerminationMessage
   **/
  public static void mainHelper(final String[] args)
    throws IOException, ClassNotFoundException, IllegalAccessException,
           InvocationTargetException {
    // Main performs 3 steps:
    // 1)  make two vectors of invariants: useful and nonuseful
    // 2)  extract the features for useful and nonuseful
    // 3)  print in proper format the labeling and if asked the descriptions

    if (args.length == 0) {
      System.out.println(USAGE);
      throw new Daikon.TerminationMessage("No arguments found");
    }

    // First, parse the arguments
    ArrayList usefuls = new ArrayList();
    ArrayList nonusefuls = new ArrayList();
    String output_file = null;
    String output_words = null;
    String output_type = null;
    int repeats = 1;
    boolean positives = false;

    for (int i = 0; i < args.length; i+=2) {
      if (args[i].equals("-p")) {
        positives = true;
        i--;
      }
      else if (args[i].equals("-u"))
        usefuls.add(args[i+1]);
      else if (args[i].equals("-n"))
        nonusefuls.add(args[i+1]);
      else if (args[i].equals("-r"))
        repeats = Integer.parseInt(args[i+1]);
      else if (args[i].equals("-o")) {
        if (output_file == null)
          output_file = args[i+1];
        else
          throw new IOException("Invalid Argument List, repeated output file");
      }
      else if (args[i].equals("-s")) {
        if (output_words == null)
          output_words = args[i+1];
        else
          throw new IOException("Invalid Argument List, repeated " +
                                "output description file");
      }
      else if (args[i].equals("-t")) {
        if ((output_type == null) || (output_type.equals(args[i+1])))
          output_type = args[i+1];
        else
          throw new IOException("Invalid Argument List, repeated output type");
      }
      else
        throw new IOException("Invalid Argument List, {u,n,o,s,t}" + args[i]);
    }
    if (output_file == null)
      throw new IOException("Invalid Argumnent List, output file not specified");
    if (output_type == null)
      throw new IOException("Invalid Argumnent List, output type not specified");
    if (output_file.equals(output_words))
      throw new IOException("Invalid Argumnent List, output and description files " +
                            "cannot be the same");
    // Step 1
    ArrayList[] allInvariants = getSimpleUsefulAndNonuseful(usefuls, nonusefuls);
    ArrayList useful = allInvariants[0];
    ArrayList nonuseful = allInvariants[1];

    // Step 2
    // Extract the features of each invariant in useful and nonuseful
    // The invariants' feature vectors are kept in the same order
    // as the invariants in useful and nonuseful.
    // Then extract the descriptions of each invariant, also kept in the
    // same order
    // ########## Commented out in order to use reflect functions
    //    ArrayList usefulFeatures = getFeatures(useful);
    //    ArrayList nonusefulFeatures = getFeatures(nonuseful);
    ArrayList usefulStrings = getStrings(useful);
    ArrayList nonusefulStrings = getStrings(nonuseful);

    HashMap lookup = getFullMapping();
    ArrayList usefulFeatures = getReflectFeatures(useful, lookup);
    ArrayList nonusefulFeatures = getReflectFeatures(nonuseful, lookup);

    // Step 3
    // Output the labeling in desired format.
    // Also, if output_words is non-null, output the invariant
    // descriptions.

    if ((!positives) || (usefulFeatures.size() > 0)) {

      if (output_type.equals("SVMfu")) {
        File output = new File(output_file);
        printSVMfuOutput(usefulFeatures, nonusefulFeatures, output);
        if (output_words != null) {
          File words = new File(output_words);
          writeInvariantDescriptions(usefulStrings, nonusefulStrings, words);
        }
      }
      else if (output_type.equals("SVMlight")) {
        File output = new File(output_file + ".tmp");
        printSVMOutput(usefulFeatures, nonusefulFeatures,
                       usefulStrings, nonusefulStrings, output);
        compactSVMFeatureFile(output, new File(args[args.length-1]));
        output.delete();
      }
      else if (output_type.equals("C5")) {
        File output = new File(output_file + ".data");
        File names = new File(output_file + ".names");
        printC5Output(usefulFeatures, nonusefulFeatures, output, names,lookup);
      }
      else
        System.err.println("Invalid Output Type: " + output_type);
    }

  }

  // Takes a vector of invariants and returns a vector of
  // the string representations of those invariants in the same order
  private static ArrayList getStrings(ArrayList invs) {
    ArrayList answer = new ArrayList();
    for (int i = 0; i < invs.size(); i++) {
      Invariant current = (Invariant) invs.get(i);
      answer.add(current.ppt.parent.name + ":::" + current.format());
    }
    return answer;
  }

  // Takes two vectors of file names and loads the invariants
  //   in those files into two vectors.
  //   Returns the useful invariants in return[0],
  //   returns the nonuseful invariants in return[1];

  private static ArrayList[] getSimpleUsefulAndNonuseful(ArrayList usefuls,
                                                      ArrayList nonusefuls)
    throws IOException, ClassNotFoundException {

    // returns two ArrayLists (in an array) of Useful invariants and
    // Non-Useful invariants
    // return[0] are Useful
    // return[1] are Non-Useful

    ArrayList[] answer = new ArrayList[2];
    answer[0] = new ArrayList(); // useful
    answer[1] = new ArrayList(); // nonuseful
    for (int i = 0; i < usefuls.size(); i++)
      for (Iterator invs=readInvMap(new File((String) usefuls.get(i))).invariantIterator(); invs.hasNext(); )
        answer[0].add(invs.next());

    for (int i = 0; i < nonusefuls.size(); i++)
      for (Iterator invs=readInvMap(new File((String) nonusefuls.get(i))).invariantIterator(); invs.hasNext(); )
        answer[1].add(invs.next());

    return answer;
  }

  // Old version of loading invariants from a list of filenames.
  //   Compares invariants within the files to determine if they
  //   are useful or non-useful.

  private static ArrayList[] getUsefulAndNonuseful(String[] args)
    throws IOException {
    // ignore args[0] and args[length-1]
    // the rest of the args are pairs of files such each pair
    // consists of a Non-Buggy.inv and Buggy.inv
    // Note, Non-Buggy.inv contains invariants present in non-buggy code
    // and Buggy.inv contains invariants present in buggy code

    // returns two ArrayLists (in an array) of Useful invariants and
    // non-useful invariants
    ArrayList[] answer = new ArrayList[2];
    answer[0] = new ArrayList();
    answer[1] = new ArrayList();

    for (int i = 1; i < args.length-1; i+=2) {
      // good contains string reps of invariants in Non-Buggy.inv
      HashSet good = new HashSet();
      for (Iterator goodppts =
             FileIO.read_serialized_pptmap(new File(args[i]), false).pptIterator();
           goodppts.hasNext(); ) {
        List temp = ((PptTopLevel) goodppts.next()).getInvariants();
        for (int j = 0; j < temp.size(); j++)
          good.add(((Invariant) temp.get(j)).repr());
      }

      // bad contains actual invariants in Buggy.inv
      ArrayList bad = new ArrayList();
      for (Iterator badppts =
             FileIO.read_serialized_pptmap(new File(args[i+1]),false).pptIterator();
           badppts.hasNext(); ) {
        List temp = ((PptTopLevel) badppts.next()).getInvariants();
        for (int j = 0; j < temp.size(); j++)
          bad.add((Invariant) temp.get(j));
      }

      for (int j = 0; j < bad.size(); j++) {
        if (good.contains(((Invariant) bad.get(j)).repr()))
          answer[1].add(bad.get(j));
        else
          answer[0].add(bad.get(j));
      }
    }
    return answer;
  }

  // Prints the labeling using C5 format

  private static void printC5Output(ArrayList usefulFeatures,
                                    ArrayList nonusefulFeatures,
                                    File outputFile, File namesFile,
                                    HashMap lookup)
    throws IOException {
    PrintStream names = new PrintStream(namesFile);

    // First create a TreeSet of all the Feature Numbers and 0 as value
    // and a Map of numbers to names
    TreeSet allFeatures = new TreeSet();
    HashMap numbersToNames = new HashMap();
    for (Iterator<Map.Entry> i = lookup.entrySet().iterator(); i.hasNext();) {
      Map.Entry entry = i.next();
      Object key = entry.getKey();
      int num = ((Integer) entry.getValue()).intValue();
      IntDoublePair pair = new IntDoublePair(num, 0);
      allFeatures.add(pair);
      String name;
      if (key instanceof Class)
        name = ((Class) key).getName() + "Bool";
      else if (key instanceof String) {
        name = (String) key;
      }
      else
        throw new RuntimeException(key + " object cannot be converted to " +
                                   "a feature.");
      numbersToNames.put(pair, name);
    }

    //    System.out.println(numbersToNames.get(new IntDoublePair(2784, 0.0)));

    // Now make the .names part
    names.println("|Beginning of .names file");
    // names.println("GoodBad."); names.println(); names.println("GoodBad: 1, -1.");
    names.println("good, bad.");
    for (Iterator<IntDoublePair> all = allFeatures.iterator(); all.hasNext(); ) {
      IntDoublePair current = all.next();
      if (numbersToNames.containsKey(current)) {
        String currentName = (String) numbersToNames.get(current);
        if (currentName.endsWith("Bool"))
          names.println(currentName + ":0.0, 1.0.");
        else if (currentName.endsWith("Float"))
          names.println(currentName + ": continuous.");
        else if (currentName.endsWith("Int"))
          //          names.println(currentName + ": discrete.");
          names.println(currentName + ": continuous.");
        else throw new IOException("All feature names must end with one of " +
                                   "Float, Bool, or Int." + lineSep + "Error: " +
                                   currentName + lineSep);
      }
      else
        throw new IOException("Feature " + current.number +
                              " not included in .names file");
      //names.println(current.number + ": continuous.");
    }
    names.println("|End of .names file");
    names.close();

    PrintStream output = new PrintStream(new FileOutputStream(outputFile));
    // Now for each invariant, print out the features C5.0 style
    // first useful
    printC5DataOutput(usefulFeatures, allFeatures, "good", output);
    // and now non useful
    printC5DataOutput(nonusefulFeatures, allFeatures, "bad", output);
    output.close();
  }

  // Prints the partial labeling using C5 format for all feature vectors
  //   in features.

  private static void printC5DataOutput (ArrayList features,
                                         TreeSet allFeatures,
                                         String label,
                                         PrintStream output) throws IOException {
    DecimalFormat df = new DecimalFormat("0.0####");
    // Create a TreeSet allFets which has all the features of
    // the current (ith) vector and the other features filled in with 0s
    for (int i = 0; i < features.size(); i++) {
      TreeSet allFets = ((TreeSet) features.get(i));

      // Debugging code to detect duplicates within allFets
      //
      //  HashSet temp = new HashSet();
      //  for (Iterator<IntDoublePair> iter = allFets.iterator(); iter.hasNext();) {
      //  IntDoublePair meh = iter.next();
      //  if (temp.contains(new Integer(meh.number)))
      //  throw new RuntimeException(lineSep + "Found duplicate feature: "+meh.number);
      //  temp.add(new Integer(meh.number));
      //  }
      // End Debugging Code

      // check which features are missing and add IntDoublePairs
      // with those features set to 0
      for (Iterator<IntDoublePair> h = allFeatures.iterator(); h.hasNext();) {
        IntDoublePair current = h.next();
        boolean contains = false;
        for (Iterator<IntDoublePair> j = allFets.iterator(); j.hasNext();) {
          IntDoublePair jguy = j.next();
          if (jguy.number == current.number)
            contains = true;
        }
        if (!contains)
          allFets.add(current);
      }

      // Debug Code that prints out features that
      // have been forgotten in AllFeatures

//       for (Iterator<IntDoublePair> h = allFets.iterator(); h.hasNext();) {
//         IntDoublePair current = h.next();
//         boolean contains = false;

//         for (Iterator<IntDoublePair> j = allFeatures.iterator(); j.hasNext();) {
//           IntDoublePair jguy = j.next();
//           if (jguy.number == current.number)
//             contains = true;
//         }
//         if (!contains)
//           System.out.println(current.number);
//       } */// end debug code

      Assert.assertTrue(allFeatures.size() == allFets.size(),
                        lineSep + "Expected number of features: "+allFeatures.size() +
                        lineSep + "Actual number of features: "+allFets.size());

      for (Iterator<IntDoublePair> fets = allFets.iterator(); fets.hasNext(); ) {
        IntDoublePair fet = fets.next();
        output.print(df.format(fet.value) + ",");
      }
      output.println(label);
    }
  }

  // Prints the labeling using SVMlight format

  private static void printSVMOutput(ArrayList usefulFeatures,
                                     ArrayList nonusefulFeatures,
                                     ArrayList usefulStrings,
                                     ArrayList nonusefulStrings,
                                     File outputFile) throws IOException {
    PrintStream output = new PrintStream(new FileOutputStream(outputFile));
    // Now add all the features in SVM-Light format to output
    // first the useful
    printSVMDataOutput(usefulFeatures, usefulStrings, "+1 ", output);
    // and now non useful
    printSVMDataOutput(nonusefulFeatures, nonusefulStrings, "-1 ", output);
    output.close();
  }

  // Prints a partial labeling using SVMlight format for all the
  //   feature vectors in features.

  private static void printSVMDataOutput(ArrayList features, ArrayList strings,
                                         String label,
                                         PrintStream output) throws IOException {
    DecimalFormat df = new DecimalFormat("0.0####");
    for (int i = 0; i < features.size(); i++) {
      output.print(label);
      for (Iterator<IntDoublePair> fets = ((TreeSet) features.get(i)).iterator();
           fets.hasNext();) {
        IntDoublePair fet = fets.next();
        if (fet.value > THRESHOLD)
          output.print(fet.number + ":" + df.format(fet.value) + " ");
      }
      output.println();
      output.println("#  " + ((String) strings.get(i)));
    }
  }

  // Prints the labeling using SVMfu format.

  private static void printSVMfuOutput(ArrayList usefulFeatures,
                                       ArrayList nonusefulFeatures,
                                       File outputFile) throws IOException {
    PrintStream output = new PrintStream(new FileOutputStream(outputFile));
    // Now add all the features in SVMfu format to output
    // first size
    output.println((usefulFeatures.size() + nonusefulFeatures.size()));
    // first the useful
    printSVMfuDataOutput(usefulFeatures, "1 ", output);
    // and now non useful
    printSVMfuDataOutput(nonusefulFeatures, "-1 ", output);
    output.close();
  }

  // Prints a partial labeling using SVMfu format for all the
  // feature vectors in features.

  private static void printSVMfuDataOutput(ArrayList features, String label,
                                           PrintStream output) throws IOException {
    DecimalFormat df = new DecimalFormat("0.0####");
    for (int i = 0; i < features.size(); i++) {
      output.print(((TreeSet) features.get(i)).size() * 2 + " ");
      for (Iterator<IntDoublePair> fets = ((TreeSet) features.get(i)).iterator();
           fets.hasNext();) {
        IntDoublePair fet = fets.next();
        output.print(fet.number + " " + df.format(fet.value) + " ");
      }
      output.println(label);
    }
  }

  // Prints the invariant descriptions to a file.

  private static void writeInvariantDescriptions(ArrayList usefulStrings,
                                       ArrayList nonusefulStrings,
                                       File outputFile) throws IOException {
    PrintStream output = new PrintStream(new FileOutputStream(outputFile));
    for (int i = 0; i < usefulStrings.size(); i++)
      output.println((String) usefulStrings.get(i));
    for (int i = 0; i < nonusefulStrings.size(); i++)
      output.println((String) nonusefulStrings.get(i));
    output.close();
  }

  // compacts an SVMlight file to remove repeats.

  private static void compactSVMFeatureFile(File input, File output)
    throws IOException {
    BufferedReader br = new BufferedReader(new FileReader(input));
    HashSet vectors = new HashSet();
    ArrayList outputData = new ArrayList();
    while (br.ready()) {
      String line = br.readLine();
      if (vectors.contains(line))
        br.readLine();
      else {
        vectors.add(line);
        line += lineSep + br.readLine();
        outputData.add(line);
      }
    }
    br.close();

    PrintStream ps = new PrintStream(new FileOutputStream(output));
    for (Iterator i = outputData.iterator(); i.hasNext(); )
      ps.println((String) i.next());
    ps.close();
  }

  // compacts an SVMfu file to remove repeats.
  private static void compactSVMfuFeatureFile(File input, File output)
    throws IOException {
    BufferedReader br = new BufferedReader(new FileReader(input));
    HashSet vectors = new HashSet();
    br.readLine();
    while (br.ready())
      vectors.add(br.readLine());
    br.close();

    PrintStream ps = new PrintStream(new FileOutputStream(output));
    ps.println(vectors.size());
    for (Iterator i = vectors.iterator(); i.hasNext(); )
      ps.println((String) i.next());
    ps.close();
  }

  // Reads an InvMap from a file that contains a serialized InvMap.
  private static InvMap readInvMap(File file) throws
  IOException, ClassNotFoundException {
    Object o = UtilMDE.readObject(file);
    if (o instanceof InvMap) {
      return (InvMap) o;
    } else
      throw new ClassNotFoundException("inv file does not contain InvMap");
  }

  // Calculate a HashMap of every feature to a unique integer.
  private static HashMap getFullMapping() throws ClassNotFoundException {
    HashMap answer = new HashMap();
    Integer counter = new Integer(0);

    //get a set of all Invariant classes
    File top = new File(CLASSES);
    ArrayList classes = getInvariantClasses(top);

    for (int i = 0; i < classes.size(); i++) {
      Class currentClass = (Class) classes.get(i);
      Field[] fields = currentClass.getFields();
      Method[] methods = currentClass.getMethods();
      //handle the class
      counter = new Integer(counter.intValue() + 1);
      answer.put(currentClass, counter);

      if (VarInfo.class.isAssignableFrom(currentClass)) {
        for (int iC = 0; iC < NUM_VARS; iC++) {
          counter = new Integer(counter.intValue() + 1);
          answer.put("Var#"+iC+"_"+currentClass.getName()+"Bool", counter);
        } //reserve space for all the variables
      }

      //handle all the fields
      for (int j = 0; j < fields.length; j++) {
        if (answer.get(fields[j]) == null) {
          //          if ((Boolean.TYPE.equals(fields[j].getType())) ||
          //          (Number.class.isAssignableFrom(fields[j].getType()))) {
          if (TYPES.contains(fields[j].getType())) {
            String name = fields[j].getName();
            if (fields[j].getType().equals(Boolean.TYPE))
              name += "Bool";
            else
              name += "Float";

            counter = new Integer(counter.intValue() + 1);
            answer.put(name, counter);
            if (VarInfo.class.isAssignableFrom(currentClass))
              for (int iC = 0; iC < NUM_VARS; iC++) {
                counter = new Integer(counter.intValue() + 1);
                answer.put(iC + "_" + name, counter);
              }
          }
        }
      }

      //handle all the methods with 0 parameters
      for (int j = 0; j < methods.length; j++) {
        if ((answer.get(methods[j].getName()) == null) &&
            (methods[j].getParameterTypes().length == 0)) {
          //    if ((Boolean.TYPE.equals(methods[j].getReturnType())) ||
          //    (Number.class.isAssignableFrom(methods[j].getReturnType()))) {
          if (TYPES.contains(methods[j].getReturnType())) {
            String name = methods[j].getName();
            if (methods[j].getReturnType().equals(Boolean.TYPE))
              name += "Bool";
            else
              name += "Float";
            counter = new Integer(counter.intValue() + 1);
            answer.put(name, counter);
            if (VarInfo.class.isAssignableFrom(currentClass))
              for (int iC = 0; iC < NUM_VARS; iC++) {
                counter = new Integer(counter.intValue() + 1);
                answer.put(iC + "_" + name, counter);
              }
          }
        }
      }
    }
    return answer;
  }

  private static ArrayList getInvariantClasses(File top)
    throws ClassNotFoundException {
    ArrayList answer = new ArrayList();
    if (top.isDirectory()) {
      File[] all = top.listFiles();
      for (int i = 0; i < all.length; i++)
        if (!(all[i].getAbsolutePath().indexOf("test") > -1))
          answer.addAll(getInvariantClasses(all[i]));
    } else if (top.getName().endsWith(".class")) {
      String name = top.getAbsolutePath();
      name = name.substring(name.indexOf("daikon"), name.indexOf(".class"));
      name = name.replace('/', '.');

      // have to remove the .ver2 or .ver3 tags
      if (name.indexOf("ver2") > -1)
        name = name.substring(0, name.indexOf(".ver2")) +
          name.substring(name.indexOf(".ver2") + 5);
      if (name.indexOf("ver3") > -1)
        name = name.substring(0, name.indexOf(".ver3")) +
          name.substring(name.indexOf(".ver3") + 5);

      try {
        Class current = Class.forName(name);
        if ((Invariant.class.isAssignableFrom(current)) ||
            (Ppt.class.isAssignableFrom(current)) ||
            (VarInfo.class.isAssignableFrom(current))) {
          //          System.out.println("Class " + name + " loaded");
          answer.add(current);
        }
      }
      catch (ClassNotFoundException e) {}
      catch (NoClassDefFoundError e) {}

    }
    return answer;
  }

  // Call getAllReflectFeatures on every Invariants in invariants
  // to get all the features of that invariant
  // and store those featues in a new TreeSet.
  // return a ArrayList of TreeSets of features.
  private static ArrayList getReflectFeatures(ArrayList invariants, HashMap lookup)
    throws IllegalAccessException, InvocationTargetException {
    ArrayList answer = new ArrayList();
    // for each invariant, extract all the features and build a new TreeSet
    for (int i = 0; i < invariants.size(); i++) {
      answer.add(new TreeSet(getReflectFeatures(invariants.get(i), lookup)));
    }
    return answer;
  }

  // Extract the features of inv using reflection,
  // return a Collection of these features in IntDoublePairs
  private static TreeSet getReflectFeatures(Object inv, HashMap lookup)
    throws IllegalAccessException, InvocationTargetException {
    TreeSet answer = new TreeSet();
    if (inv instanceof Invariant) {
      if (lookup.get(inv.getClass()) == null)
        throw new NullPointerException("Missing " + inv.getClass().getName() +
                                       " class in the lookup Map");
      answer.add(new IntDoublePair(((Integer)
                                    lookup.get(inv.getClass())).intValue(),1));
      answer.addAll(getReflectFeatures(((Invariant)inv).ppt, lookup));
      answer.addAll(getReflectFeatures(((Invariant)inv).ppt.var_infos,lookup));
      VarInfo[] varInfos = ((Invariant) inv).ppt.var_infos;
      for (int i = 0; i < varInfos.length; i++) {

        for (Iterator<IntDoublePair> it = getReflectFeatures(varInfos[i],lookup).iterator();
             it.hasNext();) {
          IntDoublePair current = it.next();
          answer.add(new IntDoublePair(current.number + i + 1, current.value));
          answer.add(current);
        }
      }
    }

    Field[] fields = inv.getClass().getFields();

    for (int i = 0; i < fields.length; i++) {
      if (!BANNED_METHODS.contains(fields[i].getName()))
        if (fields[i].getType().equals(Boolean.TYPE))
          answer.add(new IntDoublePair(((Integer) lookup.get(fields[i].getName() + "Bool")).intValue(), 1));
        else if (TYPES.contains(fields[i].getType()))
          answer.add(new IntDoublePair(((Integer) lookup.get(fields[i].getName() + "Float")).intValue(), fields[i].getDouble(inv)));
    }

    Method[] methods = inv.getClass().getMethods();
    for (int i = 0; i < methods.length; i++) {
      if (methods[i].getParameterTypes().length == 0) {
        if (!BANNED_METHODS.contains(methods[i].getName()))
          if (methods[i].getReturnType().equals(Boolean.TYPE))
            answer.add(new IntDoublePair(((Integer) lookup.get(methods[i].getName() + "Bool")).intValue(), 1));
          else if (TYPES.contains(methods[i].getReturnType()))
            answer.add(new IntDoublePair(((Integer) lookup.get(methods[i].getName() + "Float")).intValue(),
                                         ((Number)
                                          methods[i].invoke(inv, new Object[0])
                                          ).doubleValue()));
      }
    }

    //cleanup answer
    TreeSet final_answer = new TreeSet();
    HashSet index = new HashSet();
    for (Iterator<IntDoublePair> iter = answer.iterator(); iter.hasNext();) {
      IntDoublePair current = iter.next();
      if (!(index.contains(new Integer(current.number))))
        final_answer.add(current);
      index.add(new Integer(current.number));
    }

    return final_answer;
  }

  /*********************************************
   * This IntDoublePair represents a connected int and double.
   * This is pretty much a struct + constructor.
   * However this also implements Comparable
   * so that it can be used in a TreeSet or Sorted.
   * When two IntDoublePairs are compared, they are compared
   * based on their integer only.  The smaller the interger -- the smaller
   * the IntDoublePair.  Two IntDoublePairs that have the same integer are
   * considered equal.
   *********************************************/
  private static final class IntDoublePair implements Comparable{
    // public fields
    public int number;
    public double value;

    // returns a new fresh Pair with number set to num and value set to val
    public IntDoublePair(int num, double val) {
      number = num;
      value = val;
    }

    public boolean equals(Object o) {
      if (o instanceof IntDoublePair) {
        IntDoublePair other = (IntDoublePair) o;
        return ((number == other.number) && (value == other.value));
      }
      else return false;
    }

    //returns a valid hashCode
    public int hashCode() {
      return number;
    }

    // Compares an Object to this
    // Throws ClassCastException if o is not an IntDoublePair
    public int compareTo(Object o) throws ClassCastException {
      IntDoublePair p;
      try {
        p = (IntDoublePair) o;
      }
      catch (ClassCastException e) {
        throw e;
      }
      if (this.number != p.number)
        return this.number - p.number;
      else
        return (int) (this.value - p.value);
    }
  }

  /*********************************************
   * A tool for combining and normalizing multiple SVMfu and C5 files.
   *********************************************/

  public static final class CombineFiles {

    private static String USAGE =
      UtilMDE.joinLines(
        "Arguments:",
        "-i FileName:\ta SVMfu or C5 input file (with .data)",
        "-t Type:\tFormat, one of C5 or SVMfu",
        "-o FileName:\toutput file name (with.data)",
        "[-n] repeat:\tif present then the number of positive and negative",
        "\tvectors will be roughtly normalized (by repeats).");

    public static void main(String[] args)
      throws IOException, ClassNotFoundException {
      try {
        mainHelper(args);
      } catch (Daikon.TerminationMessage e) {
        System.err.println(e.getMessage());
        System.exit(1);
      }
      // Any exception other than Daikon.TerminationMessage gets propagated.
      // This simplifies debugging by showing the stack trace.
    }

    /**
     * This does the work of main, but it never calls System.exit, so it
     * is appropriate to be called progrmmatically.
     * Termination of the program with a message to the user is indicated by
     * throwing Daikon.TerminationMessage.
     * @see #main(String[])
     * @see daikon.Daikon.TerminationMessage
     **/
    public static void mainHelper(final String[] args)
      throws IOException, ClassNotFoundException {

      // First parse the arguments
      if (args.length == 0) {
        System.out.println(USAGE);
        throw new Daikon.TerminationMessage("No arguments found");
      }
      ArrayList inputs = new ArrayList();
      boolean normalize = false;
      String output = null;
      String type = null;
      for (int i = 0; i < args.length; i++) {
        if (args[i].equals("-n"))
          normalize = true;
        else if (args[i].equals("-t"))
          type = args[++i];
        else if (args[i].equals("-i"))
          inputs.add(args[++i]);
        else if (args[i].equals("-o")) {
          if (output == null)
            output = args[++i];
          else
            throw new IOException("Multiple output files not allowed");
        }
        else
          throw new IOException("Invalid argument: " + args[i]);
      }
      // Check if the required fields are specified.
      if (type == null)
        throw new IOException("You must specify a format type (C5 or SVMfu)");
      if (output == null)
        throw new IOException("You must specify an output file");
      if (inputs.size() == 0)
        throw new IOException("You must specify at least one input file");

      // Load the input files into 2 HashSets, pos and neg.
      HashSet pos = new HashSet();
      HashSet neg = new HashSet();

      for (Iterator i = inputs.iterator(); i.hasNext(); ) {
        BufferedReader br=new BufferedReader(new FileReader((String)i.next()));
        br.readLine();
        while (br.ready()) {
          String vector = br.readLine();
          assert vector != null;

          if (type.equals("C5")) {
            if (vector.indexOf("bad") > -1)
              neg.add(vector.substring(0, vector.lastIndexOf("bad")));
            else
              pos.add(vector.substring(0, vector.lastIndexOf("good")));
          } else if (type.equals("SVMfu")) {
            int posind = vector.lastIndexOf("1");
            int negind = vector.lastIndexOf("-1");

            if (negind == posind - 1)
              neg.add(vector.substring(0, vector.lastIndexOf("-1")));
            else
              pos.add(vector.substring(0, vector.lastIndexOf("1")));
          }
        }
        br.close();
      }

      // Now create two vectors, posvectors and negvectors, of the
      // positive and negative vectors respectively.
      ArrayList posvectors = new ArrayList();
      ArrayList negvectors = new ArrayList();

      for (Iterator<String> i = neg.iterator(); i.hasNext(); ) {
        String vector = i.next();
        if (!(pos.contains(vector))) {
          if (type.equals("C5"))
            negvectors.add(vector + "bad");
          else if (type.equals("SVMfu"))
            negvectors.add(vector + "-1");
        }
      }

      for (Iterator i = pos.iterator(); i.hasNext(); )
        if (type.equals("C5"))
          posvectors.add(((String) i.next()) + "good");
        else if (type.equals("SVMfu"))
          posvectors.add(((String) i.next()) + "1");

      // Set the appropriate repeat values.
      int posrepeat = 1 , negrepeat = 1;
      if (normalize) {
        if (posvectors.size() == 0)
          throw new IOException("There are no positive vectors, " +
                                "cannot normalize");
        if (negvectors.size() == 0)
          throw new IOException("There are no negative vectors, " +
                                "cannot normalize");
        if (posvectors.size() > negvectors.size())
          negrepeat = posvectors.size() / negvectors.size();
        else
          posrepeat = negvectors.size() / posvectors.size();
      }

      // Print the output to the output file.
      PrintStream ps = new PrintStream(new FileOutputStream(output));
      // first calculate size and write the header for SVMfu
      int size = negrepeat * negvectors.size() + posrepeat * posvectors.size();
      if (type.equals("SVMfu"))
        ps.println(size);
      // now write the data
      for (int repeat = 0; repeat < negrepeat; repeat++)
        for (Iterator i = negvectors.iterator(); i.hasNext(); )
          ps.println((String) i.next() + " ");
      for (int repeat = 0; repeat < posrepeat; repeat++)
        for (Iterator i = posvectors.iterator(); i.hasNext(); )
          ps.println((String) i.next() + " ");
      ps.close();

      // Print a summary of positives and negatives to stdout.
      System.out.println(posvectors.size() + "*" + posrepeat + " " +
                         negvectors.size() + "*" + negrepeat);
    }
  }

  /*********************************************
   * A tool for classifying SVMfu and C5 files.
   *********************************************/

  public static final class ClassifyInvariants {

    private static String USAGE =
    UtilMDE.joinLines(
        "Arguments:",
        "-d FileName:\tSVMfu or C5 training data (with .data)",
        "-s FileName:\tSVMfu or C5 test data (with .data)",
        "-t Type:\tFormat, one of C5 or SVMfu");

    public static void main(String[] args)
      throws IOException, ClassNotFoundException {

      // First parse the arguments
      if (args.length == 0) {
        System.out.println(USAGE);
        throw new Daikon.TerminationMessage("No arguments found");
      }
      ArrayList trains = new ArrayList();
      ArrayList tests = new ArrayList();
      String type = null;
      for (int i = 0; i < args.length; i++) {
        if (args[i].equals("-t"))
          type = args[++i];
        else if (args[i].equals("-d"))
          trains.add(args[++i]);
        else if (args[i].equals("-s"))
          tests.add(args[++i]);
        else
          throw new IOException("Invalid argument: " + args[i]);
      }
      // Check if the required fields are specified.
      if (type == null)
        throw new IOException("You must specify a format type (C5 or SVMfu)");
      if (tests.size() == 0)
        throw new IOException("You must specify at least one test data file");
      if (trains.size() == 0)
        throw new IOException("You must specify at least one train data file");

      // Load the train files into 2 HashSets, pos and neg.
      HashSet pos = new HashSet();
      HashSet neg = new HashSet();

      for (Iterator i = trains.iterator(); i.hasNext(); ) {
        BufferedReader br=new BufferedReader(new FileReader((String)i.next()));
        br.readLine();
        while (br.ready()) {
          String vector = br.readLine();
          assert vector != null;

          if (type.equals("C5")) {
            if (vector.indexOf("bad") > -1)
              neg.add(vector.substring(0, vector.lastIndexOf("bad")));
            else
              pos.add(vector.substring(0, vector.lastIndexOf("good")));
          } else if (type.equals("SVMfu")) {
            int posind = vector.lastIndexOf("1");
            int negind = vector.lastIndexOf("-1");

            if (negind == posind - 1)
              neg.add(vector.substring(0, vector.lastIndexOf("-1")));
            else
              pos.add(vector.substring(0, vector.lastIndexOf("1")));
          }
        }
        br.close();
      }

      // Load the test files into two vectors: testBad and testGood
      ArrayList testGood = new ArrayList();
      ArrayList testBad = new ArrayList();

      for (Iterator i = trains.iterator(); i.hasNext(); ) {
        BufferedReader br=new BufferedReader(new FileReader((String)i.next()));
        br.readLine();
        while (br.ready()) {
          String vector = br.readLine();
          assert vector != null;

          if (type.equals("C5")) {
            if (vector.indexOf("bad") > -1)
              testBad.add(vector.substring(0, vector.lastIndexOf("bad")));
            else
              testGood.add(vector.substring(0, vector.lastIndexOf("good")));
          } else if (type.equals("SVMfu")) {
            int posind = vector.lastIndexOf("1");
            int negind = vector.lastIndexOf("-1");

            if (negind == posind - 1)
              testBad.add(vector.substring(0, vector.lastIndexOf("-1")));
            else
              testGood.add(vector.substring(0, vector.lastIndexOf("1")));
          }
        }
        br.close();
      }
    }


  //   for (Iterator i = pos.iterator(); i.hasNext(); )
//       if (type.equals("C5"))
//         posvectors.add(((String) i.next()) + "good");
//     else if (type.equals("SVMfu"))
//       posvectors.add(((String) i.next()) + "1");

//     // Print the output to the output file.
//     PrintStream ps = new PrintStream(new FileOutputStream(output));
//     for (int repeat = 0; repeat < negrepeat; repeat++)
//       for (Iterator i = negvectors.iterator(); i.hasNext(); )
//         ps.println((String) i.next() + " ");
//     for (int repeat = 0; repeat < posrepeat; repeat++)
//       for (Iterator i = posvectors.iterator(); i.hasNext(); )
//         ps.println((String) i.next() + " ");
//     ps.close();

//     // Print a summary of positives and negatives to stdout.
//     System.out.println(posvectors.size() + "*" + posrepeat + " " +
//                        negvectors.size() + "*" + negrepeat);

  }

  private static void writeArrayLists(ArrayList one, ArrayList two,
                                     String label, PrintStream ps)
    throws IOException {

    for (int i = 0; i < one.size(); i++)
      for (int j = 0; j < two.size(); j++) {
        String first = (String) one.get(i);
        String second = (String) two.get(j);
        String answer = first.substring(first.indexOf(" ") + 1) +
          shift(second);
        answer = (new StringTokenizer(answer)).countTokens() + " " + answer;
        ps.println(answer + label);
      }
  }

  private static String shift(String vector) {
    StringBuffer answer = new StringBuffer();
    StringTokenizer tokens = new StringTokenizer(vector);
    tokens.nextToken();
    while (tokens.hasMoreTokens()) {
      answer.append((Integer.parseInt(tokens.nextToken()) +
                     oneMoreOrderThanLargestFeature));
      answer.append(" ");
      answer.append(tokens.nextToken());
      answer.append(" ");
    }
    return answer.toString();
  }


  // the following line gets rid of some extra output that
  // otherwise gets dumped to System.out:
  static {
    LogHelper.setupLogs(false ? LogHelper.FINE : LogHelper.INFO);
  }

  // the THRESHOLD is zero
  static double THRESHOLD = 0.0;

  public static int oneMoreOrderThanLargestFeature = 100000;

  public static HashSet TYPES = new HashSet();
  public static HashSet BANNED_METHODS = new HashSet();
  public static String CLASSES =
    "/PAG/g5/users/brun/research/invariants/daikon.ver3";
  public static int NUM_VARS = 8;

  static {
    TYPES.add(Boolean.TYPE);
    TYPES.add(Integer.TYPE);
    TYPES.add(Double.TYPE);
    TYPES.add(Long.TYPE);
    TYPES.add(Short.TYPE);
    TYPES.add(Float.TYPE);

    BANNED_METHODS.add("hashCode");
    BANNED_METHODS.add("min_elt");
    BANNED_METHODS.add("max_elt");
  }

}
