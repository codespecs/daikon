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
import daikon.inv.*;
import daikon.inv.unary.*;
import daikon.inv.unary.scalar.*;
import daikon.inv.unary.sequence.*;
import daikon.inv.unary.string.*;
import daikon.inv.unary.stringsequence.*;
import daikon.inv.binary.sequenceScalar.*;
import daikon.inv.binary.sequenceString.*;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.binary.twoSequence.*;
import daikon.inv.binary.twoString.*;
import daikon.inv.ternary.threeScalar.*;
import utilMDE.*;

public final class FeatureExtractor {
  // See end of file for static variable declaration

  /* Main reads the input files, extracts features and then
     outputs the labeling in SVM-Light, SVMfu, or C5.0 format.
     Arguments:
     -u FileName:   an invMap inv file with useful invariants
     -n FileName:   an invMap inv file with nonuseful invariants
     -o FileName:   output file name *Required
     -t Type:       Type is one of {SVMlight, SVMfu, C5} *Required
     -s FileName:   name of output file for invariant descriptions
     -r repeats:    number of combinations of feature vectors
     -p             do not output if no positive feature vectors are present
  */

  private static String USAGE =
"\tArguments:\n\t-u FileName:\tan invMap inv file with useful invariants\n" +
    "\t-n FileName:\tan invMap inv file with nonuseful invariants\n" +
    "\t-o FileName:\toutput file name *Required\n" +
    "\t-t Type:\tType is one of {SVMlight, SVMfu, C5}\n" +
    "\t-s FileName:\tname of output file for invariant descriptions\n" +
    "\t[-r] repeats:\tnumber of combinations of feature vectors (DISABLED)\n" +
    "\t[-p] \t\tdo not output if no positive feature vectors are present\n";

  static public void main(String[] args)
    throws IOException, ClassNotFoundException {
    // Main performs 3 steps:
    // 1)  make two vectors of invariants: useful and nonuseful
    // 2)  extract the features for useful and nonuseful
    // 3)  print in proper format the labeling and if asked the descriptions

    if (args.length == 0) {
      System.out.print(USAGE);
      System.exit(0);
    }

    // First, parse the arguments
    Vector usefuls = new Vector();
    Vector nonusefuls = new Vector();
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
    Vector[] allInvariants = getSimpleUsefulAndNonuseful(usefuls, nonusefuls);
    Vector useful = allInvariants[0];
    Vector nonuseful = allInvariants[1];

    // Step 2
    // Extract the features of each invariant in useful and nonuseful
    // The invariants' feature vectors are kept in the same order
    // as the invariants in useful and nonuseful.
    // Then extract the descriptions of each invariant, also kept in the
    // same order
    Vector usefulFeatures = getFeatures(useful);
    Vector nonusefulFeatures = getFeatures(nonuseful);
    Vector usefulStrings = getStrings(useful);
    Vector nonusefulStrings = getStrings(nonuseful);
    // and create the proper number of repeats;

    /* DISABLED FEATURE
       while (repeats >= 2) {
         Vector[] placeholder = createPermutations(usefulFeatures,
         nonusefulFeatures,
         usefulStrings,
         nonusefulStrings);
         usefulFeatures = placeholder[0];
         nonusefulFeatures = placeholder[1];
         usefulStrings = placeholder[2];
         nonusefulStrings = placeholder[3];
         repeats /= 2;
    }
    END DISABLED FEATURE */

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
        printC5Output(usefulFeatures, nonusefulFeatures, output, names);
      }
      else
        System.err.println("Invalid Output Type: " + output_type);
    }

  }


  /* permutes the feature vectors repeats times
     returns a Vector array of size 4 such that
     return[0] is usefulFeatures
     return[1] is nonusefulFeatures,
     return[2] is usefulStrings,
     return[3] is nonusefulStrings
   */
  private static Vector[] createPermutations(Vector usefulFeatures,
                                       Vector nonusefulFeatures,
                                       Vector usefulStrings,
                                       Vector nonusefulStrings) {
    Vector[] answer = new Vector[4];
    for (int i = 0; i < 4; i++)
      answer[i] = new Vector();

    Vector[] placeholder;
    // first useful-useful
    placeholder = permute(usefulFeatures, usefulFeatures,
                          usefulStrings, usefulStrings);
    answer[0].addAll(placeholder[0]);
    answer[2].addAll(placeholder[1]);
    // second useful-nonuseful
    placeholder = permute(usefulFeatures, nonusefulFeatures,
                          usefulStrings, nonusefulStrings);
    answer[0].addAll(placeholder[0]);
    answer[2].addAll(placeholder[1]);
    // third nonuseful-useful
    placeholder = permute(nonusefulFeatures, usefulFeatures,
                          nonusefulStrings, usefulStrings);
    answer[0].addAll(placeholder[0]);
    answer[2].addAll(placeholder[1]);
    // last nonuseful-nonuseful
    placeholder = permute(nonusefulFeatures, nonusefulFeatures,
                          nonusefulStrings, nonusefulStrings);
    answer[1].addAll(placeholder[0]);
    answer[3].addAll(placeholder[1]);

    return answer;
  }

  /* returns two Vectors.  return[0] contain items from oneInput
     permuted with each item from twoInput.
     return[1] contains the permuted strings.
  */
  private static Vector[] permute(Vector oneInput, Vector twoInput,
                         Vector oneString, Vector twoString) {
    Vector[] answer = new Vector[2];
    answer[0] = new Vector();
    answer[1] = new Vector();

    for (int i = 0; i < oneInput.size(); i++)
      for (int j = 0; j < twoInput.size(); j++) {
        // Create a new TreeSet that is the union of oneInput.get(i)
        // and twoInput.get(j) shifted by OneMoreOrderThanLargestFeature.
        TreeSet current = new TreeSet();
        current.addAll((TreeSet) oneInput.get(i));
        current.addAll(shift(((TreeSet) twoInput.get(j)).iterator(),
                             OneMoreOrderThanLargestFeature));
        answer[0].add(current);

        // And now strings:
        answer[1].add(oneString.get(i) + "\t" + twoString.get(j));

      }
    return answer;
  }

  /* Adds shift to the int term of every IntDoublePair in the Iterator input
   */
  private static TreeSet shift(Iterator input, int shift) {
    TreeSet answer = new TreeSet();
    for (; input.hasNext(); ) {
      IntDoublePair current = (IntDoublePair) (input.next());
      answer.add(new IntDoublePair(current.number + shift, current.value));
    }
    return answer;
  }

  /* Takes a vector of invariants and returns a vector of
     the string representations of those invariants in the same order
  */
  private static Vector getStrings(Vector invs) {
    Vector answer = new Vector();
    for (int i = 0; i < invs.size(); i++) {
      Invariant current = (Invariant) invs.get(i);
      answer.add(current.ppt.parent.name + ":::" + current.format());
    }
    return answer;
  }

  /* Takes two vectors of file names and loads the invariants
     in those files into two vectors.
     Returns the useful invariants in return[0],
     returns the nonuseful invariants in return[1];
  */
  private static Vector[] getSimpleUsefulAndNonuseful(Vector usefuls,
                                                      Vector nonusefuls)
    throws IOException, ClassNotFoundException {

    // returns two Vectors (in an array) of Useful invariants and
    // Non-Useful invariants
    // return[0] are Useful
    // return[1] are Non-Useful

    Vector[] answer = new Vector[2];
    answer[0] = new Vector(); // useful
    answer[1] = new Vector(); // nonuseful
    for (int i = 0; i < usefuls.size(); i++)
      for (Iterator invs=readInvMap(new File((String) usefuls.get(i))).invariantIterator(); invs.hasNext(); )
        answer[0].add(invs.next());

    for (int i = 0; i < nonusefuls.size(); i++)
      for (Iterator invs=readInvMap(new File((String) nonusefuls.get(i))).invariantIterator(); invs.hasNext(); )
        answer[1].add(invs.next());

    return answer;
  }

  /* Old version of loading invariants from a list of filenames.
     Compares invariants within the files to determine if they
     are useful or non-useful.
  */
  private static Vector[] getUsefulAndNonuseful(String[] args)
    throws IOException{
    // ignore args[0] and args[length-1]
    // the rest of the args are pairs of files such each pair
    // consists of a Non-Buggy.inv and Buggy.inv
    // Note, Non-Buggy.inv contains invariants present in non-buggy code
    // and Buggy.inv contains invariants present in buggy code

    // returns two Vectors (in an array) of Useful invariants and
    // non-useful invariants
    Vector[] answer = new Vector[2];
    answer[0] = new Vector();
    answer[1] = new Vector();

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
      Vector bad = new Vector();
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

  /* Prints the labeling using C5 format
   */
  private static void printC5Output(Vector usefulFeatures,
                                    Vector nonusefulFeatures,
                                    File outputFile,File namesFile)
    throws IOException {
    FileWriter names = new FileWriter(namesFile);

    // First create a TreeSet of all the Feature Numbers and 0 as value
    TreeSet allFeatures =  new TreeSet();
    HashMap numbersToNames = new HashMap();
    Field[] fields = FeatureExtractor.class.getFields();
    FeatureExtractor useless = new FeatureExtractor();
    for (int i = 0; i < fields.length; i++) {
      // if this is a Variable Feature
      // we have to replicate it for each variable
      if (fields[i].getName().startsWith("FetVar") ||
          fields[i].getName().startsWith("FetType")) {
        int val;
        try { val = fields[i].getInt(useless); }
        catch (IllegalAccessException e) {
          throw new IOException("all fields starting with Fet must be of " +
                                "type int\n" + e.getMessage()); }
        for (int j = 0; j <= MaxNumVars; j++) {
          IntDoublePair temp = new IntDoublePair(j * 10000 + val, 0);
          allFeatures.add(temp);
          if (j == 0)
            numbersToNames.put(temp, "union--" + fields[i].getName());
          else
            numbersToNames.put(temp, "--" + j + fields[i].getName());
        }
        // else if this is a not a Variable Feature but still a Feature
      } else if (fields[i].getName().startsWith("Fet")) {
        try {
          IntDoublePair temp = new IntDoublePair(fields[i].getInt(useless), 0);
          allFeatures.add(temp);
          numbersToNames.put(temp, fields[i].getName());
        }
        catch (IllegalAccessException e) {
          throw new IOException("All fields starting with Fet must be of " +
                                "type int\n" + e.getMessage()); }


      }
    }
    // Now make the .names part
    names.write("|Beginning of .names file\n");
    // names.write("GoodBad.\n\nGoodBad: 1, -1.\n");
    names.write("good, bad.\n");
    for (Iterator all = allFeatures.iterator(); all.hasNext(); ) {
      IntDoublePair current = (IntDoublePair) all.next();
      if (numbersToNames.containsKey(current)) {
        String currentName = (String) numbersToNames.get(current);
        if (currentName.endsWith("Bool"))
          names.write(currentName + ":0, 1.\n");
        else if (currentName.endsWith("Float"))
          names.write(currentName + ": continuous.\n");
        else if (currentName.endsWith("Int"))
          names.write(currentName + ": discrete.\n");
        else throw new IOException("All feature names must end with one of " +
                                   "Float, Bool, or Int.\nError: " +
                                   currentName + "\n");
      }
      else
        throw new IOException("Feature " + current.number +
                              " not included in .names file");
      //names.write(current.number + ": continuous.\n");
    }
    names.write("|End of .names file\n");
    names.close();

    FileWriter output = new FileWriter(outputFile);
    // Now for each invariant, print out the features C5.0 style
    // first useful
    printC5DataOutput(usefulFeatures, allFeatures, "good", output);
    // and now non useful
    printC5DataOutput(nonusefulFeatures, allFeatures, "bad", output);
    output.close();
  }

  /* Prints the partial labeling using C5 format for all feature vectors
     in features.
   */
  private static void printC5DataOutput (Vector features,
                                         TreeSet allFeatures,
                                         String label,
                                         FileWriter output) throws IOException{
    DecimalFormat df = new DecimalFormat("0.0####");
    // Create a TreeSet allFets which has all the features of
    // the current (ith) vector and the other features filled in with 0s
    for (int i = 0; i < features.size(); i++) {
      TreeSet allFets = ((TreeSet) features.get(i));

      // check which features are missing and add IntDoublePairs
      // with those features set to 0
      for (Iterator h = allFeatures.iterator(); h.hasNext();) {
        IntDoublePair current = (IntDoublePair) h.next();
        boolean contains = false;
        for (Iterator j = allFets.iterator(); j.hasNext();) {
          IntDoublePair jguy = (IntDoublePair) j.next();
          if (jguy.number == current.number)
            contains = true;
        }
        if (!contains)
          allFets.add(current);
      }

      // Debug Code that prints out features that
      // have been forgotten in AllFeatures
      /*      for (Iterator h = allFets.iterator(); h.hasNext();) {
              IntDoublePair current = (IntDoublePair) h.next();
              boolean contains = false;
              for (Iterator j = allFeatures.iterator(); j.hasNext();) {
              IntDoublePair jguy = (IntDoublePair) j.next();
              if (jguy.number == current.number)
              contains = true;
              }
              if (!contains)
              System.out.println(current.number);
              }
      */

      for (Iterator fets = allFets.iterator(); fets.hasNext(); ) {
        IntDoublePair fet = (IntDoublePair) fets.next();
        output.write(df.format(fet.value) + ",");
      }
      output.write(label + "\n");
    }
  }

  /* Prints the labeling using SVMlight format
   */
  private static void printSVMOutput(Vector usefulFeatures,
                                     Vector nonusefulFeatures,
                                     Vector usefulStrings,
                                     Vector nonusefulStrings,
                                     File outputFile) throws IOException {
    FileWriter output = new FileWriter(outputFile);
    // Now add all the features in SVM-Light format to output
    // first the useful
    printSVMDataOutput(usefulFeatures, usefulStrings, "+1 ", output);
    // and now non useful
    printSVMDataOutput(nonusefulFeatures, nonusefulStrings, "-1 ", output);
    output.close();
  }

  /* Prints a partial labeling using SVMlight format for all the
     feature vectors in features.
   */
  private static void printSVMDataOutput(Vector features, Vector strings,
                                         String label,
                                         FileWriter output) throws IOException{
    DecimalFormat df = new DecimalFormat("0.0####");
    for (int i = 0; i < features.size(); i++) {
      output.write(label);
      for (Iterator fets = ((TreeSet) features.get(i)).iterator();
           fets.hasNext();) {
        IntDoublePair fet = (IntDoublePair) fets.next();
        if (fet.value > THRESHOLD)
          output.write(fet.number + ":" + df.format(fet.value) + " ");
      }
      output.write("\n");
      output.write("#  " + ((String) strings.get(i)) + "\n");
    }
  }

  /* Prints the labeling using SVMfu format.
   */
  private static void printSVMfuOutput(Vector usefulFeatures,
                                       Vector nonusefulFeatures,
                                       File outputFile) throws IOException {
    FileWriter output = new FileWriter(outputFile);
    // Now add all the features in SVMfu format to output
    // first size
    output.write((usefulFeatures.size() + nonusefulFeatures.size()) + "\n");
    // first the useful
    printSVMfuDataOutput(usefulFeatures, "1 ", output);
    // and now non useful
    printSVMfuDataOutput(nonusefulFeatures, "-1 ", output);
    output.close();
  }

  /* Prints a partial labeling using SVMfu format for all the
     feature vectors in features.
   */
  private static void printSVMfuDataOutput(Vector features, String label,
                                           FileWriter output) throws IOException{
    DecimalFormat df = new DecimalFormat("0.0####");
    for (int i = 0; i < features.size(); i++) {
      output.write(((TreeSet) features.get(i)).size() * 2 + " ");
      for (Iterator fets = ((TreeSet) features.get(i)).iterator();
           fets.hasNext();) {
        IntDoublePair fet = (IntDoublePair) fets.next();
        output.write(fet.number + " " + df.format(fet.value) + " ");
      }
      output.write(label);
      output.write("\n");
    }
  }

  /* Prints the invariant descriptions to a file.
   */
  private static void writeInvariantDescriptions(Vector usefulStrings,
                                       Vector nonusefulStrings,
                                       File outputFile) throws IOException {
    FileWriter output = new FileWriter(outputFile);
    for (int i = 0; i < usefulStrings.size(); i++)
      output.write((String) usefulStrings.get(i) + "\n");
    for (int i = 0; i < nonusefulStrings.size(); i++)
      output.write((String) nonusefulStrings.get(i) + "\n");
    output.close();
  }

  /* compacts an SVMlight file to remove repeats.
   */
  private static void compactSVMFeatureFile(File input, File output)
    throws IOException {
    BufferedReader br = new BufferedReader(new FileReader(input));
    HashSet vectors = new HashSet();
    Vector outputData = new Vector();
    while (br.ready()) {
      String line = br.readLine();
      if (vectors.contains(line))
        br.readLine();
      else {
        vectors.add(line);
        line += "\n" + br.readLine();
        outputData.add(line);
      }
    }
    br.close();

    FileWriter fw = new FileWriter(output);
    for (Iterator i = outputData.iterator(); i.hasNext(); )
      fw.write((String) i.next() + "\n");
    fw.close();
  }

  /* compacts an SVMfu file to remove repeats.
   */
  private static void compactSVMfuFeatureFile(File input, File output)
    throws IOException {
    BufferedReader br = new BufferedReader(new FileReader(input));
    HashSet vectors = new HashSet();
    br.readLine();
    while (br.ready())
      vectors.add(br.readLine());
    br.close();

    FileWriter fw = new FileWriter(output);
    fw.write(vectors.size() + "\n");
    for (Iterator i = vectors.iterator(); i.hasNext(); )
      fw.write((String) i.next() + "\n");
    fw.close();
  }

  /* Reads an InvMap from a file that contains a serialized InvMap.
   */
  private static InvMap readInvMap(File file) throws
  IOException, ClassNotFoundException {
    Object o = UtilMDE.readObject(file);
    if (o instanceof InvMap) {
      return (InvMap) o;
    } else
      throw new ClassNotFoundException("inv file does not contain InvMap");
  }

  /* Extracts features for each of the elements on invariants
     and returns a Vector of TreeSets of the features.
  */
  private static Vector getFeatures(Vector invariants) {
    Vector answer = new Vector();
    // for each invariant, extract all the features and build a new TreeSet
    for (int i = 0; i < invariants.size(); i++) {
      // extract the common features
      // then test for all other possible features
      TreeSet invariant = new TreeSet();

      Invariant current = (Invariant) invariants.get(i);

      invariant.addAll(getCommonFeatures(current));
      invariant.addAll(getVarFeatures(current.ppt.var_infos));
      if (current instanceof Modulus)
        invariant.addAll(getModulusFeatures((Modulus) current));
      if (current instanceof LowerBound)
        invariant.addAll(getLowerBoundFeatures((LowerBound) current));
      if (current instanceof NonZero)
        invariant.addAll(getNonZeroFeatures((NonZero) current));
      if (current instanceof NonModulus)
        invariant.addAll(getNonModulusFeatures((NonModulus) current));
      if (current instanceof OneOfScalar)
        invariant.addAll(getOneOfScalarFeatures((OneOfScalar) current));
      if (current instanceof Positive)
        invariant.addAll(getPositiveFeatures((Positive) current));
      if (current instanceof SingleFloat)
        invariant.addAll(getSingleFloatFeatures((SingleFloat) current));
      if (current instanceof SingleScalar)
        invariant.addAll(getSingleScalarFeatures((SingleScalar) current));
      if (current instanceof UpperBound)
        invariant.addAll(getUpperBoundFeatures((UpperBound) current));

      if (current instanceof EltLowerBound)
        invariant.addAll(getEltLowerBoundFeatures((EltLowerBound) current));
      if (current instanceof EltNonZero)
        invariant.addAll(getEltNonZeroFeatures((EltNonZero) current));
      if (current instanceof EltOneOf)
        invariant.addAll(getEltOneOfFeatures((EltOneOf) current));
      if (current instanceof EltUpperBound)
        invariant.addAll(getEltUpperBoundFeatures((EltUpperBound) current));
      if (current instanceof EltwiseIntComparison)
        invariant.addAll(getEltwiseIntComparisonFeatures((EltwiseIntComparison) current));
      if (current instanceof NoDuplicates)
        invariant.addAll(getNoDuplicatesFeatures((NoDuplicates) current));
      if (current instanceof OneOfSequence)
        invariant.addAll(getOneOfSequenceFeatures((OneOfSequence) current));
      if (current instanceof SeqIndexComparison)
        invariant.addAll(getSeqIndexComparisonFeatures((SeqIndexComparison) current));
      if (current instanceof SeqIndexNonEqual)
        invariant.addAll(getSeqIndexNonEqualFeatures((SeqIndexNonEqual) current));
      if (current instanceof SingleFloatSequence)
        invariant.addAll(getSingleFloatSequenceFeatures((SingleFloatSequence) current));

      if (current instanceof OneOfString)
        invariant.addAll(getOneOfStringFeatures((OneOfString) current));
      if (current instanceof SingleString)
        invariant.addAll(getSingleStringFeatures((SingleString) current));

      if (current instanceof EltOneOfString)
        invariant.addAll(getEltOneOfStringFeatures((EltOneOfString) current));
      if (current instanceof OneOfStringSequence)
        invariant.addAll(getOneOfStringSequenceFeatures((OneOfStringSequence) current));
      if (current instanceof SingleStringSequence)
        invariant.addAll(getSingleStringSequenceFeatures((SingleStringSequence) current));
      if (current instanceof OneOf)
        invariant.addAll(getOneOfFeatures((OneOf) current));
      if (current instanceof Comparison)
        invariant.addAll(getComparisonFeatures((Comparison) current));
      if (current instanceof Implication)
        invariant.addAll(getImplicationFeatures((Implication) current));

      if (current instanceof SeqIntComparison)
        invariant.addAll(getSeqIntComparisonFeatures((SeqIntComparison) current));
      if (current instanceof SequenceScalar)
        invariant.addAll(getSequenceScalarFeatures((SequenceScalar) current));

      if (current instanceof SequenceString)
        invariant.addAll(getSequenceStringFeatures((SequenceString) current));
      if (current instanceof IntNonEqual)
        invariant.addAll(getIntNonEqualFeatures((IntNonEqual) current));
      if (current instanceof IntEqual)
        invariant.addAll(getIntEqualFeatures((IntEqual) current));
      if (current instanceof FunctionUnary)
        invariant.addAll(getFunctionUnaryFeatures((FunctionUnary) current));
      if (current instanceof IntGreaterEqual)
        invariant.addAll(getIntGreaterEqualFeatures((IntGreaterEqual) current));
      if (current instanceof IntGreaterThan)
        invariant.addAll(getIntGreaterThanFeatures((IntGreaterThan) current));
      if (current instanceof LinearBinary)
        invariant.addAll(getLinearBinaryFeatures((LinearBinary) current));
      if (current instanceof TwoScalar)
        invariant.addAll(getTwoScalarFeatures((TwoScalar) current));
      if (current instanceof IntLessEqual)
        invariant.addAll(getIntLessEqualFeatures((IntLessEqual) current));
      if (current instanceof IntLessThan)
        invariant.addAll(getIntLessThanFeatures((IntLessThan) current));

      if (current instanceof PairwiseIntComparison)
        invariant.addAll(getPairwiseIntComparisonFeatures((PairwiseIntComparison) current));
      if (current instanceof SeqComparison)
        invariant.addAll(getSeqComparisonFeatures((SeqComparison) current));
      if (current instanceof TwoSequence)
        invariant.addAll(getTwoSequenceFeatures((TwoSequence) current));
      if (current instanceof PairwiseLinearBinary)
        invariant.addAll(getPairwiseLinearBinaryFeatures((PairwiseLinearBinary) current));
      if (current instanceof SubSequence)
        invariant.addAll(getSubSequenceFeatures((SubSequence) current));
      if (current instanceof PairwiseFunctionUnary)
        invariant.addAll(getPairwiseFunctionUnaryFeatures((PairwiseFunctionUnary) current));
      if (current instanceof Reverse)
        invariant.addAll(getReverseFeatures((Reverse) current));

      if (current instanceof StringComparison)
        invariant.addAll(getStringComparisonFeatures((StringComparison) current));
      if (current instanceof TwoString)
        invariant.addAll(getTwoStringFeatures((TwoString) current));

      if (current instanceof ThreeScalar)
        invariant.addAll(getThreeScalarFeatures((ThreeScalar) current));
      if (current instanceof LinearTernary)
        invariant.addAll(getLinearTernaryFeatures((LinearTernary) current));
      if (current instanceof FunctionBinary)
        invariant.addAll(getFunctionBinaryFeatures((FunctionBinary) current));

      // Float Invariants
      if (current instanceof MemberFloat)
        invariant.addAll(getMemberFloatFeatures((MemberFloat) current));
      if (current instanceof SeqFloatComparison)
        invariant.addAll(getSeqFloatComparisonFeatures((SeqFloatComparison) current));
      if (current instanceof SequenceFloat)
        invariant.addAll(getSequenceFloatFeatures((SequenceFloat) current));
      if (current instanceof FloatEqual)
        invariant.addAll(getFloatEqualFeatures((FloatEqual) current));
      if (current instanceof FloatNonEqual)
        invariant.addAll(getFloatNonEqualFeatures((FloatNonEqual) current));
      if (current instanceof FloatLessThan)
        invariant.addAll(getFloatLessThanFeatures((FloatLessThan) current));
      if (current instanceof FloatLessEqual)
        invariant.addAll(getFloatLessEqualFeatures((FloatLessEqual) current));
      if (current instanceof FloatGreaterThan)
        invariant.addAll(getFloatGreaterThanFeatures((FloatGreaterThan) current));
      if (current instanceof FloatGreaterEqual)
        invariant.addAll(getFloatGreaterEqualFeatures((FloatGreaterEqual) current));
      if (current instanceof FunctionUnaryFloat)
        invariant.addAll(getFunctionUnaryFloatFeatures((FunctionUnaryFloat) current));
      if (current instanceof LinearBinaryFloat)
        invariant.addAll(getLinearBinaryFloatFeatures((LinearBinaryFloat) current));
      if (current instanceof TwoFloat)
        invariant.addAll(getTwoFloatFeatures((TwoFloat) current));
      if (current instanceof SeqComparisonFloat)
        invariant.addAll(getSeqComparisonFloatFeatures((SeqComparisonFloat) current));
      if (current instanceof ReverseFloat)
        invariant.addAll(getReverseFloatFeatures((ReverseFloat) current));
      if (current instanceof SubSequenceFloat)
        invariant.addAll(getSubSequenceFloatFeatures((SubSequenceFloat) current));
      if (current instanceof PairwiseFloatComparison)
        invariant.addAll(getPairwiseFloatComparisonFeatures((PairwiseFloatComparison) current));
      if (current instanceof TwoSequenceFloat)
        invariant.addAll(getTwoSequenceFloatFeatures((TwoSequenceFloat) current));
      if (current instanceof PairwiseLinearBinaryFloat)
        invariant.addAll(getPairwiseLinearBinaryFloatFeatures((PairwiseLinearBinaryFloat) current));
      if (current instanceof PairwiseFunctionUnaryFloat)
        invariant.addAll(getPairwiseFunctionUnaryFloatFeatures((PairwiseFunctionUnaryFloat) current));
      if (current instanceof FunctionBinaryFloat)
        invariant.addAll(getFunctionBinaryFloatFeatures((FunctionBinaryFloat) current));
      if (current instanceof ThreeFloat)
        invariant.addAll(getThreeFloatFeatures((ThreeFloat) current));
      if (current instanceof LinearTernaryFloat)
        invariant.addAll(getLinearTernaryFloatFeatures((LinearTernaryFloat) current));
      if (current instanceof OneOfFloat)
        invariant.addAll(getOneOfFloatFeatures((OneOfFloat) current));
      if (current instanceof SingleFloat)
        invariant.addAll(getSingleFloatFeatures((SingleFloat) current));
      if (current instanceof LowerBoundFloat)
        invariant.addAll(getLowerBoundFloatFeatures((LowerBoundFloat) current));
      if (current instanceof UpperBoundFloat)
        invariant.addAll(getUpperBoundFloatFeatures((UpperBoundFloat) current));
      if (current instanceof NonZeroFloat)
        invariant.addAll(getNonZeroFloatFeatures((NonZeroFloat) current));
      if (current instanceof OneOfFloatSequence)
        invariant.addAll(getOneOfFloatSequenceFeatures((OneOfFloatSequence) current));
      if (current instanceof EltOneOfFloat)
        invariant.addAll(getEltOneOfFloatFeatures((EltOneOfFloat) current));
      if (current instanceof EltLowerBoundFloat)
        invariant.addAll(getEltLowerBoundFloatFeatures((EltLowerBoundFloat) current));
      if (current instanceof EltUpperBoundFloat)
        invariant.addAll(getEltUpperBoundFloatFeatures((EltUpperBoundFloat) current));
      if (current instanceof NoDuplicatesFloat)
        invariant.addAll(getNoDuplicatesFloatFeatures((NoDuplicatesFloat) current));
      if (current instanceof SeqIndexComparisonFloat)
        invariant.addAll(getSeqIndexComparisonFloatFeatures((SeqIndexComparisonFloat) current));
      if (current instanceof SeqIndexNonEqualFloat)
        invariant.addAll(getSeqIndexNonEqualFloatFeatures((SeqIndexNonEqualFloat) current));
      if (current instanceof CommonFloatSequence)
        invariant.addAll(getCommonFloatSequenceFeatures((CommonFloatSequence) current));
      if (current instanceof EltNonZeroFloat)
        invariant.addAll(getEltNonZeroFloatFeatures((EltNonZeroFloat) current));
      if (current instanceof SingleFloatSequence)
        invariant.addAll(getSingleFloatSequenceFeatures((SingleFloatSequence) current));
      if (current instanceof EltwiseFloatComparison)
        invariant.addAll(getEltwiseFloatComparisonFeatures((EltwiseFloatComparison) current));

      answer.add(invariant);
    }
    return answer;
  }

  // The rest of the methods extract features of various invariant
  // types (as defined by the invariant types they take as arguments)
  private static Vector getCommonFeatures(Invariant inv) {
    Vector answer = new Vector();
    if (inv.enoughSamples()) {
      answer.add(new IntDoublePair(FetEnoughSamplesBool, 1));
      answer.add(new IntDoublePair(FetGetProbabilityFloat, inv.getProbability()));
      if (inv.isExact()) answer.add(new IntDoublePair(FetIsExactBool, 1));
      if (inv.justified()) answer.add(new IntDoublePair(FetJustifiedBool, 1));
      if (inv.isWorthPrinting()) answer.add(new IntDoublePair(FetIsWorthPrintingBool, 1));
      if (inv.hasFewModifiedSamples()) answer.add(new IntDoublePair(FetHasFewModifiedSamplesBool, 1));
      /* [INCR]
      if (inv.hasNonCanonicalVariable()) answer.add(new IntDoublePair(FetHasNonCanonicalVariableBool, 1));
      if (inv.hasOnlyConstantVariables()) answer.add(new IntDoublePair(FetHasOnlyConstantVariablesBool, 1));
      */ // [INCR]
      if (inv.isObvious()) answer.add(new IntDoublePair(FetIsObviousBool, 1));
      /* [INCR]
      if (inv.isObviousDerived()) answer.add(new IntDoublePair(FetIsObviousDerivedBool, 1));
      if (inv.isObviousImplied()) answer.add(new IntDoublePair(FetIsObviousImpliedBool, 1));
      if (inv.isControlled()) answer.add(new IntDoublePair(FetIsControlledBool, 1));
      if (inv.isImpliedPostcondition()) answer.add(new IntDoublePair(FetIsImpliedPostconditionBool, 1));
      */ // INCR
      if (inv.isInteresting()) answer.add(new IntDoublePair(FetIsInterestingBool, 1));
      answer.addAll(getPptFeatures(inv.ppt));
    }
    return answer;
  }

  // get the Ppt features
  private static Vector getPptFeatures(PptSlice ppt) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetArityInt, ppt.arity));
    PptTopLevel pptTop = ppt.parent;
    answer.add(new IntDoublePair(FetNumVarsInt, pptTop.num_vars()));
    answer.add(new IntDoublePair(FetNumArrayVarsInt, pptTop.num_array_vars()));
    /* [INCR]
    if (pptTop.entry_ppt != null)
      answer.add(new IntDoublePair(FetPptIsExitBool, 1));
    if (pptTop.combined_exit != null)
      answer.add(new IntDoublePair(FetPptIsLineNumberedExitBool, 1));
    answer.add(new IntDoublePair(FetPptNumOfExitsInt, pptTop.exit_ppts.size()));
    */ // [INCR]
    return answer;
  }

  // get the variable features
  private static Vector getVarFeatures(VarInfo[] var_infos) {
    Vector answer = new Vector();
    // the i = 0 case is the OR of all the other i cases.
    for (int i = 1; i <= var_infos.length; i++) {
      VarInfo var = var_infos[i-1];
      // delete feature because hash code is not a continuous function
      if (var.is_static_constant) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoIs_Static_ConstantBool, 1));
        answer.add(new IntDoublePair(FetVarInfoIs_Static_ConstantBool, 1)); }
      /* [INCR]
      if (var.canBeNull) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoCanBeNullBool, 1));
        answer.add(new IntDoublePair(FetVarInfoIs_Static_ConstantBool, 1)); }
      if (var.is_dynamic_constant) {
        answer.add(new IntDoublePair(i*10000+FetVarInfoIs_Dynamic_ConstantBool,1));
        answer.add(new IntDoublePair(FetVarInfoIs_Dynamic_ConstantBool, 1)); }
      */ // [INCR]
      if (var.isPrestate()) {
        answer.add(new IntDoublePair(i*10000 + FetVarIsPrestateBool, 1));
        answer.add(new IntDoublePair(FetVarIsPrestateBool, 1)); }
      answer.add(new IntDoublePair(i * 10000 + FetVarDerivedDepthInt, var.derivedDepth()));

      VarInfoAux aux = var.aux;
      if (aux.getFlag(VarInfoAux.IS_PARAM)) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoAuxIsParamBool, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxIsParamBool, 1)); }
      if (aux.getFlag(VarInfoAux.NULL_TERMINATING)) {
        answer.add(new IntDoublePair(i*10000+FetVarInfoAuxNullTerminatingBool, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxNullTerminatingBool, 1)); }
      if (aux.getFlag(VarInfoAux.HAS_NULL)) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoAuxHasNullBool, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxHasNullBool, 1)); }
      if (aux.getFlag(VarInfoAux.HAS_SIZE)) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoAuxHasSizeBool, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxHasSizeBool, 1)); }
      if (aux.getFlag(VarInfoAux.HAS_ORDER)) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoAuxHasOrderBool, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxHasOrderBool, 1)); }
      if (aux.getFlag(VarInfoAux.HAS_DUPLICATES)) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoAuxHasDuplicatesBool, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxHasDuplicatesBool, 1)); }

      ProglangType type = var.type;
      answer.add(new IntDoublePair(i*10000 + FetTypeDimensionsInt, type.dimensions()));
      // delete feature because hash code is not a continuous function
      // answer.add(new IntDoublePair(i*10000 + FetTypeBase, type.base().hashCode()));
      if (type.isArray()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsArrayBool, 1));
        answer.add(new IntDoublePair(FetTypeIsArrayBool, 1)); }
      answer.add(new IntDoublePair(i*10000 + FetTypePseudoDimensionsInt, type.pseudoDimensions()));
      if (type.isPseudoArray()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsPseudoArrayBool, 1));
        answer.add(new IntDoublePair(FetTypeIsPseudoArrayBool, 1)); }
      if (type.isPrimitive()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsPrimitiveBool, 1));
        answer.add(new IntDoublePair(FetTypeIsPrimitiveBool, 1)); }
      if (type.baseIsPrimitive()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeBaseIsArrayBool, 1));
        answer.add(new IntDoublePair(FetTypeBaseIsArrayBool, 1)); }
      if (type.isIntegral()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsIntegralBool, 1));
        answer.add(new IntDoublePair(FetTypeIsIntegralBool, 1)); }
      if (type.baseIsIntegral()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeBaseIsIntegralBool, 1));
        answer.add(new IntDoublePair(FetTypeBaseIsIntegralBool, 1)); }
      if (type.elementIsIntegral()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeElementIsIntegralBool, 1));
        answer.add(new IntDoublePair(FetTypeElementIsIntegralBool, 1)); }
      if (type.isScalar()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsScalarBool, 1));
        answer.add(new IntDoublePair(FetTypeIsScalarBool, 1)); }
      if (type.isFloat()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsFloatBool, 1));
        answer.add(new IntDoublePair(FetTypeIsFloatBool, 1)); }
      if (type.baseIsFloat()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeBaseIsFloatBool, 1));
        answer.add(new IntDoublePair(FetTypeBaseIsFloatBool, 1)); }
      if (type.isObject()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsObjectBool, 1));
        answer.add(new IntDoublePair(FetTypeIsObjectBool, 1)); }
      if (type.baseIsObject()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeBaseIsObjectBool, 1));
        answer.add(new IntDoublePair(FetTypeBaseIsObjectBool, 1)); }
    }

 return answer;
  }

  private static Vector getModulusFeatures(Modulus inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetModulusBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getLowerBoundFeatures(LowerBound inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLowerBoundBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getLowerBoundCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getLowerBoundFloatFeatures(LowerBoundFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLowerBoundFloatBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getLowerBoundCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getNonZeroFeatures(NonZero inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNonZeroBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getNonZeroFloatFeatures(NonZeroFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNonZeroFloatBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getNonModulusFeatures(NonModulus inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNonModulusBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getOneOfScalarFeatures(OneOfScalar inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfScalarBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getPositiveFeatures(Positive inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPositiveBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getSingleFloatFeatures(SingleFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleFloatBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getSingleScalarFeatures(SingleScalar inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleScalarBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getUpperBoundFeatures(UpperBound inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetUpperBoundBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getUpperBoundCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getUpperBoundFloatFeatures(UpperBoundFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetUpperBoundFloatBool, 1));
    answer.add(new IntDoublePair(FetScalarBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getUpperBoundCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getUpperBoundCoreFeatures(UpperBoundCore core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetUpperBoundCoreMax1Float, core.max1));
    return answer;
  }

  private static Vector getUpperBoundCoreFloatFeatures(UpperBoundCoreFloat core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetUpperBoundCoreFloatMax1Float, core.max1));
    return answer;
  }

  private static Vector getLowerBoundCoreFeatures(LowerBoundCore core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetLowerBoundCoreMin1Float, core.min1));
    return answer;
  }

  private static Vector getLowerBoundCoreFloatFeatures(LowerBoundCoreFloat core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetLowerBoundCoreFloatMin1Float, core.min1));
    return answer;
  }

  private static Vector getEltLowerBoundFeatures(EltLowerBound inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltLowerBoundBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getLowerBoundCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getEltLowerBoundFloatFeatures(EltLowerBoundFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltLowerBoundFloatBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getLowerBoundCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getSequenceFloatFeatures(SequenceFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSequenceFloatBool, 1));
    return answer;
  }

  private static Vector getMemberFloatFeatures(MemberFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetMemberFloatBool, 1));
    return answer;
  }

  private static Vector getCommonFloatSequenceFeatures(CommonFloatSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetCommonFloatSequenceBool, 1));
    return answer;
  }

  private static Vector getEltNonZeroFeatures(EltNonZero inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltNonZeroBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getEltNonZeroFloatFeatures(EltNonZeroFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltNonZeroFloatBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getEltOneOfFeatures(EltOneOf inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltOneOfBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getEltOneOfFloatFeatures(EltOneOfFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltOneOfFloatBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getEltUpperBoundFeatures(EltUpperBound inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltUpperBoundBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getUpperBoundCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getEltUpperBoundFloatFeatures(EltUpperBoundFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltUpperBoundFloatBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getUpperBoundCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getEltwiseIntComparisonFeatures(EltwiseIntComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltwiseIntComparisonBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));

    // Provide the same features as the core without using the core as it is
    // now not a part of EltwiseIntComparison (Alan - 1/20)

    if (inv instanceof EltwiseIntEqual)
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_EqBool, 1));
    if ((inv instanceof EltwiseIntLessThan) || (inv instanceof EltwiseIntLessEqual))
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_LtBool, 1));
    if ((inv instanceof EltwiseIntGreaterThan) || (inv instanceof EltwiseIntGreaterEqual))
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_GtBool, 1));
    return answer;
  }

  private static Vector getEltwiseFloatComparisonFeatures(EltwiseFloatComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltwiseFloatComparisonBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));

    // Provide the same features as the core without using the core as it is
    // now not a part of EltwiseIntComparison (Alan - 1/20)

    if (inv instanceof EltwiseFloatEqual)
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_EqBool, 1));
    if ((inv instanceof EltwiseFloatLessThan) || (inv instanceof EltwiseFloatLessEqual))
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_LtBool, 1));
    if ((inv instanceof EltwiseFloatGreaterThan) || (inv instanceof EltwiseFloatGreaterEqual))
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_GtBool, 1));
    return answer;
  }

  private static Vector getIntComparisonCoreFeatures(IntComparisonCore core) {
    Vector answer = new Vector();
    if (core.can_be_eq)
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_EqBool, 1));
    if (core.can_be_lt)
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_LtBool, 1));
    if (core.can_be_gt)
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_GtBool, 1));

    return answer;
  }

  private static Vector getFloatComparisonCoreFeatures(FloatComparisonCore core) {
    Vector answer = new Vector();
    if (core.can_be_eq)
      answer.add(new IntDoublePair(FetFloatComparisonCoreCan_Be_EqBool, 1));
    if (core.can_be_lt)
      answer.add(new IntDoublePair(FetFloatComparisonCoreCan_Be_LtBool, 1));
    if (core.can_be_gt)
      answer.add(new IntDoublePair(FetFloatComparisonCoreCan_Be_GtBool, 1));
    return answer;
  }

  private static Vector getNoDuplicatesFeatures(NoDuplicates inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNoDuplicatesBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getNoDuplicatesFloatFeatures(NoDuplicatesFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNoDuplicatesFloatBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getOneOfSequenceFeatures(OneOfSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfSequenceBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getOneOfFloatSequenceFeatures(OneOfFloatSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfFloatSequenceBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getSeqIndexComparisonFeatures(SeqIndexComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqIndexComparisonBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getIntComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSeqIndexComparisonFloatFeatures(SeqIndexComparisonFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqIndexComparisonFloatBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getFloatComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSeqIndexNonEqualFeatures(SeqIndexNonEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqIndexNonEqualBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getNonEqualCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSeqIndexNonEqualFloatFeatures(SeqIndexNonEqualFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqIndexNonEqualFloatBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.addAll(getNonEqualCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getNonEqualCoreFeatures(NonEqualCore core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetNonEqualCoreMin1Float, core.min1));
    //    answer.add(new IntDoublePair(FetNonEqualCoreMin2Float, core.min2));
    //    answer.add(new IntDoublePair(FetNonEqualCoreMax1Float, core.max1));
    //    answer.add(new IntDoublePair(FetNonEqualCoreMax2Float, core.max2));
    return answer;
  }

  private static Vector getNonEqualCoreFloatFeatures(NonEqualCoreFloat core) {
    Vector answer = new Vector();
    return answer;
  }

  private static Vector getSingleFloatSequenceFeatures(SingleFloatSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleFloatSequenceBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getSingleSequenceFeatures(SingleSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleSequenceBool, 1));
    answer.add(new IntDoublePair(FetSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getOneOfStringFeatures(OneOfString inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfStringBool, 1));
    answer.add(new IntDoublePair(FetStringBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getSingleStringFeatures(SingleString inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleStringBool, 1));
    answer.add(new IntDoublePair(FetStringBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    return answer;
  }

  private static Vector getEltOneOfStringFeatures(EltOneOfString inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfStringBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.add(new IntDoublePair(FetStringSequenceBool, 1));
    return answer;
  }

  private static Vector getOneOfStringSequenceFeatures(OneOfStringSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfStringSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.add(new IntDoublePair(FetStringSequenceBool, 1));
    return answer;
  }

  private static Vector getSingleStringSequenceFeatures(SingleStringSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleStringSequenceBool, 1));
    answer.add(new IntDoublePair(FetUnaryBool, 1));
    answer.add(new IntDoublePair(FetStringSequenceBool, 1));
    return answer;
  }

  private static Vector getOneOfFeatures(OneOf inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfNum_eltsInt, inv.num_elts()));
    return answer;
  }

  private static Vector getOneOfFloatFeatures(OneOfFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfFloatNum_eltsInt, inv.num_elts()));
    return answer;
  }

  private static Vector getComparisonFeatures(Comparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetComparisonBool, 1));
    answer.add(new IntDoublePair(FetComparisonEq_probabilityFloat, inv.eq_probability()));
    return answer;
  }

  private static Vector getImplicationFeatures(Implication inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetImplicationBool, 1));
    if (inv.iff) answer.add(new IntDoublePair(FetImplicationIffBool, 1));
    return answer;
  }

  private static Vector getSeqIntComparisonFeatures(SeqIntComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqIntComparisonBool, 1));
    answer.add(new IntDoublePair(FetSequenceScalarBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.addAll(getIntComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSeqFloatComparisonFeatures(SeqFloatComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqFloatComparisonBool, 1));
    answer.add(new IntDoublePair(FetSequenceScalarBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.addAll(getFloatComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSequenceScalarFeatures(SequenceScalar inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSequenceScalarBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    if (inv.seq_first) answer.add(new IntDoublePair(FetSequenceScalarSeq_firstBool, 1));
    if (inv.seq_index==1) answer.add(new IntDoublePair(FetSequenceScalarSeq_indexBool, 1));
    if (inv.scl_index==1) answer.add(new IntDoublePair(FetSequenceScalarScl_indexBool, 1));
    return answer;
  }

  private static Vector getSequenceStringFeatures(SequenceString inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSequenceStringBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    if (inv.seq_first) answer.add(new IntDoublePair(FetSequenceStringSeq_firstBool, 1));
    if (inv.seq_index==1) answer.add(new IntDoublePair(FetSequenceStringSeq_indexBool, 1));
    if (inv.scl_index==1) answer.add(new IntDoublePair(FetSequenceStringScl_indexBool, 1));
    return answer;
  }

  private static Vector getIntNonEqualFeatures(IntNonEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntNonEqualBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  private static Vector getFloatNonEqualFeatures(FloatNonEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatNonEqualBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  private static Vector getIntEqualFeatures(IntEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntEqualBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  private static Vector getFloatEqualFeatures(FloatEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatEqualBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  private static Vector getFunctionUnaryFeatures(FunctionUnary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionUnaryBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    answer.addAll(getFunctionUnaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getFunctionUnaryFloatFeatures(FunctionUnaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionUnaryFloatBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    answer.addAll(getFunctionUnaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getFunctionUnaryCoreFeatures(FunctionUnaryCore core) {
    Vector answer = new Vector();
    if (core.inverse)
      answer.add(new IntDoublePair(FetFunctionUnaryCoreInverseBool, 1));
    return answer;
  }

  private static Vector getFunctionUnaryCoreFloatFeatures(FunctionUnaryCoreFloat core) {
    Vector answer = new Vector();
    if (core.inverse)
      answer.add(new IntDoublePair(FetFunctionUnaryCoreFloatInverseBool, 1));
    return answer;
  }

  private static Vector getIntGreaterEqualFeatures(IntGreaterEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntGreaterEqualBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  private static Vector getFloatGreaterEqualFeatures(FloatGreaterEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatGreaterEqualBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  private static Vector getIntGreaterThanFeatures(IntGreaterThan inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntGreaterThanBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  private static Vector getFloatGreaterThanFeatures(FloatGreaterThan inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatGreaterThanBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  private static Vector getLinearBinaryFeatures(LinearBinary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearBinaryBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    answer.addAll(getLinearBinaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getLinearBinaryFloatFeatures(LinearBinaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearBinaryFloatBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    answer.addAll(getLinearBinaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getLinearBinaryCoreFeatures(LinearBinaryCore core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearBinaryCoreAFloat, core.a));
    answer.add(new IntDoublePair(FetLinearBinaryCoreBFloat, core.b));
    return answer;
  }

  private static Vector getLinearBinaryCoreFloatFeatures(LinearBinaryCoreFloat core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearBinaryCoreFloatAFloat, core.a));
    answer.add(new IntDoublePair(FetLinearBinaryCoreFloatBFloat, core.b));
    return answer;
  }

  private static Vector getTwoFloatFeatures(TwoFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetTwoFloatBool, 1));
    return answer;
  }

  private static Vector getTwoScalarFeatures(TwoScalar inv) {
    Vector answer = new Vector();
    return answer;
  }

  private static Vector getIntLessEqualFeatures(IntLessEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntLessEqualBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  private static Vector getFloatLessEqualFeatures(FloatLessEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatLessEqualBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  private static Vector getIntLessThanFeatures(IntLessThan inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntLessThanBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  private static Vector getFloatLessThanFeatures(FloatLessThan inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatLessThanBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoScalarBool, 1));
    return answer;
  }

  /*  private static Vector getIntComparisonFeatures(IntComparisons inv) {
      Vector answer = new Vector();
      answer.add(new IntDoublePair(FetIntComparisonBool, 1));
      answer.add(new IntDoublePair(FetBinaryBool, 1));
      answer.add(new IntDoublePair(FetTwoScalarBool, 1));
      return answer;
      } */

  private static Vector getPairwiseIntComparisonFeatures(PairwiseIntComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseIntComparisonBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    answer.addAll(getIntComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getPairwiseFloatComparisonFeatures(PairwiseFloatComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseFloatComparisonBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    answer.addAll(getFloatComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSeqComparisonFeatures(SeqComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqComparisonBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    return answer;
  }

  private static Vector getSeqComparisonFloatFeatures(SeqComparisonFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqFloatComparisonBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    return answer;
  }

  private static Vector getTwoSequenceFeatures(TwoSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    return answer;
  }

  private static Vector getTwoSequenceFloatFeatures(TwoSequenceFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetTwoSequenceFloatBool, 1));
    return answer;
  }

  private static Vector getPairwiseLinearBinaryFeatures(PairwiseLinearBinary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseLinearBinaryBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    answer.addAll(getLinearBinaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getPairwiseLinearBinaryFloatFeatures(PairwiseLinearBinaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseLinearBinaryFloatBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    answer.addAll(getLinearBinaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getSubSequenceFeatures(SubSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSubSequenceBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    return answer;
  }

  private static Vector getSubSequenceFloatFeatures(SubSequenceFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSubSequenceFloatBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    return answer;
  }

  private static Vector getPairwiseFunctionUnaryFeatures(PairwiseFunctionUnary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseFunctionUnaryBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    answer.addAll(getFunctionUnaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getPairwiseFunctionUnaryFloatFeatures(PairwiseFunctionUnaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseFunctionUnaryFloatBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    answer.addAll(getFunctionUnaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getReverseFeatures(Reverse inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetReverseBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    return answer;
  }

  private static Vector getReverseFloatFeatures(ReverseFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetReverseFloatBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoSequenceBool, 1));
    return answer;
  }

  private static Vector getStringComparisonFeatures(StringComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetStringComparisonBool, 1));
    answer.add(new IntDoublePair(FetBinaryBool, 1));
    answer.add(new IntDoublePair(FetTwoStringBool, 1));
    answer.addAll(getStringComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getStringComparisonCoreFeatures(StringComparisonCore core) {
    Vector answer = new Vector();
    if (core.can_be_eq)
       answer.add(new IntDoublePair(FetStringComparisonCoreCan_Be_EqBool, 1));
    if (core.can_be_lt)
       answer.add(new IntDoublePair(FetStringComparisonCoreCan_Be_LtBool, 1));
    if (core.can_be_gt)
       answer.add(new IntDoublePair(FetStringComparisonCoreCan_Be_GtBool, 1));
    return answer;
  }

  private static Vector getTwoStringFeatures(TwoString inv) {
    Vector answer = new Vector();
    return answer;
  }

  private static Vector getThreeScalarFeatures(ThreeScalar inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetThreeScalarBool, 1));
    return answer;
  }

  private static Vector getThreeFloatFeatures(ThreeFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetThreeFloatBool, 1));
    return answer;
  }

  private static Vector getLinearTernaryFeatures(LinearTernary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearTernaryBool, 1));
    answer.add(new IntDoublePair(FetTernaryBool, 1));
    answer.add(new IntDoublePair(FetThreeScalarBool, 1));
    answer.addAll(getLinearTernaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getLinearTernaryFloatFeatures(LinearTernaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearTernaryFloatBool, 1));
    answer.add(new IntDoublePair(FetTernaryBool, 1));
    answer.add(new IntDoublePair(FetThreeScalarBool, 1));
    answer.addAll(getLinearTernaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getLinearTernaryCoreFeatures(LinearTernaryCore core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearTernaryCoreAFloat, core.a));
    answer.add(new IntDoublePair(FetLinearTernaryCoreBFloat, core.b));
    answer.add(new IntDoublePair(FetLinearTernaryCoreCFloat, core.c));
    return answer;
  }

  private static Vector getLinearTernaryCoreFloatFeatures(LinearTernaryCoreFloat core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearTernaryCoreFloatAFloat, core.a));
    answer.add(new IntDoublePair(FetLinearTernaryCoreFloatBFloat, core.b));
    answer.add(new IntDoublePair(FetLinearTernaryCoreFloatCFloat, core.c));
    return answer;
  }

  private static Vector getFunctionBinaryFeatures(FunctionBinary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionBinaryBool, 1));
    answer.add(new IntDoublePair(FetTernaryBool, 1));
    answer.add(new IntDoublePair(FetThreeScalarBool, 1));
    answer.addAll(getFunctionBinaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getFunctionBinaryFloatFeatures(FunctionBinaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionBinaryFloatBool, 1));
    answer.add(new IntDoublePair(FetTernaryBool, 1));
    answer.add(new IntDoublePair(FetThreeScalarBool, 1));
    answer.addAll(getFunctionBinaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getFunctionBinaryCoreFeatures(FunctionBinaryCore core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionBinaryCoreVar_OrderInt, core.var_order));
    return answer;
  }

  private static Vector getFunctionBinaryCoreFloatFeatures(FunctionBinaryCoreFloat core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionBinaryCoreFloatVar_OrderInt, core.var_order));
    return answer;
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

    // Compares an Object to this
    // Throws ClassCastException if o is not an IntDoublePair
    public int compareTo(Object o) throws ClassCastException{
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
      "\tArguments:\n\t-i FileName:\ta SVMfu or C5 input file (with .data)\n" +
      "\t-t Type:\tFormat, one of C5 or SVMfu\n" +
      "\t-o FileName:\toutput file name (with.data)\n" +
      "\t[-n] repeat:\tif present then the number of positive and negative\n" +
      "\t\tvectors will be roughtly normalized (by repeats).\n";

    static public void main(String[] args)
      throws IOException, ClassNotFoundException {

      // First parse the arguments
      if (args.length == 0) {
        System.out.println(USAGE);
        System.exit(0);
      }
      Vector inputs = new Vector();
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
      Vector posvectors = new Vector();
      Vector negvectors = new Vector();

      for (Iterator i = neg.iterator(); i.hasNext(); ) {
        String vector = (String) i.next();
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
      FileWriter fw = new FileWriter(output);
      for (int repeat = 0; repeat < negrepeat; repeat++)
        for (Iterator i = negvectors.iterator(); i.hasNext(); )
          fw.write((String) i.next() + " \n");
      for (int repeat = 0; repeat < posrepeat; repeat++)
        for (Iterator i = posvectors.iterator(); i.hasNext(); )
          fw.write((String) i.next() + " \n");
      fw.close();

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
      "\tArguments:\n\t-d FileName:\tSVMfu or C5 training data (with .data)\n"+
      "\t-s FileName:\tSVMfu or C5 test data (with .data)\n" +
      "\t-t Type:\tFormat, one of C5 or SVMfu\n";

    static public void main(String[] args)
      throws IOException, ClassNotFoundException {

      // First parse the arguments
      if (args.length == 0) {
        System.out.println(USAGE);
        System.exit(0);
      }
      Vector trains = new Vector();
      Vector tests = new Vector();
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
      Vector testGood = new Vector();
      Vector testBad = new Vector();

      for (Iterator i = trains.iterator(); i.hasNext(); ) {
        BufferedReader br=new BufferedReader(new FileReader((String)i.next()));
        br.readLine();
        while (br.ready()) {
          String vector = br.readLine();

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

      /*
        for (Iterator i = pos.iterator(); i.hasNext(); )
        if (type.equals("C5"))
          posvectors.add(((String) i.next()) + "good");
        else if (type.equals("SVMfu"))
          posvectors.add(((String) i.next()) + "1");

      // Print the output to the output file.
      FileWriter fw = new FileWriter(output);
      for (int repeat = 0; repeat < negrepeat; repeat++)
        for (Iterator i = negvectors.iterator(); i.hasNext(); )
          fw.write((String) i.next() + " \n");
      for (int repeat = 0; repeat < posrepeat; repeat++)
        for (Iterator i = posvectors.iterator(); i.hasNext(); )
          fw.write((String) i.next() + " \n");
      fw.close();

      // Print a summary of positives and negatives to stdout.
      System.out.println(posvectors.size() + "*" + posrepeat + " " +
                         negvectors.size() + "*" + negrepeat);
      }
      */
  }

  private static void writeVectors(Vector one, Vector two,
                                     String label, FileWriter fw)
    throws IOException {

    for (int i = 0; i < one.size(); i++)
      for (int j = 0; j < two.size(); j++) {
        String first = (String) one.get(i);
        String second = (String) two.get(j);
        String answer = first.substring(first.indexOf(" ") + 1) +
          shift(second);
        answer = (new StringTokenizer(answer)).countTokens() + " " + answer;
        fw.write(answer + label + "\n");
      }
  }

  private static String shift(String vector) {
    String answer = new String();
    StringTokenizer tokens = new StringTokenizer(vector);
    tokens.nextToken();
    while (tokens.hasMoreTokens())
      answer += (Integer.parseInt(tokens.nextToken()) +
                 OneMoreOrderThanLargestFeature) + " " +
        tokens.nextToken() + " ";
    return answer;
  }


  // the following line gets rid of some extra output that
  // otherwise gets dumped to System.out:
  static {
    LogHelper.setupLogs(false ? LogHelper.DEBUG : LogHelper.INFO);
  }

  // the THRESHOLD is zero
  static double THRESHOLD = 0.0;

  // A bunch of public static variables, one for each feature
  public static int FetEnoughSamplesBool = 1;
  public static int FetGetProbabilityFloat = 2;
  public static int FetIsExactBool = 3;
  public static int FetJustifiedBool = 4;
  public static int FetIsWorthPrintingBool = 5;
  public static int FetHasFewModifiedSamplesBool = 6;
  public static int FetHasNonCanonicalVariableBool = 7;
  public static int FetHasOnlyConstantVariablesBool = 8;
  public static int FetIsObviousBool = 9;
  public static int FetIsObviousDerivedBool = 10;
  public static int FetIsObviousImpliedBool = 11;
  public static int FetIsControlledBool = 12;
  public static int FetIsImpliedPostconditionBool = 13;
  public static int FetIsInterestingBool = 14;
  public static int FetArityInt = 15;
  public static int FetNumVarsInt = 16;
  public static int FetNumArrayVarsInt = 17;
  public static int FetOneOfNum_eltsInt = 50;
  public static int FetComparisonBool = 51;
  public static int FetComparisonEq_probabilityFloat = 52;
  public static int FetImplicationBool = 53;
  public static int FetImplicationIffBool = 54;
  public static int FetUnaryBool = 81;
  public static int FetScalarBool = 82;
  public static int FetSequenceBool = 83;
  public static int FetStringBool = 84;
  public static int FetStringSequenceBool = 85;
  public static int FetBinaryBool = 86;
  public static int FetTwoScalarBool = 88;
  public static int FetTwoSequenceBool = 89;
  public static int FetTwoStringBool = 90;
  public static int FetTernaryBool = 91;
  public static int FetThreeScalarBool = 92;
  public static int FetModulusBool = 100;
  public static int FetLowerBoundBool = 200;
  public static int FetLowerBoundCoreMin1Float = 201;
  public static int FetNonZeroBool = 300;
  public static int FetNonModulusBool = 400;
  public static int FetOneOfScalarBool = 500;
  public static int FetPositiveBool = 600;
  public static int FetSingleFloatBool = 700;
  public static int FetSingleScalarBool = 800;
  public static int FetUpperBoundBool = 900;
  public static int FetUpperBoundCoreMax1Float = 901;
  public static int FetEltLowerBoundBool = 1000;
  public static int FetEltNonZeroBool = 1100;
  public static int FetEltOneOfBool = 1200;
  public static int FetEltUpperBoundBool = 1300;
  public static int FetEltwiseIntComparisonBool = 1400;
  public static int FetNoDuplicatesBool = 1500;
  public static int FetOneOfSequenceBool = 1600;
  public static int FetSeqIndexComparisonBool = 1700;
  public static int FetSeqIndexNonEqualBool = 1800;
  public static int FetSingleFloatSequenceBool = 1900;
  public static int FetSingleSequenceBool = 2000;
  public static int FetOneOfStringBool = 2200;
  public static int FetSingleStringBool = 2300;
  public static int FetEltOneOfStringBool = 2400;
  public static int FetOneOfStringSequenceBool = 2500;
  public static int FetSingleStringSequenceBool = 2600;
  public static int FetSeqIntComparisonBool = 2800;
  public static int FetSequenceScalarBool = 2900;
  public static int FetSequenceScalarSeq_firstBool = 2901;
  public static int FetSequenceScalarSeq_indexBool = 2902;
  public static int FetSequenceScalarScl_indexBool = 2903;
  public static int FetSequenceStringBool = 3000;
  public static int FetSequenceStringSeq_firstBool = 3001;
  public static int FetSequenceStringSeq_indexBool = 3002;
  public static int FetSequenceStringScl_indexBool = 3003;
  public static int FetIntNonEqualBool = 3100;
  public static int FetIntEqualBool = 3200;
  public static int FetNonEqualCoreMin1Float = 3301;
  public static int FetNonEqualCoreMin2Float = 3302;
  public static int FetNonEqualCoreMax1Float = 3303;
  public static int FetNonEqualCoreMax2Float = 3304;
  public static int FetFunctionUnaryBool = 3400;
  public static int FetFunctionUnaryCoreInverseBool = 3401;
  public static int FetIntGreaterEqualBool = 3500;
  public static int FetIntGreaterThanBool = 3600;
  public static int FetLinearBinaryBool = 3700;
  public static int FetLinearBinaryCoreAFloat = 3701;
  public static int FetLinearBinaryCoreBFloat = 3702;
  public static int FetIntLessEqualBool = 3800;
  public static int FetIntLessThanBool = 3900;
  public static int FetIntComparisonCoreCan_Be_EqBool = 4001;
  public static int FetIntComparisonCoreCan_Be_LtBool = 4002;
  public static int FetIntComparisonCoreCan_Be_GtBool = 4003;
  public static int FetPairwiseIntComparisonBool = 4800;
  public static int FetSeqComparisonBool = 4900;
  public static int FetPairwiseLinearBinaryBool = 5000;
  public static int FetSubSequenceBool = 5100;
  public static int FetPairwiseFunctionUnaryBool = 5200;
  public static int FetReverseBool = 5300;
  public static int FetStringComparisonBool = 5500;
  public static int FetStringComparisonCoreCan_Be_EqBool = 5501;
  public static int FetStringComparisonCoreCan_Be_LtBool = 5502;
  public static int FetStringComparisonCoreCan_Be_GtBool = 5503;
  public static int FetLinearTernaryBool = 5700;
  public static int FetLinearTernaryCoreAFloat = 5701;
  public static int FetLinearTernaryCoreBFloat = 5702;
  public static int FetLinearTernaryCoreCFloat = 5703;
  public static int FetFunctionBinaryBool = 5800;
  public static int FetFunctionBinaryCoreVar_OrderInt = 5801;
  public static int FetCommonFloatSequenceBool = 5900;
  public static int FetEltLowerBoundFloatBool = 6000;
  public static int FetEltNonZeroFloatBool = 6100;
  public static int FetEltOneOfFloatBool = 6200;
  public static int FetEltUpperBoundFloatBool = 6300;
  public static int FetEltwiseFloatComparisonBool = 6400;
  public static int FetFloatEqualBool = 6500;
  public static int FetFloatGreaterEqualBool = 6600;
  public static int FetFloatGreaterThanBool = 6700;
  public static int FetFloatLessEqualBool = 6800;
  public static int FetFloatLessThanBool = 6900;
  public static int FetFloatNonEqualBool = 7000;
  public static int FetFunctionBinaryFloatBool = 7100;
  public static int FetFunctionUnaryFloatBool = 7200;
  public static int FetLinearBinaryFloatBool = 7300;
  public static int FetLinearTernaryFloatBool = 7400;
  public static int FetLowerBoundFloatBool = 7500;
  public static int FetMemberFloatBool = 7600;
  public static int FetNoDuplicatesFloatBool = 7700;
  public static int FetNonZeroFloatBool = 7800;
  public static int FetOneOfFloatBool = 7900;
  public static int FetOneOfFloatNum_eltsInt = 7901;
  public static int FetOneOfFloatSequenceBool = 8000;
  public static int FetPairwiseFloatComparisonBool = 8100;
  public static int FetPairwiseFunctionUnaryFloatBool = 8200;
  public static int FetPairwiseLinearBinaryFloatBool = 8300;
  public static int FetReverseFloatBool = 8400;
  public static int FetSeqComparisonFloatBool = 8500;
  public static int FetSeqFloatComparisonBool = 8600;
  public static int FetSeqIndexComparisonFloatBool = 8700;
  public static int FetSeqIndexNonEqualFloatBool = 8800;
  public static int FetSequenceFloatBool = 8900;
  public static int FetSubSequenceFloatBool = 9200;
  public static int FetThreeFloatBool = 9300;
  public static int FetTwoFloatBool = 9400;
  public static int FetTwoSequenceFloatBool = 9500;
  public static int FetUpperBoundFloatBool = 9600;
  public static int FetFunctionUnaryCoreFloatInverseBool = 9701;
  public static int FetFunctionBinaryCoreFloatVar_OrderInt = 9702;
  public static int FetLinearBinaryCoreFloatAFloat = 9703;
  public static int FetLinearBinaryCoreFloatBFloat = 9704;
  public static int FetLinearTernaryCoreFloatAFloat = 9703;
  public static int FetLinearTernaryCoreFloatBFloat = 9705;
  public static int FetLinearTernaryCoreFloatCFloat = 9706;
  public static int FetFloatComparisonCoreCan_Be_EqBool = 9707;
  public static int FetFloatComparisonCoreCan_Be_LtBool = 9708;
  public static int FetFloatComparisonCoreCan_Be_GtBool = 9709;

  // 9800-9900 reserved for Ppt Features
  public static int FetPptIsExitBool = 9801;
  public static int FetPptIsLineNumberedExitBool = 9802;
  public static int FetPptNumOfExitsInt = 9803;

  // Variable Features (10000 - 49999)
  // 10000-19999 are for "invariant contains a variable that ...
  // 20000 - 29999 are for 1st variable is .... (etc. up to 3 variables)
  //  public static int FetVarInfoName = 10001;
  public static int FetVarInfoIs_Static_ConstantBool = 10002;
  public static int FetVarInfoCanBeNullBool = 10003;
  public static int FetVarInfoIs_Dynamic_ConstantBool =  10004;
  public static int FetTypeDimensionsInt = 10005;
  public static int FetTypeIsArrayBool = 10007;
  public static int FetTypeBaseIsArrayBool = 10008;
  public static int FetTypePseudoDimensionsInt = 10009;
  public static int FetTypeIsPseudoArrayBool = 10010;
  public static int FetTypeIsPrimitiveBool = 10011;
  public static int FetTypeBaseIsPrimitiveBool = 10012;
  public static int FetTypeIsIntegralBool = 10013;
  public static int FetTypeBaseIsIntegralBool = 10014;
  public static int FetTypeElementIsIntegralBool = 10015;
  public static int FetTypeIsScalarBool = 10016;
  public static int FetTypeIsFloatBool = 10017;
  public static int FetTypeBaseIsFloatBool = 10018;
  public static int FetTypeIsObjectBool = 10019;
  public static int FetTypeBaseIsObjectBool = 10020;
  public static int FetVarInfoAuxIsParamBool = 10021;
  public static int FetVarInfoAuxNullTerminatingBool = 10022;
  public static int FetVarInfoAuxHasNullBool = 10023;
  public static int FetVarInfoAuxHasSizeBool = 10024;
  public static int FetVarInfoAuxHasOrderBool = 10025;
  public static int FetVarInfoAuxHasDuplicatesBool = 10026;
  public static int FetVarIsPrestateBool = 10027;
  public static int FetVarDerivedDepthInt = 10028;

  public static int MaxNumVars = 8;

  public static int OneMoreOrderThanLargestFeature = 100000;

}
