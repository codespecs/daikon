package daikon.inv;
/*********************************************
 * An invariant feature extractor.
 * This class creates a labeling of invariants.
 * That is, it extracts features from invariants and then
 * gives a 1 or a -1 based on which of the two input files
 * the invariant came from.
 * The output goes to standard out and is in the format
 * SVM-Light, SVMfu, or C5 uses.
 *********************************************/

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
    for (int i = 0; i < usefulFeatures.size(); i++) {
      for (Iterator fets = ((TreeSet) usefulFeatures.get(i)).iterator();
           fets.hasNext();) {
        IntDoublePair fet = (IntDoublePair) fets.next();
        allFeatures.add(new IntDoublePair(fet.number, 0));
      }
    }
    for (int i = 0; i < nonusefulFeatures.size(); i++) {
      for (Iterator fets = ((TreeSet) nonusefulFeatures.get(i)).iterator();
           fets.hasNext();) {
        IntDoublePair fet = (IntDoublePair) fets.next();
        allFeatures.add(new IntDoublePair(fet.number, 0));
      }
    }

    // Now make the .names part
    names.write("|Beginning of .names file\n");
    // names.write("GoodBad.\n\nGoodBad: 1, -1.\n");
    names.write("good, bad.\n");
    for (Iterator all = allFeatures.iterator(); all.hasNext(); )
      names.write(((IntDoublePair) all.next()).number + ": continuous.\n");
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
                                           FileWriter output) throws IOException {
    DecimalFormat df = new DecimalFormat("0.0####");
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
      answer.add(new IntDoublePair(FetEnoughSamples, 1));
      answer.add(new IntDoublePair(FetGetProbability, inv.getProbability()));
      if (inv.isExact()) answer.add(new IntDoublePair(FetIsExact, 1));
      if (inv.justified()) answer.add(new IntDoublePair(FetJustified, 1));
      if (inv.isWorthPrinting()) answer.add(new IntDoublePair(FetIsWorthPrinting, 1));
      if (inv.hasFewModifiedSamples()) answer.add(new IntDoublePair(FetHasFewModifiedSamples, 1));
      /* [INCR]
      if (inv.hasNonCanonicalVariable()) answer.add(new IntDoublePair(FetHasNonCanonicalVariable, 1));
      if (inv.hasOnlyConstantVariables()) answer.add(new IntDoublePair(FetHasOnlyConstantVariables, 1));
      */ // [INCR]
      if (inv.isObvious()) answer.add(new IntDoublePair(FetIsObvious, 1));
      if (inv.isObviousDerived()) answer.add(new IntDoublePair(FetIsObviousDerived, 1));
      if (inv.isObviousImplied()) answer.add(new IntDoublePair(FetIsObviousImplied, 1));
      /* [INCR]
      if (inv.isControlled()) answer.add(new IntDoublePair(FetIsControlled, 1));
      if (inv.isImpliedPostcondition()) answer.add(new IntDoublePair(FetIsImpliedPostcondition, 1));
      */ // [INCR]
      if (inv.isInteresting()) answer.add(new IntDoublePair(FetIsInteresting, 1));
      answer.addAll(getPptFeatures(inv.ppt));
    }
    return answer;
  }

  // get the Ppt features
  private static Vector getPptFeatures(PptSlice ppt) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetArity, ppt.arity));
    PptTopLevel pptTop = ppt.parent;
    answer.add(new IntDoublePair(FetNumVars, pptTop.num_vars()));
    answer.add(new IntDoublePair(FetNumArrayVars, pptTop.num_array_vars()));
    /* [INCR]
    if (pptTop.entry_ppt != null)
      answer.add(new IntDoublePair(FetPptIsExit, 1));
    if (pptTop.combined_exit != null)
      answer.add(new IntDoublePair(FetPptIsLineNumberedExit, 1));
    answer.add(new IntDoublePair(FetPptNumOfExits, pptTop.exit_ppts.size()));
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
      //      answer.add(new IntDoublePair(i*10000 + FetVarInfoName, var.name.name().hashCode()));
      if (var.is_static_constant) {
        answer.add(new IntDoublePair(i*10000+FetVarInfoIs_Static_Constant, 1));
        answer.add(new IntDoublePair(FetVarInfoIs_Static_Constant, 1)); }
      /* [INCR]
      if (var.canBeNull) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoCanBeNull, 1));
        answer.add(new IntDoublePair(FetVarInfoIs_Static_Constant, 1)); }
      if (var.is_dynamic_constant) {
        answer.add(new IntDoublePair(i*10000+FetVarInfoIs_Dynamic_Constant,1));
        answer.add(new IntDoublePair(FetVarInfoIs_Dynamic_Constant, 1)); }
      */ // [INCR]
      if (var.isPrestate()) {
        answer.add(new IntDoublePair(i*10000 + FetVarIsPrestate, 1));
        answer.add(new IntDoublePair(FetVarIsPrestate, 1)); }
      answer.add(new IntDoublePair(i * 10000 + FetVarDerivedDepth, var.derivedDepth()));

      VarInfoAux aux = var.aux;
      if (aux.getFlag(VarInfoAux.IS_PARAM)) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoAuxIsParam, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxIsParam, 1)); }
      if (aux.getFlag(VarInfoAux.NULL_TERMINATING)) {
        answer.add(new IntDoublePair(i*10000+FetVarInfoAuxNullTerminating, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxNullTerminating, 1)); }
      if (aux.getFlag(VarInfoAux.HAS_NULL)) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoAuxHasNull, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxHasNull, 1)); }
      if (aux.getFlag(VarInfoAux.HAS_SIZE)) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoAuxHasSize, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxHasSize, 1)); }
      if (aux.getFlag(VarInfoAux.HAS_ORDER)) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoAuxHasOrder, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxHasOrder, 1)); }
      if (aux.getFlag(VarInfoAux.HAS_DUPLICATES)) {
        answer.add(new IntDoublePair(i*10000 + FetVarInfoAuxHasDuplicates, 1));
        answer.add(new IntDoublePair(FetVarInfoAuxHasDuplicates, 1)); }

      ProglangType type = var.type;
      answer.add(new IntDoublePair(i*10000 + FetTypeDimensions, type.dimensions()));
      // delete feature because hash code is not a continuous function
      // answer.add(new IntDoublePair(i*10000 + FetTypeBase, type.base().hashCode()));
      if (type.isArray()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsArray, 1));
        answer.add(new IntDoublePair(FetTypeIsArray, 1)); }
      answer.add(new IntDoublePair(i*10000 + FetTypePseudoDimensions, type.pseudoDimensions()));
      if (type.isPseudoArray()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsPseudoArray, 1));
        answer.add(new IntDoublePair(FetTypeIsPseudoArray, 1)); }
      if (type.isPrimitive()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsPrimitive, 1));
        answer.add(new IntDoublePair(FetTypeIsPrimitive, 1)); }
      if (type.baseIsPrimitive()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeBaseIsArray, 1));
        answer.add(new IntDoublePair(FetTypeBaseIsArray, 1)); }
      if (type.isIntegral()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsIntegral, 1));
        answer.add(new IntDoublePair(FetTypeIsIntegral, 1)); }
      if (type.baseIsIntegral()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeBaseIsIntegral, 1));
        answer.add(new IntDoublePair(FetTypeBaseIsIntegral, 1)); }
      if (type.elementIsIntegral()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeElementIsIntegral, 1));
        answer.add(new IntDoublePair(FetTypeElementIsIntegral, 1)); }
      if (type.isScalar()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsScalar, 1));
        answer.add(new IntDoublePair(FetTypeIsScalar, 1)); }
      if (type.isFloat()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsFloat, 1));
        answer.add(new IntDoublePair(FetTypeIsFloat, 1)); }
      if (type.baseIsFloat()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeBaseIsFloat, 1));
        answer.add(new IntDoublePair(FetTypeBaseIsFloat, 1)); }
      if (type.isObject()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeIsObject, 1));
        answer.add(new IntDoublePair(FetTypeIsObject, 1)); }
      if (type.baseIsObject()) {
        answer.add(new IntDoublePair(i*10000 + FetTypeBaseIsObject, 1));
        answer.add(new IntDoublePair(FetTypeBaseIsObject, 1)); }
    }

 return answer;
  }

  private static Vector getModulusFeatures(Modulus inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetModulus, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getLowerBoundFeatures(LowerBound inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLowerBound, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getLowerBoundCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getLowerBoundFloatFeatures(LowerBoundFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLowerBoundFloat, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getLowerBoundCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getNonZeroFeatures(NonZero inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNonZero, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getNonZeroFloatFeatures(NonZeroFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNonZeroFloat, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getNonModulusFeatures(NonModulus inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNonModulus, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getOneOfScalarFeatures(OneOfScalar inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfScalar, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getPositiveFeatures(Positive inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPositive, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getSingleFloatFeatures(SingleFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleFloat, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getSingleScalarFeatures(SingleScalar inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleScalar, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getUpperBoundFeatures(UpperBound inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetUpperBound, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getUpperBoundCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getUpperBoundFloatFeatures(UpperBoundFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetUpperBoundFloat, 1));
    answer.add(new IntDoublePair(FetScalar, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getUpperBoundCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getUpperBoundCoreFeatures(UpperBoundCore core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetUpperBoundCoreMax1, core.max1));
    return answer;
  }

  private static Vector getUpperBoundCoreFloatFeatures(UpperBoundCoreFloat core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetUpperBoundCoreFloatMax1, core.max1));
    return answer;
  }

  private static Vector getLowerBoundCoreFeatures(LowerBoundCore core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetLowerBoundCoreMin1, core.min1));
    return answer;
  }

  private static Vector getLowerBoundCoreFloatFeatures(LowerBoundCoreFloat core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetLowerBoundCoreFloatMin1, core.min1));
    return answer;
  }

  private static Vector getEltLowerBoundFeatures(EltLowerBound inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltLowerBound, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getLowerBoundCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getEltLowerBoundFloatFeatures(EltLowerBoundFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltLowerBoundFloat, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getLowerBoundCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getSequenceFloatFeatures(SequenceFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSequenceFloat, 1));
    return answer;
  }

  private static Vector getMemberFloatFeatures(MemberFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetMemberFloat, 1));
    return answer;
  }

  private static Vector getCommonFloatSequenceFeatures(CommonFloatSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetCommonFloatSequence, 1));
    return answer;
  }

  private static Vector getEltNonZeroFeatures(EltNonZero inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltNonZero, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getEltNonZeroFloatFeatures(EltNonZeroFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltNonZeroFloat, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getEltOneOfFeatures(EltOneOf inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltOneOf, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getEltOneOfFloatFeatures(EltOneOfFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltOneOfFloat, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getEltUpperBoundFeatures(EltUpperBound inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltUpperBound, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getUpperBoundCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getEltUpperBoundFloatFeatures(EltUpperBoundFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltUpperBoundFloat, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getUpperBoundCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getEltwiseIntComparisonFeatures(EltwiseIntComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltwiseIntComparison, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getIntComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getEltwiseFloatComparisonFeatures(EltwiseFloatComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltwiseFloatComparison, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getFloatComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getIntComparisonCoreFeatures(IntComparisonCore core) {
    Vector answer = new Vector();
    if (core.can_be_eq)
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_Eq, 1));
    if (core.can_be_lt)
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_Lt, 1));
    if (core.can_be_gt)
      answer.add(new IntDoublePair(FetIntComparisonCoreCan_Be_Gt, 1));
    return answer;
  }

  private static Vector getFloatComparisonCoreFeatures(FloatComparisonCore core) {
    Vector answer = new Vector();
    if (core.can_be_eq)
      answer.add(new IntDoublePair(FetFloatComparisonCoreCan_Be_Eq, 1));
    if (core.can_be_lt)
      answer.add(new IntDoublePair(FetFloatComparisonCoreCan_Be_Lt, 1));
    if (core.can_be_gt)
      answer.add(new IntDoublePair(FetFloatComparisonCoreCan_Be_Gt, 1));
    return answer;
  }

  private static Vector getNoDuplicatesFeatures(NoDuplicates inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNoDuplicates, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getNoDuplicatesFloatFeatures(NoDuplicatesFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNoDuplicatesFloat, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getOneOfSequenceFeatures(OneOfSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfSequence, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getOneOfFloatSequenceFeatures(OneOfFloatSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfFloatSequence, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getSeqIndexComparisonFeatures(SeqIndexComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqIndexComparison, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getIntComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSeqIndexComparisonFloatFeatures(SeqIndexComparisonFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqIndexComparisonFloat, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getFloatComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSeqIndexNonEqualFeatures(SeqIndexNonEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqIndexNonEqual, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getNonEqualCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSeqIndexNonEqualFloatFeatures(SeqIndexNonEqualFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqIndexNonEqualFloat, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getNonEqualCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getNonEqualCoreFeatures(NonEqualCore core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetNonEqualCoreMin1, core.min1));
    //    answer.add(new IntDoublePair(FetNonEqualCoreMin2, core.min2));
    //    answer.add(new IntDoublePair(FetNonEqualCoreMax1, core.max1));
    //    answer.add(new IntDoublePair(FetNonEqualCoreMax2, core.max2));
    return answer;
  }

  private static Vector getNonEqualCoreFloatFeatures(NonEqualCoreFloat core) {
    Vector answer = new Vector();
    return answer;
  }

  private static Vector getSingleFloatSequenceFeatures(SingleFloatSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleFloatSequence, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getSingleSequenceFeatures(SingleSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleSequence, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getOneOfStringFeatures(OneOfString inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfString, 1));
    answer.add(new IntDoublePair(FetString, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getSingleStringFeatures(SingleString inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleString, 1));
    answer.add(new IntDoublePair(FetString, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    return answer;
  }

  private static Vector getEltOneOfStringFeatures(EltOneOfString inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfString, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.add(new IntDoublePair(FetStringSequence, 1));
    return answer;
  }

  private static Vector getOneOfStringSequenceFeatures(OneOfStringSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfStringSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.add(new IntDoublePair(FetStringSequence, 1));
    return answer;
  }

  private static Vector getSingleStringSequenceFeatures(SingleStringSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSingleStringSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.add(new IntDoublePair(FetStringSequence, 1));
    return answer;
  }

  private static Vector getOneOfFeatures(OneOf inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfNum_elts, inv.num_elts()));
    return answer;
  }

  private static Vector getOneOfFloatFeatures(OneOfFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetOneOfFloatNum_elts, inv.num_elts()));
    return answer;
  }

  private static Vector getComparisonFeatures(Comparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetComparison, 1));
    answer.add(new IntDoublePair(FetComparisonEq_probability, inv.eq_probability()));
    return answer;
  }

  private static Vector getImplicationFeatures(Implication inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetImplication, 1));
    if (inv.iff) answer.add(new IntDoublePair(FetImplicationIff, 1));
    return answer;
  }

  private static Vector getSeqIntComparisonFeatures(SeqIntComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqIntComparison, 1));
    answer.add(new IntDoublePair(FetSequenceScalar, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.addAll(getIntComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSeqFloatComparisonFeatures(SeqFloatComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqFloatComparison, 1));
    answer.add(new IntDoublePair(FetSequenceScalar, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.addAll(getFloatComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSequenceScalarFeatures(SequenceScalar inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSequenceScalar, 1));
    answer.add(new IntDoublePair(FetSequenceScalar, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    if (inv.seq_first) answer.add(new IntDoublePair(FetSequenceScalarSeq_first, 1));
    if (inv.seq_index==1) answer.add(new IntDoublePair(FetSequenceScalarSeq_index, 1));
    if (inv.scl_index==1) answer.add(new IntDoublePair(FetSequenceScalarScl_index, 1));
    return answer;
  }

  private static Vector getSequenceStringFeatures(SequenceString inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSequenceString, 1));
    answer.add(new IntDoublePair(FetSequenceString, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    if (inv.seq_first) answer.add(new IntDoublePair(FetSequenceStringSeq_first, 1));
    if (inv.seq_index==1) answer.add(new IntDoublePair(FetSequenceStringSeq_index, 1));
    if (inv.scl_index==1) answer.add(new IntDoublePair(FetSequenceStringScl_index, 1));
    return answer;
  }

  private static Vector getIntNonEqualFeatures(IntNonEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntNonEqual, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  private static Vector getFloatNonEqualFeatures(FloatNonEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatNonEqual, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  private static Vector getIntEqualFeatures(IntEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntEqual, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  private static Vector getFloatEqualFeatures(FloatEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatEqual, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  private static Vector getFunctionUnaryFeatures(FunctionUnary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionUnary, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    answer.addAll(getFunctionUnaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getFunctionUnaryFloatFeatures(FunctionUnaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionUnaryFloat, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    answer.addAll(getFunctionUnaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getFunctionUnaryCoreFeatures(FunctionUnaryCore core) {
    Vector answer = new Vector();
    if (core.inverse)
      answer.add(new IntDoublePair(FetFunctionUnaryCoreInverse, 1));
    return answer;
  }

  private static Vector getFunctionUnaryCoreFloatFeatures(FunctionUnaryCoreFloat core) {
    Vector answer = new Vector();
    if (core.inverse)
      answer.add(new IntDoublePair(FetFunctionUnaryCoreFloatInverse, 1));
    return answer;
  }

  private static Vector getIntGreaterEqualFeatures(IntGreaterEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntGreaterEqual, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  private static Vector getFloatGreaterEqualFeatures(FloatGreaterEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatGreaterEqual, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  private static Vector getIntGreaterThanFeatures(IntGreaterThan inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntGreaterThan, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  private static Vector getFloatGreaterThanFeatures(FloatGreaterThan inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatGreaterThan, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  private static Vector getLinearBinaryFeatures(LinearBinary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearBinary, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    answer.addAll(getLinearBinaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getLinearBinaryFloatFeatures(LinearBinaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearBinaryFloat, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    answer.addAll(getLinearBinaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getLinearBinaryCoreFeatures(LinearBinaryCore core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearBinaryCoreA, core.a));
    answer.add(new IntDoublePair(FetLinearBinaryCoreB, core.b));
    return answer;
  }

  private static Vector getLinearBinaryCoreFloatFeatures(LinearBinaryCoreFloat core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearBinaryCoreFloatA, core.a));
    answer.add(new IntDoublePair(FetLinearBinaryCoreFloatB, core.b));
    return answer;
  }

  private static Vector getTwoFloatFeatures(TwoFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetTwoFloat, 1));
    return answer;
  }

  private static Vector getTwoScalarFeatures(TwoScalar inv) {
    Vector answer = new Vector();
    return answer;
  }

  private static Vector getIntLessEqualFeatures(IntLessEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntLessEqual, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  private static Vector getFloatLessEqualFeatures(FloatLessEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatLessEqual, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  private static Vector getIntLessThanFeatures(IntLessThan inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntLessThan, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  private static Vector getFloatLessThanFeatures(FloatLessThan inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFloatLessThan, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    return answer;
  }

  /*  private static Vector getIntComparisonFeatures(IntComparisons inv) {
      Vector answer = new Vector();
      answer.add(new IntDoublePair(FetIntComparison, 1));
      answer.add(new IntDoublePair(FetBinary, 1));
      answer.add(new IntDoublePair(FetTwoScalar, 1));
      return answer;
      } */

  private static Vector getPairwiseIntComparisonFeatures(PairwiseIntComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseIntComparison, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    answer.addAll(getIntComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getPairwiseFloatComparisonFeatures(PairwiseFloatComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseFloatComparison, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    answer.addAll(getFloatComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getSeqComparisonFeatures(SeqComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqComparison, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    return answer;
  }

  private static Vector getSeqComparisonFloatFeatures(SeqComparisonFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqFloatComparison, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    return answer;
  }

  private static Vector getTwoSequenceFeatures(TwoSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    return answer;
  }

  private static Vector getTwoSequenceFloatFeatures(TwoSequenceFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetTwoSequenceFloat, 1));
    return answer;
  }

  private static Vector getPairwiseLinearBinaryFeatures(PairwiseLinearBinary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseLinearBinary, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    answer.addAll(getLinearBinaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getPairwiseLinearBinaryFloatFeatures(PairwiseLinearBinaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseLinearBinaryFloat, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    answer.addAll(getLinearBinaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getSubSequenceFeatures(SubSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSubSequence, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    return answer;
  }

  private static Vector getSubSequenceFloatFeatures(SubSequenceFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSubSequenceFloat, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    return answer;
  }

  private static Vector getPairwiseFunctionUnaryFeatures(PairwiseFunctionUnary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseFunctionUnary, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    answer.addAll(getFunctionUnaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getPairwiseFunctionUnaryFloatFeatures(PairwiseFunctionUnaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetPairwiseFunctionUnaryFloat, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    answer.addAll(getFunctionUnaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getReverseFeatures(Reverse inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetReverse, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    return answer;
  }

  private static Vector getReverseFloatFeatures(ReverseFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetReverseFloat, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    return answer;
  }

  private static Vector getStringComparisonFeatures(StringComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetStringComparison, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoString, 1));
    answer.addAll(getStringComparisonCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getStringComparisonCoreFeatures(StringComparisonCore core) {
    Vector answer = new Vector();
    if (core.can_be_eq)
       answer.add(new IntDoublePair(FetStringComparisonCoreCan_Be_Eq, 1));
    if (core.can_be_lt)
       answer.add(new IntDoublePair(FetStringComparisonCoreCan_Be_Lt, 1));
    if (core.can_be_gt)
       answer.add(new IntDoublePair(FetStringComparisonCoreCan_Be_Gt, 1));
    return answer;
  }

  private static Vector getTwoStringFeatures(TwoString inv) {
    Vector answer = new Vector();
    return answer;
  }

  private static Vector getThreeScalarFeatures(ThreeScalar inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetThreeScalar, 1));
    return answer;
  }

  private static Vector getThreeFloatFeatures(ThreeFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetThreeFloat, 1));
    return answer;
  }

  private static Vector getLinearTernaryFeatures(LinearTernary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearTernary, 1));
    answer.add(new IntDoublePair(FetTernary, 1));
    answer.add(new IntDoublePair(FetThreeScalar, 1));
    answer.addAll(getLinearTernaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getLinearTernaryFloatFeatures(LinearTernaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearTernaryFloat, 1));
    answer.add(new IntDoublePair(FetTernary, 1));
    answer.add(new IntDoublePair(FetThreeScalar, 1));
    answer.addAll(getLinearTernaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getLinearTernaryCoreFeatures(LinearTernaryCore core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearTernaryCoreA, core.a));
    answer.add(new IntDoublePair(FetLinearTernaryCoreB, core.b));
    answer.add(new IntDoublePair(FetLinearTernaryCoreC, core.c));
    return answer;
  }

  private static Vector getLinearTernaryCoreFloatFeatures(LinearTernaryCoreFloat core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearTernaryCoreFloatA, core.a));
    answer.add(new IntDoublePair(FetLinearTernaryCoreFloatB, core.b));
    answer.add(new IntDoublePair(FetLinearTernaryCoreFloatC, core.c));
    return answer;
  }

  private static Vector getFunctionBinaryFeatures(FunctionBinary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionBinary, 1));
    answer.add(new IntDoublePair(FetTernary, 1));
    answer.add(new IntDoublePair(FetThreeScalar, 1));
    answer.addAll(getFunctionBinaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getFunctionBinaryFloatFeatures(FunctionBinaryFloat inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionBinaryFloat, 1));
    answer.add(new IntDoublePair(FetTernary, 1));
    answer.add(new IntDoublePair(FetThreeScalar, 1));
    answer.addAll(getFunctionBinaryCoreFloatFeatures(inv.core));
    return answer;
  }

  private static Vector getFunctionBinaryCoreFeatures(FunctionBinaryCore core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionBinaryCoreVar_Order, core.var_order));
    return answer;
  }

  private static Vector getFunctionBinaryCoreFloatFeatures(FunctionBinaryCoreFloat core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionBinaryCoreFloatVar_Order, core.var_order));
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
   * A tool for combining multiple TrainFu files.
   *********************************************/

  public static final class CombineFuFiles {

    private static String USAGE =
      "\tArguments:\n\t-i FileName:\ta TrainFu input file\n" +
      "\t-o FileName:\toutput file name\n" +
      "\t[-r] repeat:\tif present then the number of positive and negative\n" +
      "\t\tvectors will be roughtly normalized (by repeats).\n";

    static public void main(String[] args)
      throws IOException, ClassNotFoundException {

      // First parse the arguments
      if (args.length == 0) {
        System.out.println(USAGE);
        System.exit(0);
      }
      Vector inputs = new Vector();
      boolean repeats = false;
      String output = null;
      for (int i = 0; i < args.length; i++) {
        if (args[i].equals("-r"))
          repeats = true;
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

          int posind = vector.lastIndexOf("1");
          int negind = vector.lastIndexOf("-1");

          if (negind == posind - 1)
            neg.add(vector.substring(0, vector.lastIndexOf("-1")));
          else
            pos.add(vector.substring(0, vector.lastIndexOf("1")));
        }
        br.close();
      }

      // Now create two vectors, posvectors and negvectors, of the
      // positive and negative TrainFu vectors respectively.
      Vector posvectors = new Vector();
      Vector negvectors = new Vector();

      for (Iterator i = neg.iterator(); i.hasNext(); ) {
        String vector = (String) i.next();
        if (!(pos.contains(vector)))
          negvectors.add(vector + "-1");
      }

      for (Iterator i = pos.iterator(); i.hasNext(); )
        posvectors.add(((String) i.next()) + "1");

      // Set the appropriate repeat values.
      int posrepeat = 1 , negrepeat = 1;
      if (repeats)
        if (posvectors.size() > negvectors.size())
          negrepeat = posvectors.size() / negvectors.size();
        else
          posrepeat = negvectors.size() / posvectors.size();

      // Print the output to the output file.
      FileWriter fw = new FileWriter(output);
      fw.write((posvectors.size() * posrepeat +
                negvectors.size() * negrepeat) + " \n");

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
   * A tool for normalizing a C5 file.
   *********************************************/

  public static final class NormalizeC5Files {

    private static String USAGE =
      "\tArguments:\n\t-i FileName:\ta C5 input file root\n";

    static public void main(String[] args)
      throws IOException, ClassNotFoundException {

      // First parse the arguments
      if (args.length != 2) {
        System.out.println(USAGE);
        System.exit(0);
      }
      String input = new String();

      for (int i = 0; i < args.length; i++) {
        if (args[i].equals("-i"))
          input = args[++i] + ".data";
        else
          throw new IOException("Invalid argument: " + args[i]);
      }

      // Load the input files into 2 HashSets, pos and neg.
      HashSet pos = new HashSet();
      HashSet neg = new HashSet();
      BufferedReader br = new BufferedReader(new FileReader(input));
      br.readLine();
      while (br.ready()) {
        String vector = br.readLine();

        boolean ispos = (vector.indexOf("good") > -1);
        if (ispos == (vector.indexOf("bad") > -1))
          throw new IOException("Invalid input data in " + input);

        if (ispos)
          pos.add(vector.substring(0, vector.lastIndexOf("good")));
        else
          neg.add(vector.substring(0, vector.lastIndexOf("bad")));
      }
      br.close();

      // Now create two vectors, posvectors and negvectors, of the
      // positive and negative TrainFu vectors respectively.
      Vector posvectors = new Vector();
      Vector negvectors = new Vector();

      for (Iterator i = neg.iterator(); i.hasNext(); ) {
        String vector = (String) i.next();
        if (!(pos.contains(vector)))
          negvectors.add(vector + "bad");
      }

      for (Iterator i = pos.iterator(); i.hasNext(); )
        posvectors.add(((String) i.next()) + "good");

      // Set the appropriate repeat values.
      int posrepeat = 1 , negrepeat = 1;
      if (posvectors.size() > negvectors.size())
        negrepeat = posvectors.size() / negvectors.size();
      else
        posrepeat = negvectors.size() / posvectors.size();

      // Print the output to the output file.
      FileWriter fw = new FileWriter(input);
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
   * A tool for permuting TrainFu files.
   *********************************************/

  public static final class PermuteFuFiles {

    private static String USAGE =
      "\tArguments:\n\t-i FileName:\ta TrainFu input file\n" +
      "\t-o FileName:\toutput file name\n";

    static public void main(String[] args)
      throws IOException, ClassNotFoundException {

      // First parse the arguments
      if (args.length == 0) {
        System.out.println(USAGE);
        System.exit(0);
      }
      String input = null;
      String output = null;
      for (int i = 0; i < args.length; i++) {
        if (args[i].equals("-i")) {
          if (input == null)
            input = args[++i];
          else
            throw new IOException("Multiple input files not allowed");
        }
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
      if (output == null)
        throw new IOException("You must specify an output file");
      if (input == null)
        throw new IOException("You must specify an input file");

      // Load the input file into 2 Vectors, pos and neg.
      Vector pos = new Vector();
      Vector neg = new Vector();
      BufferedReader br = new BufferedReader(new FileReader(input));
      br.readLine();
      while (br.ready()) {
        String vector = br.readLine();

        int posind = vector.lastIndexOf("1");
        int negind = vector.lastIndexOf("-1");

        if (negind == posind - 1)
          neg.add(vector.substring(0, vector.lastIndexOf("-1")));
        else
          pos.add(vector.substring(0, vector.lastIndexOf("1")));
      }
      br.close();

      // Set up output file
      FileWriter fw = new FileWriter(output);
      fw.write((int) Math.pow(pos.size() + neg.size(), 2) + "\n");

      writeVectors(pos, pos, "1", fw);
      writeVectors(pos, neg, "1", fw);
      writeVectors(neg, pos, "1", fw);
      writeVectors(neg, neg, "-1", fw);
      fw.close();
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


  }

  // the following line gets rid of some extra output that
  // otherwise gets dumped to System.out:
  static {
    LogHelper.setupLogs(false ? LogHelper.DEBUG : LogHelper.INFO);
  }

  // the THRESHOLD is zero
  static double THRESHOLD = 0.0;

  // A bunch of static variables, one for each feature
  static int FetEnoughSamples = 1;
  static int FetGetProbability = 2;
  static int FetIsExact = 3;
  static int FetJustified = 4;
  static int FetIsWorthPrinting = 5;
  static int FetHasFewModifiedSamples = 6;
  static int FetHasNonCanonicalVariable = 7;
  static int FetHasOnlyConstantVariables = 8;
  static int FetIsObvious = 9;
  static int FetIsObviousDerived = 10;
  static int FetIsObviousImplied = 11;
  static int FetIsControlled = 12;
  static int FetIsImpliedPostcondition = 13;
  static int FetIsInteresting = 14;
  static int FetArity = 15;
  static int FetNumVars = 16;
  static int FetNumArrayVars = 17;
  static int FetOneOfNum_elts = 50;
  static int FetComparison = 51;
  static int FetComparisonEq_probability = 52;
  static int FetImplication = 53;
  static int FetImplicationIff = 54;
  static int FetUnary = 81;
  static int FetScalar = 82;
  static int FetSequence = 83;
  static int FetString = 84;
  static int FetStringSequence = 85;
  static int FetBinary = 86;
  static int FetTwoScalar = 88;
  static int FetTwoSequence = 89;
  static int FetTwoString = 90;
  static int FetTernary = 91;
  static int FetThreeScalar = 92;
  static int FetModulus = 100;
  static int FetLowerBound = 200;
  static int FetLowerBoundCoreMin1 = 201;
  static int FetNonZero = 300;
  static int FetNonModulus = 400;
  static int FetOneOfScalar = 500;
  static int FetPositive = 600;
  static int FetSingleFloat = 700;
  static int FetSingleScalar = 800;
  static int FetUpperBound = 900;
  static int FetUpperBoundCoreMax1 = 901;
  static int FetEltLowerBound = 1000;
  static int FetEltNonZero = 1100;
  static int FetEltOneOf = 1200;
  static int FetEltUpperBound = 1300;
  static int FetEltwiseIntComparison = 1400;
  static int FetNoDuplicates = 1500;
  static int FetOneOfSequence = 1600;
  static int FetSeqIndexComparison = 1700;
  static int FetSeqIndexNonEqual = 1800;
  static int FetSingleFloatSequence = 1900;
  static int FetSingleSequence = 2000;
  static int FetOneOfString = 2200;
  static int FetSingleString = 2300;
  static int FetEltOneOfString = 2400;
  static int FetOneOfStringSequence = 2500;
  static int FetSingleStringSequence = 2600;
  static int FetSeqIntComparison = 2800;
  static int FetSequenceScalar = 2900;
  static int FetSequenceScalarSeq_first = 2901;
  static int FetSequenceScalarSeq_index = 2902;
  static int FetSequenceScalarScl_index = 2903;
  static int FetSequenceString = 3000;
  static int FetSequenceStringSeq_first = 3001;
  static int FetSequenceStringSeq_index = 3002;
  static int FetSequenceStringScl_index = 3003;
  static int FetIntNonEqual = 3100;
  static int FetIntEqual = 3200;
  static int FetNonEqualCoreMin1 = 3301;
  static int FetNonEqualCoreMin2 = 3302;
  static int FetNonEqualCoreMax1 = 3303;
  static int FetNonEqualCoreMax2 = 3304;
  static int FetFunctionUnary = 3400;
  static int FetFunctionUnaryCoreInverse = 3401;
  static int FetIntGreaterEqual = 3500;
  static int FetIntGreaterThan = 3600;
  static int FetLinearBinary = 3700;
  static int FetLinearBinaryCoreA = 3701;
  static int FetLinearBinaryCoreB = 3702;
  static int FetIntLessEqual = 3800;
  static int FetIntLessThan = 3900;
  static int FetIntComparisonCoreCan_Be_Eq = 4001;
  static int FetIntComparisonCoreCan_Be_Lt = 4002;
  static int FetIntComparisonCoreCan_Be_Gt = 4003;
  static int FetPairwiseIntComparison = 4800;
  static int FetSeqComparison = 4900;
  static int FetPairwiseLinearBinary = 5000;
  static int FetSubSequence = 5100;
  static int FetPairwiseFunctionUnary = 5200;
  static int FetReverse = 5300;
  static int FetStringComparison = 5500;
  static int FetStringComparisonCoreCan_Be_Eq = 5501;
  static int FetStringComparisonCoreCan_Be_Lt = 5502;
  static int FetStringComparisonCoreCan_Be_Gt = 5503;
  static int FetLinearTernary = 5700;
  static int FetLinearTernaryCoreA = 5701;
  static int FetLinearTernaryCoreB = 5702;
  static int FetLinearTernaryCoreC = 5703;
  static int FetFunctionBinary = 5800;
  static int FetFunctionBinaryCoreVar_Order = 5801;
  static int FetCommonFloatSequence = 5900;
  static int FetEltLowerBoundFloat = 6000;
  static int FetEltNonZeroFloat = 6100;
  static int FetEltOneOfFloat = 6200;
  static int FetEltUpperBoundFloat = 6300;
  static int FetEltwiseFloatComparison = 6400;
  static int FetFloatEqual = 6500;
  static int FetFloatGreaterEqual = 6600;
  static int FetFloatGreaterThan = 6700;
  static int FetFloatLessEqual = 6800;
  static int FetFloatLessThan = 6900;
  static int FetFloatNonEqual = 7000;
  static int FetFunctionBinaryFloat = 7100;
  static int FetFunctionUnaryFloat = 7200;
  static int FetLinearBinaryFloat = 7300;
  static int FetLinearTernaryFloat = 7400;
  static int FetLowerBoundFloat = 7500;
  static int FetMemberFloat = 7600;
  static int FetNoDuplicatesFloat = 7700;
  static int FetNonZeroFloat = 7800;
  static int FetOneOfFloat = 7900;
  static int FetOneOfFloatNum_elts = 7901;
  static int FetOneOfFloatSequence = 8000;
  static int FetPairwiseFloatComparison = 8100;
  static int FetPairwiseFunctionUnaryFloat = 8200;
  static int FetPairwiseLinearBinaryFloat = 8300;
  static int FetReverseFloat = 8400;
  static int FetSeqComparisonFloat = 8500;
  static int FetSeqFloatComparison = 8600;
  static int FetSeqIndexComparisonFloat = 8700;
  static int FetSeqIndexNonEqualFloat = 8800;
  static int FetSequenceFloat = 8900;
  static int FetSubSequenceFloat = 9200;
  static int FetThreeFloat = 9300;
  static int FetTwoFloat = 9400;
  static int FetTwoSequenceFloat = 9500;
  static int FetUpperBoundFloat = 9600;
  static int FetFunctionUnaryCoreFloatInverse = 9701;
  static int FetFunctionBinaryCoreFloatVar_Order = 9702;
  static int FetLinearBinaryCoreFloatA = 9703;
  static int FetLinearBinaryCoreFloatB = 9704;
  static int FetLinearTernaryCoreFloatA = 9703;
  static int FetLinearTernaryCoreFloatB = 9705;
  static int FetLinearTernaryCoreFloatC = 9706;
  static int FetFloatComparisonCoreCan_Be_Eq = 9707;
  static int FetFloatComparisonCoreCan_Be_Lt = 9708;
  static int FetFloatComparisonCoreCan_Be_Gt = 9709;

  // 9800-9900 reserved for Ppt Features
  static int FetPptIsExit = 9801;
  static int FetPptIsLineNumberedExit = 9802;
  static int FetPptNumOfExits = 9803;

  // Variable Features (10000 - 49999)
  // 10000-19999 are for "invariant contains a variable that ...
  // 20000 - 29999 are for 1st variable is .... (etc. up to 3 variables)
  //  static int FetVarInfoName = 10001;
  static int FetVarInfoIs_Static_Constant = 10002;
  static int FetVarInfoCanBeNull = 10003;
  static int FetVarInfoIs_Dynamic_Constant =  10004;
  static int FetTypeDimensions = 10005;
  static int FetTypeIsArray = 10007;
  static int FetTypeBaseIsArray = 10008;
  static int FetTypePseudoDimensions = 10009;
  static int FetTypeIsPseudoArray = 10010;
  static int FetTypeIsPrimitive = 10011;
  static int FetTypeBaseIsPrimitive = 10012;
  static int FetTypeIsIntegral = 10013;
  static int FetTypeBaseIsIntegral = 10014;
  static int FetTypeElementIsIntegral = 10015;
  static int FetTypeIsScalar = 10016;
  static int FetTypeIsFloat = 10017;
  static int FetTypeBaseIsFloat = 10018;
  static int FetTypeIsObject = 10019;
  static int FetTypeBaseIsObject = 10020;
  static int FetVarInfoAuxIsParam = 10021;
  static int FetVarInfoAuxNullTerminating = 10022;
  static int FetVarInfoAuxHasNull = 10023;
  static int FetVarInfoAuxHasSize = 10024;
  static int FetVarInfoAuxHasOrder = 10025;
  static int FetVarInfoAuxHasDuplicates = 10026;
  static int FetVarIsPrestate = 10027;
  static int FetVarDerivedDepth = 10028;

  static int OneMoreOrderThanLargestFeature = 100000;

}
