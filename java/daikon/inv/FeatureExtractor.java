/*********************************************
 * An invariant feature extractor.
 * This class creates a labeling of invariants.
 * That is, it extracts features from invariants and then
 * gives a 1 or a -1 based on which of the two input files
 * the invariant came from.
 * The output goes to standard out and is in the format
 * SVM-Light uses.
 *
 * Created by Yuriy Brun, 5/12/2002
 *********************************************/

import java.io.*;
import java.util.*;
import java.text.*;
import daikon.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;
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


public class FeatureExtractor {

  // the following line gets rid of some extra output that
  // otherwise gets dumped to System.out:
  static {
    Logger.setupLogs(false ? Logger.DEBUG : Logger.INFO);
  }

  //the THRESHOLD for the largest number that should not be considered 0
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
  //  static int FetNonEqual = 3300; removed invariant
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
  //  static int FetIntComparison = 4000; removed invariant
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
  //Variable Features (10000 - 49999)
  //10000-19999 are for "invariant contains a variable that ...
  ///20000 - 29999 are for 1st variable is .... (etc. up to 3 variables)
  //  static int FetVarInfoName = 10001;
  static int FetVarInfoIs_Static_Constant = 10002;
  static int FetVarInfoCanBeNull = 10003;
  static int FetVarInfoIs_Dynamic_Constant =  10004;
  static int FetTypeDimensions = 10005;
  //  static int FetTypeBase = 10006;
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



  /* Main reads the input files, extracts features and then
     outputs the labeling in SVM-Light format.

     Main takes an odd number of arguments, "SVM" or "C5" and then
     Pairs of .inv files.
     So arguments look like this:
     SVM Good.inv Bad.inv Good.inv Bad.inb Good.inv Bad.inv ...
  */
  static public void main(String[] args) throws IOException{
    // Main performs 3 steps:
    // 1)  make two vectors of invariants: useful and nonuseful
    // 2)  extract the features for useful and nonuseful
    // 3)  print in proper format (based on args[0]) the labeling

    //Step 1
    List fileNames = Arrays.asList(args);
    fileNames.remove(0);
    Vector[] allInvariants = getGoodAndBad((String[]) fileNames.toArray());
    Vector useful = allInvariants[0];
    Vector nonuseful = allInvariants[1];

    //Step 2
    //features will have one element for each invariant.
    //each element will be a TreeSet of IntDoublePairs
    Vector usefulFeatures = getFeatures(useful);
    Vector nonusefulFeatures = getFeatures(nonuseful);

    //Step 3
    String output = new String();
    if (args[0].indexOf("SVM") > -1)
      output = printSVMOutput(usefulFeatures, nonusefulFeatures);
    else if (args[0].indexOf("C5") > -1)
      output = printC5Output(usefulFeatures, nonusefulFeatures);
    else
      System.err.println("Invalid First Argument: " + args[0]);

    System.out.print(output);
  }

  private static Vector[] getGoodAndBad(String[] args) throws IOException{
    // the args are pairs of files such each pair
    // consists of a Bad.inv and Good.inv
    // Note, Good.inv contains invariants based on non-fault revealing
    // test cases, and Bad.inv contains invariants based on fault revealing
    // ones only.

    // returns two Vectors (in an array) of Useful invariants and
    //non-useful invariants
    Vector[] answer = new Vector[2];
    answer[0] = new Vector();
    answer[1] = new Vector();

    for (int i = 0; i < args.length; i+=2) {
      //good contains string reps of invariants in Good.inv
      Vector good = new Vector();
      Iterator goodppts =
        FileIO.read_serialized_pptmap(new File(args[i]), false).iterator();
      while(goodppts.hasNext()) {
        List temp = ((PptTopLevel) goodppts.next()).getInvariants();
        for (int j = 0; j < temp.size(); j++)
          good.add(((Invariant) temp.get(j)).format_using(OutputFormat.JAVA));
      }

      //bad contains actual invariants in Bad.inv
      Vector bad = new Vector();
      Iterator badppts =
        FileIO.read_serialized_pptmap(new File(args[i+1]), false).iterator();
      while(badppts.hasNext()) {
        List temp = ((PptTopLevel) badppts.next()).getInvariants();
        for (int j = 0; j < temp.size(); j++)
          bad.add((Invariant) temp.get(j));
      }

      for (int j = 0; j < bad.size(); j++) {
        if (good.contains(((Invariant) bad.get(j)).format_using(OutputFormat.JAVA)))
          answer[1].add(bad.get(j));
        else
          answer[0].add(bad.get(j));
      }
    }
    return answer;
  }

  private static String printC5Output(Vector usefulFeatures,
                                      Vector nonusefulFeatures) {
    String answer = new String();
    //First create a TreeSet of all the Feature Numbers and 0 as value
    TreeSet allFeatures =  new TreeSet();
    for (int i = 0; i < usefulFeatures.size(); i++) {
      Iterator fets = ((TreeSet) usefulFeatures.get(i)).iterator();
      while(fets.hasNext()) {
        IntDoublePair fet = (IntDoublePair) fets.next();
        allFeatures.add(new IntDoublePair(fet.number, 0));
      }
    }
    for (int i = 0; i < nonusefulFeatures.size(); i++) {
      Iterator fets = ((TreeSet) nonusefulFeatures.get(i)).iterator();
      while(fets.hasNext()) {
        IntDoublePair fet = (IntDoublePair) fets.next();
        allFeatures.add(new IntDoublePair(fet.number, 0));
      }
    }


    //Now make the .names part
    answer += "|Beginning of .names file\n";
    Iterator all = allFeatures.iterator();
    answer += "GoodBad.\n\nGoodBad: 1, -1.\n";
    while (all.hasNext())
      answer += ((IntDoublePair) all.next()).number + ": continuous.\n";
    answer += "useless: ignore.\n";
    answer += "|End of .names file\n\n\n";

    //Now for each invariant, print out the features C5.0 style
    //first useful
    for (int i = 0; i < usefulFeatures.size(); i++) {
      TreeSet allFets = ((TreeSet) usefulFeatures.get(i));
      allFets.addAll(allFeatures); //Add a 0 feature if it is not present

      Iterator fets = allFets.iterator();
      answer += "1,";
      DecimalFormat df = new DecimalFormat("0.0####");
      while(fets.hasNext()) {
        IntDoublePair fet = (IntDoublePair) fets.next();
        answer += df.format(fet.value) + ",";
      }
      answer += "N/A\n";
    }
    //and now non useful
    for (int i = 0; i < nonusefulFeatures.size(); i++) {
      TreeSet allFets = ((TreeSet) nonusefulFeatures.get(i));
      allFets.addAll(allFeatures); //Add a 0 feature if it is not present

      Iterator fets = allFets.iterator();
      answer += "-1,";
      DecimalFormat df = new DecimalFormat("0.0####");
      while(fets.hasNext()) {
        IntDoublePair fet = (IntDoublePair) fets.next();
        answer += df.format(fet.value) + ",";
      }
      answer += "N/A\n";
    }
    return answer;
  }

  private static String printSVMOutput(Vector usefulFeatures,
                                       Vector nonusefulFeatures) {
    String answer = new String();
    //Now add all the features in SVM-Light format to answer
    //first the useful
    for (int i = 0; i < usefulFeatures.size(); i++) {
      Iterator fets = ((TreeSet) usefulFeatures.get(i)).iterator();
      answer += "1 ";
      DecimalFormat df = new DecimalFormat("0.0####");
      while(fets.hasNext()) {
	IntDoublePair fet = (IntDoublePair) fets.next();
	if (fet.value > THRESHOLD)
	  answer += fet.number + ":" + df.format(fet.value) + " ";
      }
      answer += "\n";
    }
    //and now non useful
    for (int i = 0; i < nonusefulFeatures.size(); i++) {
      Iterator fets = ((TreeSet) nonusefulFeatures.get(i)).iterator();
      answer += "-1 ";
      DecimalFormat df = new DecimalFormat("0.0####");
      while(fets.hasNext()) {
	IntDoublePair fet = (IntDoublePair) fets.next();
	if (fet.value > THRESHOLD)
	  answer += fet.number + ":" + df.format(fet.value) + " ";
      }
      answer += "\n";
    }
    return answer;
  }

  /* extracts features for each of the elements on invariants
     and returns a Vector of TreeSets of the features */
  private static Vector getFeatures(Vector invariants) {
    Vector answer = new Vector();
    //for each invariant, extract all the features and build a new TreeSet
    for (int i = 0; i < invariants.size(); i++) {
      //extract the common features
      //then test for all other possible features
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
      answer.add(invariant);
    }
    return answer;
  }


  // The rest of the methods extract features of various invariant
  // types (as defined by the invariant types they take as arguments)
  private static Vector getCommonFeatures(Invariant inv) {
    Vector answer = new Vector();
    if (inv.enoughSamples()) answer.add(new IntDoublePair(FetEnoughSamples, 1));
    answer.add(new IntDoublePair(FetGetProbability, inv.getProbability()));
    if (inv.isExact()) answer.add(new IntDoublePair(FetIsExact, 1));
    if (inv.justified()) answer.add(new IntDoublePair(FetJustified, 1));
    if (inv.isWorthPrinting()) answer.add(new IntDoublePair(FetIsWorthPrinting, 1));
    if (inv.hasFewModifiedSamples()) answer.add(new IntDoublePair(FetHasFewModifiedSamples, 1));
    // [INCR] if (inv.hasNonCanonicalVariable()) answer.add(new IntDoublePair(FetHasNonCanonicalVariable, 1));
    // [INCR] if (inv.hasOnlyConstantVariables()) answer.add(new IntDoublePair(FetHasOnlyConstantVariables, 1));
    if (inv.isObvious()) answer.add(new IntDoublePair(FetIsObvious, 1));
    if (inv.isObviousDerived()) answer.add(new IntDoublePair(FetIsObviousDerived, 1));
    if (inv.isObviousImplied()) answer.add(new IntDoublePair(FetIsObviousImplied, 1));
    // [INCR] if (inv.isControlled()) answer.add(new IntDoublePair(FetIsControlled, 1));
    // [INCR] if (inv.isImpliedPostcondition()) answer.add(new IntDoublePair(FetIsImpliedPostcondition, 1));
    if (inv.isInteresting()) answer.add(new IntDoublePair(FetIsInteresting, 1));
    answer.add(new IntDoublePair(FetArity, inv.ppt.arity));
    answer.add(new IntDoublePair(FetNumVars, inv.ppt.parent.num_vars()));
    answer.add(new IntDoublePair(FetNumArrayVars, inv.ppt.parent.num_array_vars()));

    return answer;
  }

  //get the variable features
  private static Vector getVarFeatures(VarInfo[] var_infos) {
    Vector answer = new Vector();
    //the i = 0 case is the OR of all the other i cases.
    for (int i = 1; i <= var_infos.length; i++) {
      VarInfo var = var_infos[i-1];
      //delete feature because hash code is not a continuous function
      //      answer.add(new IntDoublePair(i*10000 + FetVarInfoName, var.name.name().hashCode()));
      if (var.is_static_constant) {
	answer.add(new IntDoublePair(i*10000+FetVarInfoIs_Static_Constant, 1));
	answer.add(new IntDoublePair(FetVarInfoIs_Static_Constant, 1)); }
      /* [INCR]
      if (var.canBeNull) {
	answer.add(new IntDoublePair(i*10000 + FetVarInfoCanBeNull, 1));
	answer.add(new IntDoublePair(FetVarInfoIs_Static_Constant, 1)); }
      /* [INCR]
      if (var.is_dynamic_constant) {
	answer.add(new IntDoublePair(i*10000+FetVarInfoIs_Dynamic_Constant,1));
	answer.add(new IntDoublePair(FetVarInfoIs_Dynamic_Constant, 1)); }
      */ // [INCR]

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
      //delete feature because hash code is not a continuous function
      //answer.add(new IntDoublePair(i*10000 + FetTypeBase, type.base().hashCode()));
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

  private static Vector getNonZeroFeatures(NonZero inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNonZero, 1));
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

  private static Vector getUpperBoundCoreFeatures(UpperBoundCore core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetUpperBoundCoreMax1, core.max1));
    return answer;
  }

  private static Vector getLowerBoundCoreFeatures(LowerBoundCore core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetLowerBoundCoreMin1, core.min1));
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

  private static Vector getEltNonZeroFeatures(EltNonZero inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltNonZero, 1));
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

  private static Vector getEltUpperBoundFeatures(EltUpperBound inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetEltUpperBound, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getUpperBoundCoreFeatures(inv.core));
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

  private static Vector getNoDuplicatesFeatures(NoDuplicates inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetNoDuplicates, 1));
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

  private static Vector getSeqIndexComparisonFeatures(SeqIndexComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqIndexComparison, 1));
    answer.add(new IntDoublePair(FetSequence, 1));
    answer.add(new IntDoublePair(FetUnary, 1));
    answer.addAll(getIntComparisonCoreFeatures(inv.core));
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

  private static Vector getNonEqualCoreFeatures(NonEqualCore core) {
    Vector answer = new Vector();
    //    answer.add(new IntDoublePair(FetNonEqualCoreMin1, core.min1));
    //    answer.add(new IntDoublePair(FetNonEqualCoreMin2, core.min2));
    //    answer.add(new IntDoublePair(FetNonEqualCoreMax1, core.max1));
    //    answer.add(new IntDoublePair(FetNonEqualCoreMax2, core.max2));
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

  private static Vector getIntEqualFeatures(IntEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntEqual, 1));
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

  private static Vector getFunctionUnaryCoreFeatures(FunctionUnaryCore core) {
    Vector answer = new Vector();
    if (core.inverse)
      answer.add(new IntDoublePair(FetFunctionUnaryCoreInverse, 1));
    return answer;
  }

  private static Vector getIntGreaterEqualFeatures(IntGreaterEqual inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntGreaterEqual, 1));
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

  private static Vector getLinearBinaryFeatures(LinearBinary inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearBinary, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoScalar, 1));
    answer.addAll(getLinearBinaryCoreFeatures(inv.core));
    return answer;
  }

  private static Vector getLinearBinaryCoreFeatures(LinearBinaryCore core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearBinaryCoreA, core.a));
    answer.add(new IntDoublePair(FetLinearBinaryCoreB, core.b));
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

  private static Vector getIntLessThanFeatures(IntLessThan inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetIntLessThan, 1));
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

  private static Vector getSeqComparisonFeatures(SeqComparison inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSeqComparison, 1));
    answer.add(new IntDoublePair(FetBinary, 1));
    answer.add(new IntDoublePair(FetTwoSequence, 1));
    return answer;
  }

  private static Vector getTwoSequenceFeatures(TwoSequence inv) {
    Vector answer = new Vector();
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

  private static Vector getSubSequenceFeatures(SubSequence inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetSubSequence, 1));
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

  private static Vector getReverseFeatures(Reverse inv) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetReverse, 1));
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

  private static Vector getLinearTernaryCoreFeatures(LinearTernaryCore core) {
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetLinearTernaryCoreA, core.a));
    answer.add(new IntDoublePair(FetLinearTernaryCoreB, core.b));
    answer.add(new IntDoublePair(FetLinearTernaryCoreC, core.c));
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

  private static Vector getFunctionBinaryCoreFeatures(FunctionBinaryCore core){
    Vector answer = new Vector();
    answer.add(new IntDoublePair(FetFunctionBinaryCoreVar_Order, core.var_order));
    return answer;
  }

}
