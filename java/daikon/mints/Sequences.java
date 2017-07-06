package daikon.mints;

import daikon.PptMap;
import daikon.PptName;
import daikon.PptTopLevel;
import daikon.inv.Invariant;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * @author Huascar Sanchez
 */
class Sequences {
  private Sequences(){}

  /**
   * Returns a list of invariant sequences; warnings will be filter out.
   *
   * @param pptMap program point map
   * @return a new list of sequences
   */
  static List<InvariantSequence> filtered(PptMap pptMap){
    return from(pptMap, true);
  }

  /**
   * Returns a list of invariant segments; warnings will not be filter out.
   *
   * @param pptMap program point map
   * @return a new list of segments
   */
  static List<InvariantSequence> unfiltered(PptMap pptMap){
    return from(pptMap, false);
  }

  private static List<InvariantSequence> from(PptMap pptMap, boolean	skipWarning){
    if(Objects.isNull(pptMap))	return	Collections.emptyList();

    final	List<InvariantSequence>	result	=	new LinkedList<>();

    final Map<Source, InvariantSequence> segmentMap = new HashMap<>();

    for(String	eachKey	:	pptMap.nameStringSet()){
      // ignore Randoop & JUnit related artifacts
      if(eachKey.contains("BuildersRegression"))	  continue;
      if(eachKey.contains("RegressionTestDriver"))	continue;
      if(eachKey.contains("org.junit."))					  continue;
      if(eachKey.contains("RegressionTest"))			  continue;

      final	boolean	isEnter	=	eachKey.contains("ENTER");
      final	boolean	isExit	=	eachKey.contains("EXIT")	&&	!isEnter;

      //if(isExit) continue;
      System.out.println("INFO: Processing " + ((isExit ? "input " : "output ")) + "invariants.");

      final PptTopLevel eachValue	=	pptMap.get(eachKey);
      final PptName pptName		=	eachValue.ppt_name;

      final Optional<Source> candidateSource = Optional
        .ofNullable(Source.from(pptName, !isExit));

      if(!candidateSource.isPresent()) continue;

      final Source invSource = candidateSource.get();

      // skip constructors
      if(invSource.isConstructor()) continue;

      final	List<Invariant> validOnes	=	collapseConsecutive(filterWarnings(
        eachValue.getInvariants(),
        skipWarning
      ));

      if(!segmentMap.containsKey(invSource)){
        final InvariantSequence sequence = new InvariantSequence(invSource);
        sequence.add(validOnes);
        segmentMap.put(invSource, sequence);
      } else {
        segmentMap.get(invSource).add(validOnes);
      }

      result.addAll(segmentMap.values());
    }

    return result;
  }

  private static List<Invariant> collapseConsecutive(List<Invariant> invariants){
    final List<Invariant> result = new LinkedList<>();

    if(invariants.isEmpty()) return result;

    result.add(invariants.get(0));

    for(Invariant each : invariants){
      if(result.get(result.size() - 1).equals(each)) continue;

      result.add(each);
    }


    return result;
  }

  private static List<Invariant> filterWarnings(List<Invariant> invariants, boolean skipWarnings){
    List<Invariant> validOnes = invariants;

    if(skipWarnings){
      validOnes = validOnes
        .stream()
        .filter(i -> !i.format().contains("warning:"))
        .collect(Collectors.toList());
    }

    return validOnes;
  }
}
