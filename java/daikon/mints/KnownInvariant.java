package daikon.mints;

import daikon.inv.Invariant;

/**
 * @author Huascar Sanchez
 */
class KnownInvariant {
  private final String    typeOfInvariant;
  private final Invariant invariantLiteral;


  /**
   * A Likely Invariant.
   */
  private KnownInvariant(String typeOfInvariant, Invariant invariantLiteral){

    this.typeOfInvariant  = typeOfInvariant;
    this.invariantLiteral = invariantLiteral;

  }

  /**
   * Creates a new known invariant given a Daikon invariant.
   *
   * @param i Daikon invariant
   * @return a new KnownInvariant object.
   */
  static KnownInvariant from(Invariant i){
    return new KnownInvariant(i.getClass().getSimpleName(), i);
  }

  String typeOf(){
    return typeOfInvariant;
  }

  Invariant invariantObject(){
    return invariantLiteral;
  }


  @Override public String toString() {
    return typeOf() + "=>" + invariantObject().format();
  }
}
