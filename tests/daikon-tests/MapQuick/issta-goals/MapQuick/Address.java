package MapQuick;

/** An Address is an immutable record type defining a location.
 *   @specfield num     : integer // number of the building along the street
 *   @specfield name    : String  // name of the street
 *   @specfield zipcode : String  // US ZIP code
 *   @endspec
 */
public class Address {

  /*@ invariant this.streetNum >= 1; */
  /*@ invariant this.streetName != null; */
  /*@ invariant this.zipcode != null; */
  /*@ spec_public */ private int streetNum;
  /*@ spec_public */ private String streetName;
  /*@ spec_public */ private String zipcode;

  /*@ requires num >= 1; */
  /*@ requires name != null; */
  /*@ requires zipcode != null; */
  /*@ ensures num == this.streetNum; */
  /*@ ensures name == this.streetName; */
  /*@ ensures zipcode == this.zipcode; */
  /** @effects returns an Address with the given field values */
  public Address (int num, String name, String zipcode) {
    this.streetNum = num;
    this.streetName = name;
    this.zipcode = zipcode;
  }

  /*@ ensures \result == this.streetNum; */
  /**@ ensures \result == \old(this.streetNum); */
  /** @returns this.num */
  public int getNum () {
    return streetNum;
  }

  /*@ ensures \result == this.streetName; */
  /**@ ensures \result == \old(this.streetName); */
  /** @returns this.name */
  public String getName () {
    return streetName;
  }

  /*@ ensures \result == this.zipcode; */
  /**@ ensures \result == \old(this.zipcode); */
  /** @returns this.zipcode */
  public String getZipcode () {
    return zipcode;
  }

  /*@ also_ensures (\result == true)  ==>  (other != null); */
  /*# also_ensures (\result == true)  ==>  (\typeof(other) == \type(MapQuick.Address)); */ // ESC bug
  public boolean equals (Object other) {
    return (other instanceof Address) && equals((Address) other);
  }

  /*@ modifies other.streetNum, other.streetName, other.zipcode; */
  /*@ ensures (\result == true)  ==>  (other != null); */
  public boolean equals (Address other) {
    return
      (other != null) &&
      zipcode.equals(other.zipcode) &&
      streetName.equals(other.streetName) &&
      (streetNum == other.streetNum);
  }

  public int hashCode () {
    return streetName.hashCode() + zipcode.hashCode() * 7 + streetNum * 17;
  }

  /**@ also_ensures \result != null; */
  /**
   * @return a String representation of this address in the format:
   * "num name zipcode"
   */
  public String toString () {
    return new String (streetNum + " " + streetName + " " + zipcode);
  }


}
