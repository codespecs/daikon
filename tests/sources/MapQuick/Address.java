package MapQuick;

/** An Address is an immutable record type defining a location.
 *   @specfield num     : integer // number of the building along the street
 *   @specfield name    : String  // name of the street
 *   @specfield zipcode : String  // US ZIP code
 *   @endspec
 */
public class Address {

  private int streetNum;
  private String streetName;
  private String zipcode;

  /** @effects returns an Address with the given field values */
  public Address (int num, String name, String zipcode) {
    this.streetNum = num;
    this.streetName = name;
    this.zipcode = zipcode;
  }

  /** @returns this.num */
  public int getNum () {
    return streetNum;
  }

  /** @returns this.name */
  public String getName () {
    return streetName;
  }

  /** @returns this.zipcode */
  public String getZipcode () {
    return zipcode;
  }

  public boolean equals (Object other) {
    return (other instanceof Address) && equals((Address) other);
  }

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

  /**
   * @return a String representation of this address in the format:
   * "num name zipcode"
   */
  public String toString () {
    return new String (streetNum + " " + streetName + " " + zipcode);
  }


}
