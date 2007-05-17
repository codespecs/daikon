package binary_variables;

/**
 * Address is uniquely determined by a combination of module+offset within the module
 * (the offset is given as a hex number that is converted to long).
 *
 * Date: 28/02/2007
 */
public final class Address extends Addressable<Address> {
  public Address(long address, String module) {
    super(address, module);
  }
  public Address(Addressable addr) {
    super(addr);
  }
}
