package binary_variables;

/**
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
