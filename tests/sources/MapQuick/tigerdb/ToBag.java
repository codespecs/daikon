package MapQuick.tigerdb;

import java.util.*;

class ToBag extends LinkedHashMap {
  ToBag() { super(); }
  public Object get(Object key) {
    Object bag = super.get(key);
    if (bag == null) {
      bag = new ArrayList();
      super.put(key, bag);
    }
    return bag;
  }
  public Collection getBag(Object key) {
    return (Collection) this.get(key);
  }
}
