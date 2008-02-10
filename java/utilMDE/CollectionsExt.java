package utilMDE;

import java.util.List;

/**
 * Extends functionality from Java Collections API.
 */
public class CollectionsExt {

  /**
   * Inserts the elements in eltsToPrepent at the beginning
   * of the list. Shifts the elements in the original list
   * by the appropriate amount.
   */
  public static <T, U extends T> void prepend(List<U> eltsToPrepend, List<T> list) {
    for (int i = eltsToPrepend.size() - 1; i >= 0; i--) {
      list.add(0, eltsToPrepend.get(i));
    }
  }

}
