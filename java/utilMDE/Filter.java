package utilMDE;

/**
 * Interface for things that make boolean decisions.
 * This is inspired by java.io.FilenameFilter.
 */
public interface Filter {
  /** Tests if a specified Object satisfies the filter. */
  boolean accept(Object o);
}
