package daikon.split;

import daikon.*;
import java.util.Arrays;

// Shouldn't this be an inner class of ContextSplitterFactory?
/**
 * Simple record type to store a PptName and Splitter array.
 **/
public final class PptNameAndSplitters
{
  public final String ppt_name; // really more like a regexp
  public final Splitter[] splitters;

  public PptNameAndSplitters(String ppt_name, Splitter[] splitters) {
    this.ppt_name = ppt_name;
    this.splitters = splitters;
  }

  public String toString() {
    return "PptNameAndSplitters<" + ppt_name + ","
      + Arrays.asList(splitters).toString() + ">";
  }

}
