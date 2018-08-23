package daikon.split.misc;

import daikon.split.Splitter;
import daikon.split.SplitterList;

public final class MiscSplitters {

  static {
    SplitterList.put(
        "",
        new Splitter[] {
          new ReturnTrueSplitter(),
        });
  }
}
