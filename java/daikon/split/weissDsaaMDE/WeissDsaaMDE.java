package daikon.split.weissDsaaMDE;

import daikon.split.*;

// I really ought to abstract out the tests rather than making each a class
// all to itself.  Oh, well.


// Instantiate this class in order to install all the appropriate splitters
// in SplitterList.
public class WeissDsaaMDE {

  static {

    SplitterList.put("LinkedList.find(Ljava/lang/Object;)LDataStructures/LinkedListItr;:::EXIT0",
                     new Splitter[] { new result_current_ne_0() });

    SplitterList.put("LinkedList.find(Ljava/lang/Object;)LDataStructures/LinkedListItr;:::ENTER",
                     new Splitter[] { new result_current_ne_0() });

    SplitterList.put("LinkedList.findPrevious(Ljava/lang/Object;)LDataStructures/LinkedListItr;:::EXIT0",
                     new Splitter[] { new result_current_ne_0() });

    SplitterList.put("LinkedList.findPrevious(Ljava/lang/Object;)LDataStructures/LinkedListItr;:::ENTER",
                     new Splitter[] { new result_current_ne_0() });

  }

}
