package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class BinarySearchTreeSplitter_elementAt extends Splitter {

    VarInfo t_varinfo;

// Constructor: BinarySearchTreeSplitter_elementAt()
// This is a constructor. But I don't know how this could be used...

    public BinarySearchTreeSplitter_elementAt() {}

// Constructor: BinarySearchTreeSplitter_elementAt(Ppt ppt)
// This instantiates the splitter for a given program point. This
// also initializes the variable information used in the current
// splitter.

    public BinarySearchTreeSplitter_elementAt(Ppt ppt) {
	t_varinfo = ppt.findVar("t");
    }

// Method: valid()
// This method returns whether all the variables used in the condition
// are valid at the context or not.

    public boolean valid() {
	return ((t_varinfo != null) && true);
    }

// Method: instantiate(Ppt ppt)
// This method instantiates the splitter for the given program point.
// This simply invokes the constructor.

    public Splitter instantiate(Ppt ppt) {
	return new BinarySearchTreeSplitter_elementAt(ppt);
    }

// Method: condition()
// This method returns a String object which contains the original
// expression of the condition. The word "original" means that if
// the condition was expressed as a simple boolean method call,
// this method will return the boolean method call itself though
// the evaluation might be done in another form.

    public String condition() {
	return "t == null ";
    }

// Method: test(ValueTuple vt)
// This method returns whether the condition is true or false at
// the given program point. A single boolean method condition can
// be evaluated by replacing the method with the corresponding
// expression in the boolean method definition.

    public boolean test(ValueTuple vt) {
	return (t_varinfo.getIntValue(vt) == 0);
    }
}
