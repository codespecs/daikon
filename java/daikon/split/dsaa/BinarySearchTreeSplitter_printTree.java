package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class BinarySearchTreeSplitter_printTree extends Splitter {

	public BinarySearchTreeSplitter_printTree() {
	}

	VarInfo root_varinfo;

	public BinarySearchTreeSplitter_printTree(Ppt ppt) {
		root_varinfo = ppt.findVar("root");
	}

	public boolean valid() {
		return ((root_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new BinarySearchTreeSplitter_printTree(ppt);
	}

	public String condition() {
		return "isEmpty()";
	}

	public boolean test(ValueTuple vt) {
		return ((root_varinfo.getIntValue(vt) == 0));
	}
}

