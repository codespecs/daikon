package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class BinarySearchTreeSplitter_size1 extends Splitter {

	public BinarySearchTreeSplitter_size1() {
	}

	VarInfo t_varinfo;

	public BinarySearchTreeSplitter_size1(Ppt ppt) {
		t_varinfo = ppt.findVar("t");
	}

	public boolean valid() {
		return ((t_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new BinarySearchTreeSplitter_size1(ppt);
	}

	public String condition() {
		return "t == null";
	}

	public boolean test(ValueTuple vt) {
		return (t_varinfo.getIntValue(vt) == 0);
	}
}

