package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class BinarySearchTreeSplitter_removeMin_1 extends Splitter {

	public BinarySearchTreeSplitter_removeMin_1() {
	}

	VarInfo t_varinfo;
	VarInfo t_left_varinfo;

	public BinarySearchTreeSplitter_removeMin_1(Ppt ppt) {
		t_varinfo = ppt.findVar("t");
		t_left_varinfo = ppt.findVar("t.left");
	}

	public boolean valid() {
		return ((t_varinfo != null) && (t_left_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new BinarySearchTreeSplitter_removeMin_1(ppt);
	}

	public String condition() {
		return "t.left != null";
	}

	public boolean test(ValueTuple vt) {
		return (t_left_varinfo.getIntValue(vt) != 0);
	}
}

