package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class BinarySearchTreeSplitter_findMax1_1 extends Splitter {

	public BinarySearchTreeSplitter_findMax1_1() {
	}

	VarInfo t_right_varinfo;
	VarInfo t_varinfo;

	public BinarySearchTreeSplitter_findMax1_1(Ppt ppt) {
		t_right_varinfo = ppt.findVar("t.right");
		t_varinfo = ppt.findVar("t");
	}

	public boolean valid() {
		return ((t_right_varinfo != null) && (t_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new BinarySearchTreeSplitter_findMax1_1(ppt);
	}

	public String condition() {
		return "t.right != null";
	}

	public boolean test(ValueTuple vt) {
		return (t_right_varinfo.getIntValue(vt) != 0);
	}
}

