package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class BinarySearchTreeSplitter_remove1_1 extends Splitter {

	public BinarySearchTreeSplitter_remove1_1() {
	}

	VarInfo t_right_varinfo;
	VarInfo t_varinfo;
	VarInfo t_left_varinfo;

	public BinarySearchTreeSplitter_remove1_1(Ppt ppt) {
		t_right_varinfo = ppt.findVar("t.right");
		t_varinfo = ppt.findVar("t");
		t_left_varinfo = ppt.findVar("t.left");
	}

	public boolean valid() {
		return ((t_right_varinfo != null) && (t_varinfo != null) && (t_left_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new BinarySearchTreeSplitter_remove1_1(ppt);
	}

	public String condition() {
		return "t.left != null && t.right != null";
	}

	public boolean test(ValueTuple vt) {
		return (t_left_varinfo.getIntValue(vt) != 0 && t_right_varinfo.getIntValue(vt) != 0);
	}
}

