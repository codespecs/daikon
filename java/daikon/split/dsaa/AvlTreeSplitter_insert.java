package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class AvlTreeSplitter_insert extends Splitter {

	public AvlTreeSplitter_insert() {
	}

	VarInfo t_varinfo;

	public AvlTreeSplitter_insert(Ppt ppt) {
		t_varinfo = ppt.findVar("t");
	}

	public boolean valid() {
		return ((t_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new AvlTreeSplitter_insert(ppt);
	}

	public String condition() {
		return "t == null";
	}

	public boolean test(ValueTuple vt) {
		return (t_varinfo.getIntValue(vt) == 0);
	}
}

