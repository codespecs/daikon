package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class HeapSortSplitter_percDown_1 extends Splitter {

	public HeapSortSplitter_percDown_1() {
	}

	VarInfo tmp_varinfo;
	VarInfo child_varinfo;
	VarInfo array_varinfo;

	public HeapSortSplitter_percDown_1(Ppt ppt) {
		tmp_varinfo = ppt.findVar("tmp");
		child_varinfo = ppt.findVar("child");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((tmp_varinfo != null) && (child_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new HeapSortSplitter_percDown_1(ppt);
	}

	public String condition() {
		return "tmp < array[child]";
	}

	public boolean test(ValueTuple vt) {
		return (tmp_varinfo.getIntValue(vt) < array_varinfo.getIntArrayValue(vt) [child_varinfo.getIntValue(vt)]);
	}
}

