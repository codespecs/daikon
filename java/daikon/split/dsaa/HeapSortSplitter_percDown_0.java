package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class HeapSortSplitter_percDown_0 extends Splitter {

	public HeapSortSplitter_percDown_0() {
	}

	VarInfo child_varinfo;
	VarInfo n_varinfo;
	VarInfo array_varinfo;

	public HeapSortSplitter_percDown_0(Ppt ppt) {
		child_varinfo = ppt.findVar("child");
		n_varinfo = ppt.findVar("n");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((child_varinfo != null) && (n_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new HeapSortSplitter_percDown_0(ppt);
	}

	public String condition() {
		return "child != n - 1 && array[child] < array[child+1]";
	}

	public boolean test(ValueTuple vt) {
		return (child_varinfo.getIntValue(vt) != n_varinfo.getIntValue(vt) - 1 && array_varinfo.getIntArrayValue(vt) [child_varinfo.getIntValue(vt)] < array_varinfo.getIntArrayValue(vt) [child_varinfo.getIntValue(vt)+1]);
	}
}

