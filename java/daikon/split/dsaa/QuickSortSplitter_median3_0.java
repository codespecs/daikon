package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class QuickSortSplitter_median3_0 extends Splitter {

	public QuickSortSplitter_median3_0() {
	}

	VarInfo center_varinfo;
	VarInfo left_varinfo;
	VarInfo array_varinfo;

	public QuickSortSplitter_median3_0(Ppt ppt) {
		center_varinfo = ppt.findVar("center");
		left_varinfo = ppt.findVar("left");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((center_varinfo != null) && (left_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new QuickSortSplitter_median3_0(ppt);
	}

	public String condition() {
		return "array[center] < array[left]";
	}

	public boolean test(ValueTuple vt) {
		return (array_varinfo.getIntArrayValue(vt) [center_varinfo.getIntValue(vt)] < array_varinfo.getIntArrayValue(vt) [left_varinfo.getIntValue(vt)]);
	}
}

