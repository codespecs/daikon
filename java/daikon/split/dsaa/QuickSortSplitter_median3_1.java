package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class QuickSortSplitter_median3_1 extends Splitter {

	public QuickSortSplitter_median3_1() {
	}

	VarInfo left_varinfo;
	VarInfo right_varinfo;
	VarInfo array_varinfo;

	public QuickSortSplitter_median3_1(Ppt ppt) {
		left_varinfo = ppt.findVar("left");
		right_varinfo = ppt.findVar("right");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((left_varinfo != null) && (right_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new QuickSortSplitter_median3_1(ppt);
	}

	public String condition() {
		return "array[right] < array[left]";
	}

	public boolean test(ValueTuple vt) {
		return (array_varinfo.getIntArrayValue(vt) [right_varinfo.getIntValue(vt)] < array_varinfo.getIntArrayValue(vt) [left_varinfo.getIntValue(vt)]);
	}
}

