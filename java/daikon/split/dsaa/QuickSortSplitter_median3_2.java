package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class QuickSortSplitter_median3_2 extends Splitter {

	public QuickSortSplitter_median3_2() {
	}

	VarInfo center_varinfo;
	VarInfo right_varinfo;
	VarInfo array_varinfo;

	public QuickSortSplitter_median3_2(Ppt ppt) {
		center_varinfo = ppt.findVar("center");
		right_varinfo = ppt.findVar("right");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((center_varinfo != null) && (right_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new QuickSortSplitter_median3_2(ppt);
	}

	public String condition() {
		return "array[right] < array[center]";
	}

	public boolean test(ValueTuple vt) {
		return (array_varinfo.getIntArrayValue(vt) [right_varinfo.getIntValue(vt)] < array_varinfo.getIntArrayValue(vt) [center_varinfo.getIntValue(vt)]);
	}
}

