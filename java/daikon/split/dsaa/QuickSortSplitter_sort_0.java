package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class QuickSortSplitter_sort_0 extends Splitter {

	public QuickSortSplitter_sort_0() {
	}

	VarInfo i_varinfo;
	VarInfo left_varinfo;
	VarInfo right_varinfo;
	VarInfo CUTOFF_varinfo;

	public QuickSortSplitter_sort_0(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
		left_varinfo = ppt.findVar("left");
		right_varinfo = ppt.findVar("right");
		CUTOFF_varinfo = ppt.findVar("CUTOFF");
	}

	public boolean valid() {
		return ((i_varinfo != null) && (left_varinfo != null) && (right_varinfo != null) && (CUTOFF_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new QuickSortSplitter_sort_0(ppt);
	}

	public String condition() {
		return "left + CUTOFF <= right";
	}

	public boolean test(ValueTuple vt) {
		return (left_varinfo.getIntValue(vt) + CUTOFF_varinfo.getIntValue(vt) <= right_varinfo.getIntValue(vt));
	}
}

