package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class QuickSortSplitter_sort_1 extends Splitter {

	public QuickSortSplitter_sort_1() {
	}

	VarInfo i_varinfo;
	VarInfo pivot_varinfo;
	VarInfo array_varinfo;

	public QuickSortSplitter_sort_1(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
		pivot_varinfo = ppt.findVar("pivot");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((i_varinfo != null) && (pivot_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new QuickSortSplitter_sort_1(ppt);
	}

	public String condition() {
		return "array[++i] < pivot";
	}

	public boolean test(ValueTuple vt) {
		return (array_varinfo.getIntArrayValue(vt)[+ +i_varinfo.getIndexValue(vt)] < pivot_varinfo.getIntValue(vt));
	}
}

