package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class QuickSortSplitter_sort_2 extends Splitter {

	public QuickSortSplitter_sort_2() {
	}

	VarInfo i_varinfo;
	VarInfo pivot_varinfo;
	VarInfo j_varinfo;
	VarInfo array_varinfo;

	public QuickSortSplitter_sort_2(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
		pivot_varinfo = ppt.findVar("pivot");
		j_varinfo = ppt.findVar("j");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((i_varinfo != null) && (pivot_varinfo != null) && (j_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new QuickSortSplitter_sort_2(ppt);
	}

	public String condition() {
		return "array[--j] > pivot";
	}

	public boolean test(ValueTuple vt) {
		return (array_varinfo.getIntArrayValue(vt)[- -j_varinfo.getIndexValue(vt)] > pivot_varinfo.getIntValue(vt));
	}
}

