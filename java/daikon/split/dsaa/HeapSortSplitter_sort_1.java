package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class HeapSortSplitter_sort_1 extends Splitter {

	public HeapSortSplitter_sort_1() {
	}

	VarInfo array_length_varinfo;

	public HeapSortSplitter_sort_1(Ppt ppt) {
		array_length_varinfo = ppt.findVar("array.length");
	}

	public boolean valid() {
		return ((array_length_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new HeapSortSplitter_sort_1(ppt);
	}

	public String condition() {
		return "array.length > 0";
	}

	public boolean test(ValueTuple vt) {
		return (array_length_varinfo.getIntValue(vt) > 0);
	}
}

