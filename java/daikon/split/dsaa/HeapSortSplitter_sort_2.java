package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class HeapSortSplitter_sort_2 extends Splitter {

	public HeapSortSplitter_sort_2() {
	}

	VarInfo i_varinfo;

	public HeapSortSplitter_sort_2(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
	}

	public boolean valid() {
		return ((i_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new HeapSortSplitter_sort_2(ppt);
	}

	public String condition() {
		return "i > 0";
	}

	public boolean test(ValueTuple vt) {
		return (i_varinfo.getIntValue(vt) > 0);
	}
}

