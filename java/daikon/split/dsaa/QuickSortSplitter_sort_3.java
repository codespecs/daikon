package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class QuickSortSplitter_sort_3 extends Splitter {

	public QuickSortSplitter_sort_3() {
	}

	VarInfo i_varinfo;
	VarInfo j_varinfo;

	public QuickSortSplitter_sort_3(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
		j_varinfo = ppt.findVar("j");
	}

	public boolean valid() {
		return ((i_varinfo != null) && (j_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new QuickSortSplitter_sort_3(ppt);
	}

	public String condition() {
		return "i < j";
	}

	public boolean test(ValueTuple vt) {
		return (i_varinfo.getIntValue(vt) < j_varinfo.getIntValue(vt));
	}
}

