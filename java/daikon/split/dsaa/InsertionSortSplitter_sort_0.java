package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class InsertionSortSplitter_sort_0 extends Splitter {

	public InsertionSortSplitter_sort_0() {
	}

	VarInfo p_varinfo;
	VarInfo length_varinfo;

	public InsertionSortSplitter_sort_0(Ppt ppt) {
		p_varinfo = ppt.findVar("p");
		length_varinfo = ppt.findVar("length");
	}

	public boolean valid() {
		return ((p_varinfo != null) && (length_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new InsertionSortSplitter_sort_0(ppt);
	}

	public String condition() {
		return "p < length";
	}

	public boolean test(ValueTuple vt) {
		return (p_varinfo.getIntValue(vt) < length_varinfo.getIntValue(vt));
	}
}

