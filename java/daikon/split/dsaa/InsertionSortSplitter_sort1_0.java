package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class InsertionSortSplitter_sort1_0 extends Splitter {

	public InsertionSortSplitter_sort1_0() {
	}

	VarInfo p_varinfo;
	VarInfo array_length_varinfo;
	VarInfo array_varinfo;

	public InsertionSortSplitter_sort1_0(Ppt ppt) {
		p_varinfo = ppt.findVar("p");
		array_length_varinfo = ppt.findVar("array.length");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((p_varinfo != null) && (array_length_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new InsertionSortSplitter_sort1_0(ppt);
	}

	public String condition() {
		return "p < array.length";
	}

	public boolean test(ValueTuple vt) {
		return (p_varinfo.getIntValue(vt) < array_length_varinfo.getIntValue(vt));
	}
}

