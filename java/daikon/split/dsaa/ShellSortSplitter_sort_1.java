package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class ShellSortSplitter_sort_1 extends Splitter {

	public ShellSortSplitter_sort_1() {
	}

	VarInfo i_varinfo;
	VarInfo array_length_varinfo;
	VarInfo array_varinfo;

	public ShellSortSplitter_sort_1(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
		array_length_varinfo = ppt.findVar("array.length");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((i_varinfo != null) && (array_length_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new ShellSortSplitter_sort_1(ppt);
	}

	public String condition() {
		return "i < array.length";
	}

	public boolean test(ValueTuple vt) {
		return (i_varinfo.getIntValue(vt) < array_length_varinfo.getIntValue(vt));
	}
}

