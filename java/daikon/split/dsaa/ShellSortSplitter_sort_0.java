package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class ShellSortSplitter_sort_0 extends Splitter {

	public ShellSortSplitter_sort_0() {
	}

	VarInfo gap_varinfo;

	public ShellSortSplitter_sort_0(Ppt ppt) {
		gap_varinfo = ppt.findVar("gap");
	}

	public boolean valid() {
		return ((gap_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new ShellSortSplitter_sort_0(ppt);
	}

	public String condition() {
		return "gap > 0";
	}

	public boolean test(ValueTuple vt) {
		return (gap_varinfo.getIntValue(vt) > 0);
	}
}

