package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class ShellSortSplitter_sort_2 extends Splitter {

	public ShellSortSplitter_sort_2() {
	}

	VarInfo gap_varinfo;
	VarInfo tmp_varinfo;
	VarInfo j_varinfo;
	VarInfo array_varinfo;

	public ShellSortSplitter_sort_2(Ppt ppt) {
		gap_varinfo = ppt.findVar("gap");
		tmp_varinfo = ppt.findVar("tmp");
		j_varinfo = ppt.findVar("j");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((gap_varinfo != null) && (tmp_varinfo != null) && (j_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new ShellSortSplitter_sort_2(ppt);
	}

	public String condition() {
		return "j >= gap && tmp < array[j-gap]";
	}

	public boolean test(ValueTuple vt) {
		return (j_varinfo.getIntValue(vt) >= gap_varinfo.getIntValue(vt) && tmp_varinfo.getIntValue(vt) < array_varinfo.getIntArrayValue(vt) [j_varinfo.getIndexValue(vt) -gap_varinfo.getIndexValue(vt)]);
	}
}

