package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class InsertionSortSplitter_sort_1 extends Splitter {

	public InsertionSortSplitter_sort_1() {
	}

	VarInfo p_varinfo;
	VarInfo tmp_varinfo;
	VarInfo j_varinfo;
	VarInfo begin_varinfo;
	VarInfo array_varinfo;

	public InsertionSortSplitter_sort_1(Ppt ppt) {
		p_varinfo = ppt.findVar("p");
		tmp_varinfo = ppt.findVar("tmp");
		j_varinfo = ppt.findVar("j");
		begin_varinfo = ppt.findVar("begin");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((p_varinfo != null) && (tmp_varinfo != null) && (j_varinfo != null) && (begin_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new InsertionSortSplitter_sort_1(ppt);
	}

	public String condition() {
		return "j > begin && tmp < array[j-1]";
	}

	public boolean test(ValueTuple vt) {
		return (j_varinfo.getIntValue(vt) > begin_varinfo.getIntValue(vt) && tmp_varinfo.getIntValue(vt) < array_varinfo.getIntArrayValue(vt) [j_varinfo.getIndexValue(vt)-1]);
	}
}

