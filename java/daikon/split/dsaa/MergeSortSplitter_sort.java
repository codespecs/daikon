package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class MergeSortSplitter_sort extends Splitter {

	public MergeSortSplitter_sort() {
	}

	VarInfo left_varinfo;
	VarInfo right_varinfo;

	public MergeSortSplitter_sort(Ppt ppt) {
		left_varinfo = ppt.findVar("left");
		right_varinfo = ppt.findVar("right");
	}

	public boolean valid() {
		return ((left_varinfo != null) && (right_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new MergeSortSplitter_sort(ppt);
	}

	public String condition() {
		return "left < right";
	}

	public boolean test(ValueTuple vt) {
		return (left_varinfo.getIntValue(vt) < right_varinfo.getIntValue(vt));
	}
}

