package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class MergeSortSplitter_merge_4 extends Splitter {

	public MergeSortSplitter_merge_4() {
	}

	VarInfo i_varinfo;
	VarInfo numElements_varinfo;

	public MergeSortSplitter_merge_4(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
		numElements_varinfo = ppt.findVar("numElements");
	}

	public boolean valid() {
		return ((i_varinfo != null) && (numElements_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new MergeSortSplitter_merge_4(ppt);
	}

	public String condition() {
		return "i < numElements";
	}

	public boolean test(ValueTuple vt) {
		return (i_varinfo.getIntValue(vt) < numElements_varinfo.getIntValue(vt));
	}
}

