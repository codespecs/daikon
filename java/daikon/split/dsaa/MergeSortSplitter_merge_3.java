package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class MergeSortSplitter_merge_3 extends Splitter {

	public MergeSortSplitter_merge_3() {
	}

	VarInfo rightPos_varinfo;
	VarInfo i_varinfo;
	VarInfo rightEnd_varinfo;

	public MergeSortSplitter_merge_3(Ppt ppt) {
		rightPos_varinfo = ppt.findVar("rightPos");
		i_varinfo = ppt.findVar("i");
		rightEnd_varinfo = ppt.findVar("rightEnd");
	}

	public boolean valid() {
		return ((rightPos_varinfo != null) && (i_varinfo != null) && (rightEnd_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new MergeSortSplitter_merge_3(ppt);
	}

	public String condition() {
		return "rightPos <= rightEnd";
	}

	public boolean test(ValueTuple vt) {
		return (rightPos_varinfo.getIntValue(vt) <= rightEnd_varinfo.getIntValue(vt));
	}
}

