package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class MergeSortSplitter_merge_0 extends Splitter {

	public MergeSortSplitter_merge_0() {
	}

	VarInfo rightPos_varinfo;
	VarInfo i_varinfo;
	VarInfo leftEnd_varinfo;
	VarInfo leftPos_varinfo;
	VarInfo rightEnd_varinfo;

	public MergeSortSplitter_merge_0(Ppt ppt) {
		rightPos_varinfo = ppt.findVar("rightPos");
		i_varinfo = ppt.findVar("i");
		leftEnd_varinfo = ppt.findVar("leftEnd");
		leftPos_varinfo = ppt.findVar("leftPos");
		rightEnd_varinfo = ppt.findVar("rightEnd");
	}

	public boolean valid() {
		return ((rightPos_varinfo != null) && (i_varinfo != null) && (leftEnd_varinfo != null) && (leftPos_varinfo != null) && (rightEnd_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new MergeSortSplitter_merge_0(ppt);
	}

	public String condition() {
		return "leftPos <= leftEnd && rightPos <= rightEnd";
	}

	public boolean test(ValueTuple vt) {
		return (leftPos_varinfo.getIntValue(vt) <= leftEnd_varinfo.getIntValue(vt) && rightPos_varinfo.getIntValue(vt) <= rightEnd_varinfo.getIntValue(vt));
	}
}

