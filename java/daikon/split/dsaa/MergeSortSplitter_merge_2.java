package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class MergeSortSplitter_merge_2 extends Splitter {

	public MergeSortSplitter_merge_2() {
	}

	VarInfo leftEnd_varinfo;
	VarInfo leftPos_varinfo;

	public MergeSortSplitter_merge_2(Ppt ppt) {
		leftEnd_varinfo = ppt.findVar("leftEnd");
		leftPos_varinfo = ppt.findVar("leftPos");
	}

	public boolean valid() {
		return ((leftEnd_varinfo != null) && (leftPos_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new MergeSortSplitter_merge_2(ppt);
	}

	public String condition() {
		return "leftPos <= leftEnd";
	}

	public boolean test(ValueTuple vt) {
		return (leftPos_varinfo.getIntValue(vt) <= leftEnd_varinfo.getIntValue(vt));
	}
}

