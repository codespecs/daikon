package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class MergeSortSplitter_merge_1 extends Splitter {

	public MergeSortSplitter_merge_1() {
	}

	VarInfo rightPos_varinfo;
	VarInfo i_varinfo;
	VarInfo leftPos_varinfo;
	VarInfo array_varinfo;

	public MergeSortSplitter_merge_1(Ppt ppt) {
		rightPos_varinfo = ppt.findVar("rightPos");
		i_varinfo = ppt.findVar("i");
		leftPos_varinfo = ppt.findVar("leftPos");
		array_varinfo = ppt.findVar("array");
	}

	public boolean valid() {
		return ((rightPos_varinfo != null) && (i_varinfo != null) && (leftPos_varinfo != null) && (array_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new MergeSortSplitter_merge_1(ppt);
	}

	public String condition() {
		return "array[leftPos] <= array[rightPos]";
	}

	public boolean test(ValueTuple vt) {
		return (array_varinfo.getIntArrayValue(vt) [leftPos_varinfo.getIndexValue(vt)] <= array_varinfo.getIntArrayValue(vt) [rightPos_varinfo.getIndexValue(vt)]);
	}
}

