package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class StackArSplitter_push extends Splitter {

	public StackArSplitter_push() {
	}

	VarInfo theArray_length_varinfo;
	VarInfo topOfStack_varinfo;

	public StackArSplitter_push(Ppt ppt) {
		theArray_length_varinfo = ppt.findVar("theArray.length");
		topOfStack_varinfo = ppt.findVar("topOfStack");
	}

	public boolean valid() {
		return ((theArray_length_varinfo != null) && (topOfStack_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new StackArSplitter_push(ppt);
	}

	public String condition() {
		return "isFull()";
	}

	public boolean test(ValueTuple vt) {
		return ((topOfStack_varinfo.getIntValue(vt) == theArray_length_varinfo.getIntValue(vt) - 1));
	}
}

