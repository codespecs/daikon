package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class StackArSplitter_pop extends Splitter {

	public StackArSplitter_pop() {
	}

	VarInfo topOfStack_varinfo;

	public StackArSplitter_pop(Ppt ppt) {
		topOfStack_varinfo = ppt.findVar("topOfStack");
	}

	public boolean valid() {
		return ((topOfStack_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new StackArSplitter_pop(ppt);
	}

	public String condition() {
		return "isEmpty()";
	}

	public boolean test(ValueTuple vt) {
		return ((topOfStack_varinfo.getIntValue(vt) == -1));
	}
}

