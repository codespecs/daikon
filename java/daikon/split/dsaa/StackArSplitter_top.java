package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class StackArSplitter_top extends Splitter {

	public StackArSplitter_top() {
	}

	VarInfo topOfStack_varinfo;

	public StackArSplitter_top(Ppt ppt) {
		topOfStack_varinfo = ppt.findVar("topOfStack");
	}

	public boolean valid() {
		return ((topOfStack_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new StackArSplitter_top(ppt);
	}

	public String condition() {
		return "isEmpty()";
	}

	public boolean test(ValueTuple vt) {
		return ((topOfStack_varinfo.getIntValue(vt) == -1));
	}
}

