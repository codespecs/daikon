package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class StackLiSplitter_top extends Splitter {

	public StackLiSplitter_top() {
	}

	VarInfo topOfStack_varinfo;

	public StackLiSplitter_top(Ppt ppt) {
		topOfStack_varinfo = ppt.findVar("topOfStack");
	}

	public boolean valid() {
		return ((topOfStack_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new StackLiSplitter_top(ppt);
	}

	public String condition() {
		return "isEmpty()";
	}

	public boolean test(ValueTuple vt) {
		return ((topOfStack_varinfo.getIntValue(vt) == 0));
	}
}

