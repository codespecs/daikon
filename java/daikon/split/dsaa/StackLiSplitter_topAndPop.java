package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class StackLiSplitter_topAndPop extends Splitter {

	public StackLiSplitter_topAndPop() {
	}

	VarInfo topOfStack_varinfo;

	public StackLiSplitter_topAndPop(Ppt ppt) {
		topOfStack_varinfo = ppt.findVar("topOfStack");
	}

	public boolean valid() {
		return ((topOfStack_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new StackLiSplitter_topAndPop(ppt);
	}

	public String condition() {
		return "isEmpty()";
	}

	public boolean test(ValueTuple vt) {
		return ((topOfStack_varinfo.getIntValue(vt) == 0));
	}
}

