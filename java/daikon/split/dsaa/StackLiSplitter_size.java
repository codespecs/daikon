package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class StackLiSplitter_size extends Splitter {

	public StackLiSplitter_size() {
	}

	VarInfo ptr_varinfo;

	public StackLiSplitter_size(Ppt ppt) {
		ptr_varinfo = ppt.findVar("ptr");
	}

	public boolean valid() {
		return ((ptr_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new StackLiSplitter_size(ppt);
	}

	public String condition() {
		return "ptr != null";
	}

	public boolean test(ValueTuple vt) {
		return (ptr_varinfo.getIntValue(vt) != 0);
	}
}

