package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class LinkedListItrSplitter_advance extends Splitter {

	public LinkedListItrSplitter_advance() {
	}

	VarInfo current_varinfo;

	public LinkedListItrSplitter_advance(Ppt ppt) {
		current_varinfo = ppt.findVar("current");
	}

	public boolean valid() {
		return ((current_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new LinkedListItrSplitter_advance(ppt);
	}

	public String condition() {
		return "!isPastEnd()";
	}

	public boolean test(ValueTuple vt) {
		return (! (current_varinfo.getIntValue(vt) == 0));
	}
}

