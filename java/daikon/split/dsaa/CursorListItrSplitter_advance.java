package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class CursorListItrSplitter_advance extends Splitter {

	public CursorListItrSplitter_advance() {
	}

	VarInfo current_varinfo;

	public CursorListItrSplitter_advance(Ppt ppt) {
		current_varinfo = ppt.findVar("current");
	}

	public boolean valid() {
		return ((current_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new CursorListItrSplitter_advance(ppt);
	}

	public String condition() {
		return "!isPastEnd()";
	}

	public boolean test(ValueTuple vt) {
		return (! (current_varinfo.getIntValue(vt) == 0));
	}
}

