package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class CursorListSplitter_alloc extends Splitter {

	public CursorListSplitter_alloc() {
	}

	VarInfo p_varinfo;

	public CursorListSplitter_alloc(Ppt ppt) {
		p_varinfo = ppt.findVar("p");
	}

	public boolean valid() {
		return ((p_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new CursorListSplitter_alloc(ppt);
	}

	public String condition() {
		return "p == 0";
	}

	public boolean test(ValueTuple vt) {
		return (p_varinfo.getIntValue(vt) == 0);
	}
}

