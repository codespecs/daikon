package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class CursorListSplitter_insert extends Splitter {

	public CursorListSplitter_insert() {
	}

	VarInfo p_varinfo;
	VarInfo p_current_varinfo;

	public CursorListSplitter_insert(Ppt ppt) {
		p_varinfo = ppt.findVar("p");
		p_current_varinfo = ppt.findVar("p.current");
	}

	public boolean valid() {
		return ((p_varinfo != null) && (p_current_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new CursorListSplitter_insert(ppt);
	}

	public String condition() {
		return "p != null && p.current != 0";
	}

	public boolean test(ValueTuple vt) {
		return (p_varinfo.getIntValue(vt) != 0 && p_current_varinfo.getIntValue(vt) != 0);
	}
}

