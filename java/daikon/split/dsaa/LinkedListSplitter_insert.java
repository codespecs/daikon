package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class LinkedListSplitter_insert extends Splitter {

	public LinkedListSplitter_insert() {
	}

	VarInfo p_varinfo;
	VarInfo p_current_varinfo;

	public LinkedListSplitter_insert(Ppt ppt) {
		p_varinfo = ppt.findVar("p");
		p_current_varinfo = ppt.findVar("p.current");
	}

	public boolean valid() {
		return ((p_varinfo != null) && (p_current_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new LinkedListSplitter_insert(ppt);
	}

	public String condition() {
		return "p != null && p.current != null";
	}

	public boolean test(ValueTuple vt) {
		return (p_varinfo.getIntValue(vt) != 0 && p_current_varinfo.getIntValue(vt) != 0);
	}
}

