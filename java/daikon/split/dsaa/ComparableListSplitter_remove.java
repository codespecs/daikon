package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class ComparableListSplitter_remove extends Splitter {

	public ComparableListSplitter_remove() {
	}

	VarInfo p_current_next_varinfo;

	public ComparableListSplitter_remove(Ppt ppt) {
		p_current_next_varinfo = ppt.findVar("p.current.next");
	}

	public boolean valid() {
		return ((p_current_next_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new ComparableListSplitter_remove(ppt);
	}

	public String condition() {
		return "p.current.next != null";
	}

	public boolean test(ValueTuple vt) {
		return (p_current_next_varinfo.getIntValue(vt) != 0);
	}
}

