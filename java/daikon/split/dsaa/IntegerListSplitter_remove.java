package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class IntegerListSplitter_remove extends Splitter {

	public IntegerListSplitter_remove() {
	}

	VarInfo p_current_next_varinfo;

	public IntegerListSplitter_remove(Ppt ppt) {
		p_current_next_varinfo = ppt.findVar("p.current.next");
	}

	public boolean valid() {
		return ((p_current_next_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new IntegerListSplitter_remove(ppt);
	}

	public String condition() {
		return "p.current.next != null";
	}

	public boolean test(ValueTuple vt) {
		return (p_current_next_varinfo.getIntValue(vt) != 0);
	}
}

