package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class ComparableListSplitter_insert extends Splitter {

	public ComparableListSplitter_insert() {
	}

	VarInfo ptr_next_varinfo;

	public ComparableListSplitter_insert(Ppt ppt) {
		ptr_next_varinfo = ppt.findVar("ptr.next");
	}

	public boolean valid() {
		return ((ptr_next_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new ComparableListSplitter_insert(ppt);
	}

	public String condition() {
		return "ptr.next == null";
	}

	public boolean test(ValueTuple vt) {
		return (ptr_next_varinfo.getIntValue(vt) == 0);
	}
}

