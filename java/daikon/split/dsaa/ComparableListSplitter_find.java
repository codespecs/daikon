package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class ComparableListSplitter_find extends Splitter {

	public ComparableListSplitter_find() {
	}

	VarInfo x_varinfo;
	VarInfo itr_varinfo;
	VarInfo itr_element_varinfo;

	public ComparableListSplitter_find(Ppt ppt) {
		x_varinfo = ppt.findVar("x");
		itr_varinfo = ppt.findVar("itr");
		itr_element_varinfo = ppt.findVar("itr.element");
	}

	public boolean valid() {
		return ((x_varinfo != null) && (itr_varinfo != null) && (itr_element_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new ComparableListSplitter_find(ppt);
	}

	public String condition() {
		return "itr != null && itr.element != x";
	}

	public boolean test(ValueTuple vt) {
		return (itr_varinfo.getIntValue(vt) != 0 && itr_element_varinfo.getIntValue(vt) != x_varinfo.getIntValue(vt));
	}
}

