package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class Sample85Splitter_printArray extends Splitter {

	public Sample85Splitter_printArray() {
	}

	VarInfo i_varinfo;
	VarInfo array_length_varinfo;

	public Sample85Splitter_printArray(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
		array_length_varinfo = ppt.findVar("array.length");
	}

	public boolean valid() {
		return ((i_varinfo != null) && (array_length_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new Sample85Splitter_printArray(ppt);
	}

	public String condition() {
		return "i < array.length";
	}

	public boolean test(ValueTuple vt) {
		return (i_varinfo.getIntValue(vt) < array_length_varinfo.getIntValue(vt));
	}
}

