package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class Sample82Splitter_generateArray extends Splitter {

	public Sample82Splitter_generateArray() {
	}

	VarInfo i_varinfo;
	VarInfo length_varinfo;

	public Sample82Splitter_generateArray(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
		length_varinfo = ppt.findVar("length");
	}

	public boolean valid() {
		return ((i_varinfo != null) && (length_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new Sample82Splitter_generateArray(ppt);
	}

	public String condition() {
		return "i < length";
	}

	public boolean test(ValueTuple vt) {
		return (i_varinfo.getIntValue(vt) < length_varinfo.getIntValue(vt));
	}
}

