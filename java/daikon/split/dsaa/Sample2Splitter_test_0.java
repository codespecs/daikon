package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class Sample2Splitter_test_0 extends Splitter {

	public Sample2Splitter_test_0() {
	}

	VarInfo i_varinfo;
	VarInfo size_varinfo;

	public Sample2Splitter_test_0(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
		size_varinfo = ppt.findVar("size");
	}

	public boolean valid() {
		return ((i_varinfo != null) && (size_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new Sample2Splitter_test_0(ppt);
	}

	public String condition() {
		return "i < size";
	}

	public boolean test(ValueTuple vt) {
		return (i_varinfo.getIntValue(vt) < size_varinfo.getIntValue(vt));
	}
}

