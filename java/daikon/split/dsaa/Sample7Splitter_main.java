package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class Sample7Splitter_main extends Splitter {

	public Sample7Splitter_main() {
	}

	VarInfo i_varinfo;
	VarInfo size_varinfo;

	public Sample7Splitter_main(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
		size_varinfo = ppt.findVar("size");
	}

	public boolean valid() {
		return ((i_varinfo != null) && (size_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new Sample7Splitter_main(ppt);
	}

	public String condition() {
		return "i < size";
	}

	public boolean test(ValueTuple vt) {
		return (i_varinfo.getIntValue(vt) < size_varinfo.getIntValue(vt));
	}
}

