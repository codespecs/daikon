package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class QueueArSplitter_makeEmpty extends Splitter {

	public QueueArSplitter_makeEmpty() {
	}

	VarInfo theArray_length_varinfo;
	VarInfo i_varinfo;

	public QueueArSplitter_makeEmpty(Ppt ppt) {
		theArray_length_varinfo = ppt.findVar("theArray.length");
		i_varinfo = ppt.findVar("i");
	}

	public boolean valid() {
		return ((theArray_length_varinfo != null) && (i_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new QueueArSplitter_makeEmpty(ppt);
	}

	public String condition() {
		return "i < theArray.length";
	}

	public boolean test(ValueTuple vt) {
		return (i_varinfo.getIntValue(vt) < theArray_length_varinfo.getIntValue(vt));
	}
}

