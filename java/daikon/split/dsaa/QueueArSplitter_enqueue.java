package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class QueueArSplitter_enqueue extends Splitter {

	public QueueArSplitter_enqueue() {
	}

	VarInfo theArray_length_varinfo;
	VarInfo currentSize_varinfo;

	public QueueArSplitter_enqueue(Ppt ppt) {
		theArray_length_varinfo = ppt.findVar("theArray.length");
		currentSize_varinfo = ppt.findVar("currentSize");
	}

	public boolean valid() {
		return ((theArray_length_varinfo != null) && (currentSize_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new QueueArSplitter_enqueue(ppt);
	}

	public String condition() {
		return "isFull()";
	}

	public boolean test(ValueTuple vt) {
		return ((currentSize_varinfo.getIntValue(vt) == theArray_length_varinfo.getIntValue(vt)));
	}
}

