package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class QueueArSplitter_getFront extends Splitter {

	public QueueArSplitter_getFront() {
	}

	VarInfo currentSize_varinfo;

	public QueueArSplitter_getFront(Ppt ppt) {
		currentSize_varinfo = ppt.findVar("currentSize");
	}

	public boolean valid() {
		return ((currentSize_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new QueueArSplitter_getFront(ppt);
	}

	public String condition() {
		return "isEmpty()";
	}

	public boolean test(ValueTuple vt) {
		return ((currentSize_varinfo.getIntValue(vt) == 0));
	}
}

