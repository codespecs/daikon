package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class QueueArSplitter_dequeue extends Splitter {

	public QueueArSplitter_dequeue() {
	}

	VarInfo currentSize_varinfo;

	public QueueArSplitter_dequeue(Ppt ppt) {
		currentSize_varinfo = ppt.findVar("currentSize");
	}

	public boolean valid() {
		return ((currentSize_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new QueueArSplitter_dequeue(ppt);
	}

	public String condition() {
		return "isEmpty()";
	}

	public boolean test(ValueTuple vt) {
		return ((currentSize_varinfo.getIntValue(vt) == 0));
	}
}

