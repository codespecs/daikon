package daikon.split.dsaa;

import daikon.*;
import daikon.split.*;

class CursorListSplitter_length extends Splitter {

	public CursorListSplitter_length() {
	}

	VarInfo i_varinfo;
	VarInfo SPACE_SIZE_varinfo;

	public CursorListSplitter_length(Ppt ppt) {
		i_varinfo = ppt.findVar("i");
		SPACE_SIZE_varinfo = ppt.findVar("SPACE_SIZE");
	}

	public boolean valid() {
		return ((i_varinfo != null) && (SPACE_SIZE_varinfo != null) && true);
	}

	public Splitter instantiate(Ppt ppt) {
		return new CursorListSplitter_length(ppt);
	}

	public String condition() {
		return "i < SPACE_SIZE";
	}

	public boolean test(ValueTuple vt) {
		return (i_varinfo.getIntValue(vt) < SPACE_SIZE_varinfo.getIntValue(vt));
	}
}

