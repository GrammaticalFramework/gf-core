package org.grammaticalframework.pgf;

/** A bracket represents a syntactic constituent in the parse tree
 * of a sentence. */
public class Bracket {
	/** The category of this bracket */
	public final String cat;
	
	/** The abstract function name for the bracket */
	public final String fun;
	
	/** Every constituent has an unique id. If the constituent is
	 * discontinuous then it will be represented with several brackets
	 * where they all will have the same id */
	public final int fid;

	public final String ann;
	
	/** The children of the bracket. Every element is either a string
	 * if this is a leaf in the parse tree, or a {@link Bracket} object.
	 */
	public final Object[] children;

	public Bracket(String cat, String fun, int fid, String ann, Object[] children) {
		this.cat = cat;
		this.fun = fun;
		this.fid = fid;
		this.ann = ann;
		this.children = children;
	}
}
