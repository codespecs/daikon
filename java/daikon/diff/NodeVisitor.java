package daikon.diff;

public interface NodeVisitor {
  
  public void visitRootNode(RootNode node);
  public void visitPptNode(PptNode node);
  public void visitInvNode(InvNode node);

}
