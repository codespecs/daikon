package daikon.diff;

public abstract class NodeVisitor {
  
  public void preVisitRootNode(RootNode node) { }
  public void postVisitRootNode(RootNode node) { }
  public void preVisitPptNode(PptNode node) { }
  public void postVisitPptNode(PptNode node) { }
  public void preVisitInvNode(InvNode node) { }
  public void postVisitInvNode(InvNode node) { }
}
