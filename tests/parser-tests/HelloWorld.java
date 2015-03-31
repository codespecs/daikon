public class HelloWorld
{
  public static LNode head ;

  public static void main(String[] args)
  {
    LNode a = new LNode(1);
    head = a;
    LNode b = new LNode(2, a);
    LNode c = new LNode(3, b);
    c.next = a;
    print(a);
    print(b);
    print(c);
  }

  private static void print(LNode node)
  {
    System.out.println(node.val);
  }
}

class LNode
{
  public int val ;

  public LNode next ;

  public LNode(int n)
  {
    this.val = n;
  }

  public LNode(int n, LNode prev)
  {
    this.val = n;
    prev.next = this;
  }
}
