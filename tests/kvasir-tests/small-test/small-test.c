#include <stdlib.h>

struct foo {
  int a;
  int b;
};

struct node {
  struct node * next;
  struct node * prev;
  int b;
};

void bar(struct foo *f) {
}

void baz(struct node *n) {
}

void baa(int a,int b) {}
void bab(int a,int b) {}
void bac(int a,int b) {}
void bad(int a,int b) {}
void bae(int a,int b) {}
void caa(int a,int b,int c) {}
void cab(int a,int b,int c) {}
void cac(int a,int b,int c) {}
void cad(int a,int b,int c) {}
void cae(int a,int b,int c) {}


int main() {
	struct foo *a=(struct foo *)malloc(sizeof(struct foo));
        int i;
	a->a=1;
	a->b=2;
	bar(a);

	{
	  struct node *n1=(struct node *)malloc(sizeof(struct node));
	  struct node *n2=(struct node *)malloc(sizeof(struct node));
	  n1->next=n2;
	  n1->prev=0;
	  n2->prev=n1;
	  n2->next=0;
	  n1->b=2;
	  n2->b=2;
	  baz(n1);
	}

        for (i=0;i<1000;i++) {
          baa(i,i*2+3);
          bab(i,i*i);
          bac(i,i+3);
          caa(i,i+2,i*(i+2));
          cab(i,i+3,i+(i+3));
        }
	return 0;
}
