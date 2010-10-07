// Check this out.  If you never call a member function that is
// defined inside of a class body, then there isn't even an entry for
// it in the symbol table.  However, if you define it OUTSIDE of a
// class body, then there is an entry for it.

/*
$ nm CppInlineTest 
...
0804838e T main
08048384 T _ZN3foo14definedOutsideEv
080483b0 W _ZN3foo15visibleFunctionEv
*/

class foo {
public:
  // This one never appears in the symbol table because it was never
  // called from anywhere:
  int invisibleFunction() {
    return 1;
  }

  // This one appears in the symbol table because it was called from
  // main()
  int visibleFunction() {
    return 2;
  }

  int definedOutside();
};

// This one appears even though it was never called from anywhere:
int foo::definedOutside() {
  return 3;
}

int main() {
  foo f;
  f.visibleFunction();

  // Un-comment to make invisibleFunction() appear in symbol table:
  //  f.invisibleFunction();
  return 0;
}
