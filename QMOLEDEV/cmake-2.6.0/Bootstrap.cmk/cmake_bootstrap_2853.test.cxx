#if defined(TEST1)
# include <iostream>
#else
# include <iostream.h>
#endif

class NeedCXX 
{
public:
  NeedCXX() { this->Foo = 1; }
  int GetFoo() { return this->Foo; }
private:
  int Foo;
};
int main()
{
  NeedCXX c;
#ifdef TEST3
  cout << c.GetFoo() << endl;
#else
  std::cout << c.GetFoo() << std::endl;
#endif
  return 0;
}
