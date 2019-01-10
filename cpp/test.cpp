#include <iostream> // Include for I/O streams
#include <string>

using namespace std; // Streams are in the std namespace (standard library)

int main()
{
  string foo = "I am foo";
  string bar = "I am bar";


  string& fooRef = foo; // This creates a reference to foo.
  cout << foo; // Prints "I am foo. Hi!"
  fooRef += ". Hi!\n"; // Modifies foo through the reference
  cout << fooRef; // Prints "I am foo. Hi!"
  cout << foo; // Prints "I am foo. Hi!"

}
