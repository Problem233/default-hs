#include <iostream>
#include <cmath>

#define EXP 16445

using namespace std;

int main() {
  long double num = 0.5;
  for (int i = 1; i < EXP; i++) num = num / 2;
  printf("2^-%d = %.16445Lf\n\n", EXP, num);
  return 0;
}
