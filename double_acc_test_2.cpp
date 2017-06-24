#include <iostream>

using namespace std;

int main() {
  double num = 0.5;
  for (int i = 1; i < 1074; i++) num = num / 2;
  printf("2^-1074 = %.1100f\n\n", num);
  return 0;
}
