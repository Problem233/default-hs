#include <iostream>
#include <random>
#include <functional>

using namespace std;

int main() {
  default_random_engine rand_eng(__rdtsc());
  uniform_int_distribution<int> rand_op_dist(0, 2);
  auto rand_op = bind(rand_op_dist, rand_eng);
  uniform_real_distribution<double> rand_num_dist(-1e12, 1e12);
  auto rand_num = bind(rand_num_dist, rand_eng);

  double num = rand_num();
  printf("initital value: %.32g\n\n", num);
  for (int i = 0; i < 100; i++) {
    double n = rand_num();
    switch (rand_op()) {
      case 0: printf("* %.32g = %.2048f\n\n", n, num = num * n); break;
      case 1:
      case 2: printf("/ %.32g = %.2048f\n\n", n, num = num / n); break;
    }
  }
}
