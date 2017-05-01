extern crate rand;

pub mod lib;

use lib::trpl::fib::*;

fn main() {
  let fib_vec = fib2(&1, &1, &70);
  for x in fib_vec {
    print!("{} ", x);
  }
  println!();
}
