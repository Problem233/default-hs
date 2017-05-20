extern crate rand;
extern crate permutohedron;

pub mod lib;

use lib::codewars::*;

fn main() {
  println!("{:?}", diophantine_equation(90000001));
}
