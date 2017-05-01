extern crate rand;

pub mod lib;

use lib::trpl::geometry::*;

fn main() {
  println!("{}", Rectangle::new(&1600, &900).area());
}
