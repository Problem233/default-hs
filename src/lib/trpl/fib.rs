pub fn fib1(x: &i32, y: &i32, length: &usize) {
  let mut a = *x;
  let mut b = *y;
  for _ in 1..*length {
    print!("{} ", a);
    let next = a + b;
    a = b;
    b = next;
  }
  println!();
}

pub fn fib2(x: &i64, y: &i64, length: &usize) -> Vec<i64> {
  let mut a = *x;
  let mut b = *y;
  let mut fib_vec: Vec<i64> = Vec::new();
  for _ in 1..*length {
    fib_vec.push(a);
    let next = a + b;
    a = b;
    b = next;
  }
  fib_vec
}
