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
