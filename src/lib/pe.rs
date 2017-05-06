pub fn answer9_1() -> u64 {
  for a in 1..500 {
    for b in a..500 {
      let c = 1000 - a - b;
      if a * a + b * b == c * c {
        return a * b * c;
      }
    }
  }
  panic!("couldn't find any answer!")
}

pub fn answer15_1() -> u64 {
  let mut f: [[u64; 20]; 20] = [[0; 20]; 20];
  for x in 0..20 {
    f[x][0] = x as u64 + 2;
    f[0][x] = x as u64 + 2;
  }
  for x in 1..20 {
    for y in 1..(x + 1) {
      f[x][y] = f[x - 1][y] + f[x][y - 1];
      f[y][x] = f[y - 1][x] + f[y][x - 1];
    }
  }
  f[20 - 1][20 - 1]
}