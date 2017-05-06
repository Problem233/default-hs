use std::collections::HashMap;

pub fn mean(nums: &Vec<i64>) -> f64 {
  let mut sum = 0i64;
  for x in nums {
    sum += *x;
  }
  sum as f64 / nums.len() as f64
}

pub fn median(nums: &Vec<i64>) -> f64 {
  let length = nums.len();
  if length % 2 != 0 {
    nums[(length + 1) / 2 - 1] as f64
  } else {
    (nums[length / 2] + nums[length / 2 - 1]) as f64 / 2f64
  }
}

pub fn mode(nums: &Vec<i64>) -> Vec<i64> {
  let mut appear_times: HashMap<i64, usize> = HashMap::new();
  for x in nums {
    let times = match appear_times.get(x) {
      None => 1,
      Some(n) => n + 1
    };
    appear_times.insert(*x, times);
  }
  let mut max_time: usize = 0;
  for (_, time) in &appear_times {
    if time > &max_time {
      max_time = *time;
    }
  }
  let mut modes: Vec<i64> = Vec::new();
  for (x, time) in &appear_times {
    if time == &max_time {
      modes.push(*x);
    }
  }
  if modes.len() == appear_times.len() {
    Vec::new()
  } else {
    modes
  }
}
