use std::collections::HashMap;

pub fn example_hashmap() -> HashMap<String, i32> {
  let mut hashmap: HashMap<String, i32> = HashMap::new();
  hashmap.insert(String::from("one"), 1);
  hashmap.insert(String::from("two"), 2);
  hashmap.insert(String::from("three"), 3);
  hashmap
}

pub fn get_from_hashmap() {
  match example_hashmap().get("two") {
    None => println!("Can not get two!"),
    Some(x) => println!("two: {}", x)
  };
}

pub fn iter_in_hashmap() {
  for (name, num) in example_hashmap() {
    println!("{}: {}", name, num);
  }
}

pub fn update_a_hashmap() {
  let mut hashmap = example_hashmap();
  hashmap.insert(String::from("four"), 4);
  hashmap.entry(String::from("three")).or_insert(3);
  println!("{:?}", hashmap);
}
