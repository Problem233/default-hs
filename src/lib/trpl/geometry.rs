#[derive(Debug)]
pub struct Rectangle {
  pub width: u32,
  pub height: u32
}

impl Rectangle {

  pub fn new(width: &u32, height: &u32) -> Rectangle {
    Rectangle {
      width: *width,
      height: *height
    }
  }

  pub fn square(size: &u32) -> Rectangle {
    Rectangle::new(size, size)
  }

  pub fn area(&self) -> u32 {
    self.width * self.height
  }

  pub fn can_hold(&self, other: &Rectangle) -> bool {
    self.height >= other.height && self.width >= other.width
  }

}
