let x: i32 = 42;
let y : &mut i32 = &mut x;
let z : &mut i32 = &mut *y;
let r : i32 = *z;

