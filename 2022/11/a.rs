
// Disable Copy (hence implicit pass-by-value) to get compiler guarantees that
// moving moves.
struct Item {worry: usize}

enum Operation {
    Add(usize),
    Mul(usize)
}
use Operation::*;
use std::rc::Rc;
use std::cell::RefCell;

impl Operation {
    fn as_fn(self) -> Box<dyn FnMut(&mut Item)> {
        match self {
            Add(x) => Box::new(move |i: &mut Item| i.worry += x),
            Mul(x) => Box::new(move |i: &mut Item| i.worry *= x)
        }
    }
}

struct Test(usize);
impl Test {
    fn as_fn(self) -> impl FnMut(&Item) -> bool {
        move |i| i.worry % self.0 == 0
    }
}

struct Monkey {
    items: Vec<Item>,
    operation: Box<dyn FnMut(&mut Item)>,
    test: Box<dyn FnMut(&Item) -> bool>,
    targets: [Rc<RefCell<Monkey>>; 2]
}

impl Monkey {
    fn pass(&mut self, i: Item) {
        self.items.push(i)
    }

    fn update(&mut self) {
        for mut item in self.items.drain(..) {
            (self.operation)(&mut item);
            item.worry /= 3;
            self.targets[(self.test)(&item) as usize].borrow_mut().pass(item);
        }
    }
}

fn main() {
    println!("Hello");
}
