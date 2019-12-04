use std::collections::HashSet;

fn main() {
    const START: usize = 246_540;
    const END: usize = 787_419;

    let one = Bruter {
        start: START,
        end: END,
        position: 0,
        matching: HashSet::new(),
    }
    .part_one();
    let two = Bruter {
        start: START,
        end: END,
        position: 0,
        matching: HashSet::new(),
    }
    .part_two();
    println!("one: {} | two: {}", one.len(), two.len());
}

struct Bruter {
    matching: HashSet<usize>,
    start: usize,
    end: usize,
    position: usize,
}

fn to_digits(n: usize) -> Vec<usize> {
    fn x_inner(n: usize, xs: &mut Vec<usize>) {
        if n >= 10 {
            x_inner(n / 10, xs);
        }
        xs.push(n % 10);
    }
    let mut xs = Vec::new();
    x_inner(n, &mut xs);
    xs
}

impl Bruter {
    fn next(&mut self, allow_3x: bool) {
        let num = self.start + self.position;
        let digits = to_digits(num);
        let mut valid_dupe = 0;
        for (i, n) in digits.iter().cloned().enumerate() {
            if i == 0 {
                continue;
            }
            // ISSUE:
            // I'm checking against ALL others instead of just the ones next to it.
            let prev = digits[i - 1];
            if prev == n {
                if !allow_3x {
                    valid_dupe += 1;
                    for (j, d) in digits.iter().cloned().enumerate() {
                        if d == n && j != i - 1 && j != i {
                            valid_dupe -= 1;
                            break;
                        }
                    }
                } else {
                    valid_dupe += 1;
                }
            }
            if n < prev {
                self.position += 1;
                return;
            }
        }
        if valid_dupe > 0 {
            self.matching.insert(num);
        }
        self.position += 1;
    }

    fn run(mut self, allow_3x: bool) -> HashSet<usize> {
        while self.start + self.position < self.end {
            self.next(allow_3x);
        }
        self.matching
    }
    fn part_one(self) -> HashSet<usize> {
        self.run(true)
    }
    fn part_two(self) -> HashSet<usize> {
        self.run(false)
    }
}
