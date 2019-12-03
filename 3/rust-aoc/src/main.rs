use std::collections::HashMap;
use std::fs;

const INPUT_PATH: &str = "input.txt";

fn main() {
    let raw = fs::read_to_string(INPUT_PATH).unwrap();
    let mut raw_iter = raw.split('\n');
    let first = Drawer::from(raw_iter.next().unwrap().split(',').map(Edit::from));
    let second = Drawer::from(raw_iter.next().unwrap().split(',').map(Edit::from));
    println!("{}", part_one(first.clone(), second.clone()));
    println!("{}", part_two(first, second));
}

fn part_one<I: Iterator<Item = Edit>>(first: Drawer<I>, second: Drawer<I>) -> isize {
    let mut intersections = seek_intersect(first, second);
    intersections.sort_by(|(a, _), (b, _)| {
        (a.0.abs() + a.1.abs())
            .partial_cmp(&(b.0.abs() + b.1.abs()))
            .unwrap()
    });
    let pos = intersections.first().unwrap().0;
    pos.0.abs() + pos.1.abs()
}

fn part_two<I: Iterator<Item = Edit>>(first: Drawer<I>, second: Drawer<I>) -> isize {
    let mut intersections = seek_intersect(first, second);
    intersections.sort_by(
        |(_, (first_steps_i, second_steps_i)), (_, (first_steps_j, second_steps_j))| {
            (*first_steps_i + *second_steps_i)
                .partial_cmp(&(*first_steps_j + *second_steps_j))
                .unwrap()
        },
    );
    let pos = intersections.first().unwrap().1;
    pos.1 + pos.0
}

enum Direction {
    Left,
    Right,
    Up,
    Down,
}
struct Edit {
    direction: Direction,
    amount: isize,
}
impl From<&str> for Edit {
    fn from(s: &str) -> Edit {
        let mut chars = s.chars();
        let delim = chars.next().unwrap();
        let amount = chars.collect::<String>().parse::<isize>().unwrap();
        assert_ne!(amount, 0);
        let direction = match delim {
            'R' => Direction::Right,
            'L' => Direction::Left,
            'U' => Direction::Up,
            'D' => Direction::Down,
            _ => panic!("Unrecognized direction `{}`", delim),
        };
        Edit { amount, direction }
    }
}

#[derive(Clone)]
struct Drawer<I: Iterator<Item = Edit>> {
    source: I,
}
impl<I: Iterator<Item = Edit>> From<I> for Drawer<I> {
    fn from(source: I) -> Self {
        Self { source }
    }
}
impl<I: Iterator<Item = Edit>> Drawer<I> {
    fn complete<F: FnMut((isize, isize), isize)>(&mut self, mut f: F) {
        let mut position = (0, 0);
        let mut total_steps = 0;
        while let Some(edit) = self.source.next() {
            for _ in 0..edit.amount {
                use Direction::*;
                match edit.direction {
                    Left => position.0 -= 1,
                    Right => position.0 += 1,
                    Up => position.1 += 1,
                    Down => position.1 -= 1,
                }
                total_steps += 1;
                f(position, total_steps);
            }
        }
    }
}

fn seek_intersect<I: Iterator<Item = Edit>>(
    mut first: Drawer<I>,
    mut second: Drawer<I>,
) -> Vec<((isize, isize), (isize, isize))> {
    let mut first_history = HashMap::new();
    let history = &mut first_history;
    first.complete(|position, steps| {
        history.insert(position, steps);
    });

    let mut intersections = Vec::new();

    second.complete(|position, steps| {
        if let Some(first_steps) = first_history.get(&position).cloned() {
            intersections.push((position, (first_steps, steps)));
        }
    });
    intersections
}
