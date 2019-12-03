use std::collections::HashSet;
use std::fs;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
struct Position {
    x: isize,
    y: isize,
}
impl Position {
    fn mv<M: Movement>(&mut self, m: M) {
        let (x, y) = m.offset();
        self.x += x;
        self.y += y;
    }
    fn distance(&self) -> isize {
        isize::abs(self.x) + isize::abs(self.y)
    }
}
trait Movement {
    fn offset(self) -> (isize, isize);
}
#[derive(Debug)]
enum Edit {
    Up(isize),
    Right(isize),
}
impl Movement for Edit {
    fn offset(self) -> (isize, isize) {
        match self {
            Edit::Up(n) => (0, n),
            Edit::Right(n) => (n, 0),
        }
    }
}

impl From<&str> for Edit {
    fn from(s: &str) -> Edit {
        let mut chars = s.chars();
        let delim = chars.next().unwrap();
        let amount = chars.collect::<String>().parse::<isize>().unwrap();
        match delim {
            'R' => Edit::Right(amount),
            'L' => Edit::Right(-amount),
            'U' => Edit::Up(amount),
            'D' => Edit::Up(-amount),
            _ => panic!("Unrecognized direction `{}`", delim),
        }
    }
}

const INPUT_PATH: &str = "input.txt";

fn main() {
    let raw = fs::read_to_string(INPUT_PATH).unwrap();
    let mut raw_iter = raw.split('\n');
    let first = Line::from(raw_iter.next().unwrap().split(',').map(Edit::from)).named("first");
    let second = Line::from(raw_iter.next().unwrap().split(',').map(Edit::from)).named("second");
    let mut intersections = seek_intersect(first, second);
    intersections.sort_by(|a, b| a.distance().partial_cmp(&b.distance()).unwrap());
    println!("{:#?}", intersections);
}

struct Line<I: Iterator<Item = Edit>> {
    source: I,
    name: String,
    current: Position,
    history: HashSet<Position>,
}
impl<I: Iterator<Item = Edit>> From<I> for Line<I> {
    fn from(source: I) -> Self {
        Self {
            source,
            name: String::new(),
            current: Position { x: 0, y: 0 },
            history: HashSet::new(),
        }
    }
}
impl<I: Iterator<Item = Edit>> Line<I> {
    fn forward(&mut self) -> Option<Position> {
        let edit = self.source.next()?;
        let mut pos = self.current.clone();
        println!("moving {} with {:?}", self.name, &edit);
        print!("{:?} -> ", &pos);
        pos.mv(edit);
        println!("{:?}", &pos);

        self.current = pos.clone();
        self.history.insert(pos.clone());
        Some(pos)
    }
    fn complete(&mut self) {
        while let Some(_) = self.forward() {}
    }
    fn named(mut self, name: &str) -> Self {
        self.name = name.to_owned();
        self
    }
}

fn seek_intersect<I: Iterator<Item = Edit>>(
    mut first: Line<I>,
    mut second: Line<I>,
) -> Vec<Position> {
    first.complete();
    let mut intersections = Vec::new();

    while let Some(position) = second.forward() {
        if first.history.contains(&position) {
            intersections.push(position);
        }
    }
    intersections
}
