use std::fs;

fn parse_numeric(s: &str) -> (isize, isize) {
    let mut chars = s.chars();
    let delim = chars.next().unwrap();
    let amount = chars.collect::<String>().parse::<isize>().unwrap();
    match delim {
        'R' => (amount, 0),
        'L' => (-amount, 0),
        'U' => (0, amount),
        'D' => (0, -amount),
        _ => panic!("Unrecognized direction `{}`", delim),
    }
}

const INPUT_PATH: &str = "input.txt";

fn main() {
    let raw = fs::read_to_string(INPUT_PATH).unwrap();
    let mut raw_iter = raw.split('\n');
    let first = Drawer::from(raw_iter.next().unwrap().split(',').map(parse_numeric));
    let second = Drawer::from(raw_iter.next().unwrap().split(',').map(parse_numeric));
    let mut intersections = seek_intersect(first, second);
    intersections.sort_by(|a, b| {
        (a.0.abs() + a.1.abs())
            .partial_cmp(&(b.0.abs() + b.1.abs()))
            .unwrap()
    });
    println!("{:#?}", intersections);
}

type Coord = (isize, isize);

#[derive(Debug)]
struct Line {
    start: Coord,
    end: Coord,
}
impl Line {
    fn contains(&self, n: (isize, isize)) -> bool {
        let x = n.0 >= self.start.0 && n.0 <= self.end.0;
        let y = n.1 >= self.start.1 && n.1 <= self.end.1;
        x && y
    }
}
struct Drawer<I: Iterator<Item = Coord>> {
    source: I,
    last: Coord,
    history: Vec<Line>,
}
impl<I: Iterator<Item = Coord>> From<I> for Drawer<I> {
    fn from(source: I) -> Self {
        Self {
            source,
            last: (0, 0),
            history: Vec::new(),
        }
    }
}
impl<I: Iterator<Item = Coord>> Drawer<I> {
    fn forward(&mut self) -> Option<(isize, isize)> {
        let (off_x, off_y) = self.source.next()?;
        let previous = self.last;
        self.last = (previous.0 + off_x, previous.1 + off_y);
        let drawn = Line {
            start: previous,
            end: self.last,
        };
        self.history.push(drawn);
        Some(self.last)
    }
    fn complete(&mut self) {
        while let Some(_) = self.forward() {}
    }
}

fn seek_intersect<I: Iterator<Item = Coord>>(
    mut first: Drawer<I>,
    mut second: Drawer<I>,
) -> Vec<(isize, isize)> {
    first.complete();
    let mut intersections = Vec::new();

    while let Some(position) = second.forward() {
        for line in first.history.iter() {
            if line.contains(position) {
                intersections.push(position);
            }
            println!("{:?} bounds do not contain {:?}", line, position);
        }
    }
    intersections
}
