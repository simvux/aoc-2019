use std::collections::HashSet;
use std::io;
use std::io::Read;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Astroid {
    x: isize,
    y: isize,
}

impl Astroid {}

#[derive(Debug)]
struct Map {
    astroids: HashSet<Astroid>,
}

impl<I: Iterator<Item = Result<u8, io::Error>>> From<I> for Map {
    fn from(s: I) -> Map {
        let (mut x, mut y) = (0, 0);
        let mut astroids = HashSet::with_capacity(40);
        for c in s {
            match c {
                Ok(b'#') => {
                    astroids.insert(Astroid { x, y });
                    x += 1;
                }
                Ok(b'\n') => {
                    x = 0;
                    y += 1;
                }
                Ok(b'.') => x += 1,
                _ => panic!("Malformed input: {:?}", c),
            }
        }
        Map { astroids }
    }
}

fn main() {
    let map = Map::from(std::io::stdin().lock().bytes());
    let mut all_visibility = map
        .astroids
        .iter()
        .map(|ast| (ast, ast.visible_from(map.astroids.clone())))
        .collect::<Vec<(&Astroid, HashSet<Astroid>)>>();
    all_visibility.sort_by(|i, j| i.1.len().cmp(&j.1.len()));
    let sel = &all_visibility[0];
    println!("{:?} {}", sel.0, sel.1.len());
}
