use std::io::Read;

const WIDTH: usize = 25;
const HEIGHT: usize = 6;

type Pixel = u8;

fn display_pixel(p: Pixel) -> u8 {
    match p {
        0 => b'.',
        1 => b'#',
        2 => b'@',
        _ => b'?',
    }
}

struct ImageBuilder<I: Iterator<Item = u8>> {
    source: I,
    buffer: Vec<Vec<Vec<Pixel>>>,
    l: usize,
}

impl<I: Iterator<Item = u8>> ImageBuilder<I> {
    fn new(source: I) -> Self {
        Self {
            buffer: vec![vec![Vec::new()]],
            l: 0,
            source,
        }
    }
    fn draw(mut self) -> Vec<Vec<Vec<Pixel>>> {
        while let Some(n) = self.source.next() {
            if n == b'\n' {
                continue;
            }
            let height = self.buffer[self.l].len();
            self.buffer[self.l][height - 1].push(n - 48);
            let width = self.buffer[self.l][height - 1].len();
            if width == WIDTH {
                if height == HEIGHT {
                    self.l += 1;
                    self.buffer.push(vec![Vec::new()]);
                } else {
                    self.buffer[self.l].push(Vec::new());
                }
            }
        }
        if self.buffer.last().unwrap()[0].is_empty() {
            self.buffer.pop();
        }
        self.buffer
    }
}

fn main() {
    let mut inp = String::with_capacity(200);
    std::io::stdin().read_to_string(&mut inp).unwrap();
    let image = ImageBuilder::new(inp.bytes()).draw();

    let selected_layer = image
        .iter()
        .min_by(|layeri: &&Vec<Vec<u8>>, layerj: &&Vec<Vec<u8>>| {
            let a: usize = occourances_of(layeri.as_slice(), 0);
            let b: usize = occourances_of(layerj.as_slice(), 0);
            a.cmp(&b)
        })
        .unwrap();
    println!(
        "part-1: {}",
        occourances_of(selected_layer, 1) * occourances_of(selected_layer, 2)
    );

    let rendered_part2 = render_part_two(&image);
    for row in rendered_part2 {
        println!(
            "{}",
            String::from_utf8(row.iter().map(|p| display_pixel(*p)).collect::<Vec<u8>>()).unwrap()
        );
    }
}

fn occourances_of(list: &[Vec<u8>], check: u8) -> usize {
    list.iter()
        .map(|r| r.iter().filter(|&n| *n == check).count())
        .sum()
}

fn render_part_two(image: &[Vec<Vec<u8>>]) -> Vec<Vec<u8>> {
    image[0]
        .iter()
        .enumerate()
        .map(|(ri, row)| {
            row.iter()
                .enumerate()
                .map(|(pi, _)| {
                    image
                        .iter()
                        .map(|l| l[ri][pi])
                        .find(|p| *p != 2)
                        .unwrap_or(2)
                })
                .collect::<Vec<u8>>()
        })
        .collect::<Vec<Vec<u8>>>()
}
