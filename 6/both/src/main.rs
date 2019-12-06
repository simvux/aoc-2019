use std::collections::HashMap;
use std::io;
use std::io::BufRead;

fn main() {
    let mut objects = HashMap::new();

    for line in io::stdin().lock().lines() {
        let line = line.unwrap();
        let mut spl = line.split(')');
        let left = spl.next().unwrap();
        let right = spl.next().unwrap();
        objects.insert(right.to_owned(), left.to_owned());
    }

    let mut total = 0;
    for v in objects.keys() {
        total += count(&objects, v);
    }

    println!("{}", total);
    println!("{:?}", find_san(&objects));
}

fn count(objects: &HashMap<String, String>, direct: &str) -> usize {
    if &objects[direct] == "COM" {
        return 1;
    }
    1 + count(objects, &objects[direct])
}

fn parents<'a>(objects: &'a HashMap<String, String>, direct: &str) -> Vec<&'a str> {
    match objects.get(direct) {
        None => Vec::new(),
        Some(obj) => {
            let mut parents = parents(objects, obj);
            parents.push(obj);
            parents
        }
    }
}

fn find_san(objects: &HashMap<String, String>) -> usize {
    let you = parents(&objects, "YOU")
        .into_iter()
        .rev()
        .collect::<Vec<_>>();
    let san = parents(&objects, "SAN")
        .into_iter()
        .rev()
        .collect::<Vec<_>>();
    let i = you.iter().position(|a| san.contains(a)).unwrap();
    i + san.iter().position(|a| *a == you[i]).unwrap()
}
