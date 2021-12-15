/*
--- Day 15: Chiton ---
You've almost reached the exit of the cave, but the walls are getting closer together. Your submarine can barely still fit, though; the main problem is that the walls of the cave are covered in chitons, and it would be best not to bump any of them.

The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The shape of the cavern resembles a square; a quick scan of chiton density produces a map of risk level throughout the cave (your puzzle input). For example:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its risk level; to determine the total risk of an entire path, add up the risk levels of each position you enter (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).

Your goal is to find a path with the lowest total risk. In this example, a path with the lowest total risk is highlighted here:

1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
The total risk of this path is 40 (the starting position is never entered, so its risk is not counted).

What is the lowest total risk of any path from the top left to the bottom right?
*/
// run with: `rustc -o runner ./2021/src/Day15_Chiton_Part1.rs && ./runner`

use std::fs;
use std::cmp::{max,Ordering};
use std::collections::BinaryHeap;
use std::io::{BufRead, BufReader};
use std::vec::{Vec};

fn read_board() -> Vec<Vec<u32>> {
    let file = fs::File::open("./2021/resources/day15.in").unwrap();
    let reader = BufReader::new(file);

    let mut board = Vec::new();
    // Read the file line by line using the lines() iterator from std::io::BufRead.
    for (_, line) in reader.lines().enumerate() {
        let mut row = Vec::new();
        let line = line.unwrap(); // Ignore errors.
        for c in line.chars() {
            row.push(c.to_digit(10).unwrap());
        }
        // Show the line and its number.
        //println!("{}. {}", index + 1, line);

        board.push(row);
    }
    return board;
}

fn create_costs() -> Vec<Vec<u32>> {
    let mut vec = Vec::with_capacity(100);
    for _i in 0..100 {
        let mut row_vec = Vec::with_capacity(100);
        row_vec.resize(100, 1000000);
        vec.push(row_vec);
    }
    return vec;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Step {
    x: u32,
    y: u32,
    cost: u32,
    steps: u32,
}

impl Ord for Step {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
            .then_with(|| self.x.cmp(&other.x))
            .then_with(|| self.y.cmp(&other.y))
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for Step {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn main() {
    let board: Vec<Vec<u32>> = read_board();

    // keep track of the lowest cost in a given position
    let mut lowest_costs = create_costs();
    let mut lowest_cost_steps = create_costs();

    let mut queue = BinaryHeap::new(); //Vec::with_capacity(10000);
    queue.push(Step {
        x: 0,
        y: 0,
        cost: 0,
        steps: 0,
    });

    let mut max_queue_size = 0;

    println!("Started");
    while let Some(Step { x, y, cost, steps }) = queue.pop() {
        max_queue_size = max(max_queue_size, queue.len());
        //println!("{:?}", entry);
        if cost < lowest_costs[y as usize][x as usize] {
            // println!("Set ({}, {}) to {}", x, y, cost);
            lowest_costs[y as usize][x as usize] = cost;
            lowest_cost_steps[y as usize][x as usize] = steps;
            if x == 99 && y == 99 {
                // don't add next steps
                continue;
            }

            // go down (from map)
            if y > 0 {
                queue.push(Step {
                    x: x,
                    y: y - 1,
                    cost: cost + board[(y - 1) as usize][x as usize],
                    steps: steps + 1,
                });
            }
            // go up (from map)
            if y < 99 {
                queue.push(Step {
                    x: x,
                    y: y + 1,
                    cost: cost + board[(y + 1) as usize][x as usize],
                    steps: steps + 1,
                });
            }
            // go left
            if x > 0 {
                queue.push(Step {
                    x: x - 1,
                    y: y,
                    cost: cost + board[y as usize][(x - 1) as usize],
                    steps: steps + 1,
                });
            }
            // go right
            if x < 99 {
                queue.push(Step {
                    x: x + 1,
                    y: y,
                    cost: cost + board[y as usize][(x + 1) as usize],
                    steps: steps + 1,
                });
            }
        }
    }
    println!("Done");

    //println!("Lowest costs: {:?}", &lowest_costs);
    //println!("Lowest costs steps: {:?}", &lowest_cost_steps);
    let lowest_risk_level = lowest_costs[99][99];
    println!("Lowest risk level: {}", &lowest_risk_level);

    println!("Max ever queue size: {:?}", &max_queue_size);
}
