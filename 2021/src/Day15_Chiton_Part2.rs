/*
--- Part Two ---
Now that you know how to find low-risk paths in the cave, you can try to find your way out.

The entire cave is actually five times larger in both dimensions than you thought; the area you originally scanned is just one tile in a 5x5 tile area that forms the full map. Your original map tile repeats to the right and downward; each time the tile repeats to the right or downward, all of its risk levels are 1 higher than the tile immediately up or left of it. However, risk levels above 9 wrap back around to 1. So, if your original map had some position with a risk level of 8, then that same position on each of the 25 total tiles would be as follows:

8 9 1 2 3
9 1 2 3 4
1 2 3 4 5
2 3 4 5 6
3 4 5 6 7
Each single digit above corresponds to the example position with a value of 8 on the top-left tile. Because the full map is actually five times larger in both dimensions, that position appears a total of 25 times, once in each duplicated tile, with the values shown above.

Here is the full five-times-as-large version of the first example above, with the original map in the top left corner highlighted:

11637517422274862853338597396444961841755517295286
13813736722492484783351359589446246169155735727126
21365113283247622439435873354154698446526571955763
36949315694715142671582625378269373648937148475914
74634171118574528222968563933317967414442817852555
13191281372421239248353234135946434524615754563572
13599124212461123532357223464346833457545794456865
31254216394236532741534764385264587549637569865174
12931385212314249632342535174345364628545647573965
23119445813422155692453326671356443778246755488935
22748628533385973964449618417555172952866628316397
24924847833513595894462461691557357271266846838237
32476224394358733541546984465265719557637682166874
47151426715826253782693736489371484759148259586125
85745282229685639333179674144428178525553928963666
24212392483532341359464345246157545635726865674683
24611235323572234643468334575457944568656815567976
42365327415347643852645875496375698651748671976285
23142496323425351743453646285456475739656758684176
34221556924533266713564437782467554889357866599146
33859739644496184175551729528666283163977739427418
35135958944624616915573572712668468382377957949348
43587335415469844652657195576376821668748793277985
58262537826937364893714847591482595861259361697236
96856393331796741444281785255539289636664139174777
35323413594643452461575456357268656746837976785794
35722346434683345754579445686568155679767926678187
53476438526458754963756986517486719762859782187396
34253517434536462854564757396567586841767869795287
45332667135644377824675548893578665991468977611257
44961841755517295286662831639777394274188841538529
46246169155735727126684683823779579493488168151459
54698446526571955763768216687487932779859814388196
69373648937148475914825958612593616972361472718347
17967414442817852555392896366641391747775241285888
46434524615754563572686567468379767857948187896815
46833457545794456865681556797679266781878137789298
64587549637569865174867197628597821873961893298417
45364628545647573965675868417678697952878971816398
56443778246755488935786659914689776112579188722368
55172952866628316397773942741888415385299952649631
57357271266846838237795794934881681514599279262561
65719557637682166874879327798598143881961925499217
71484759148259586125936169723614727183472583829458
28178525553928963666413917477752412858886352396999
57545635726865674683797678579481878968159298917926
57944568656815567976792667818781377892989248891319
75698651748671976285978218739618932984172914319528
56475739656758684176786979528789718163989182927419
67554889357866599146897761125791887223681299833479
Equipped with the full map, you can now find a path from the top left corner to the bottom right corner with the lowest total risk:

11637517422274862853338597396444961841755517295286
13813736722492484783351359589446246169155735727126
21365113283247622439435873354154698446526571955763
36949315694715142671582625378269373648937148475914
74634171118574528222968563933317967414442817852555
13191281372421239248353234135946434524615754563572
13599124212461123532357223464346833457545794456865
31254216394236532741534764385264587549637569865174
12931385212314249632342535174345364628545647573965
23119445813422155692453326671356443778246755488935
22748628533385973964449618417555172952866628316397
24924847833513595894462461691557357271266846838237
32476224394358733541546984465265719557637682166874
47151426715826253782693736489371484759148259586125
85745282229685639333179674144428178525553928963666
24212392483532341359464345246157545635726865674683
24611235323572234643468334575457944568656815567976
42365327415347643852645875496375698651748671976285
23142496323425351743453646285456475739656758684176
34221556924533266713564437782467554889357866599146
33859739644496184175551729528666283163977739427418
35135958944624616915573572712668468382377957949348
43587335415469844652657195576376821668748793277985
58262537826937364893714847591482595861259361697236
96856393331796741444281785255539289636664139174777
35323413594643452461575456357268656746837976785794
35722346434683345754579445686568155679767926678187
53476438526458754963756986517486719762859782187396
34253517434536462854564757396567586841767869795287
45332667135644377824675548893578665991468977611257
44961841755517295286662831639777394274188841538529
46246169155735727126684683823779579493488168151459
54698446526571955763768216687487932779859814388196
69373648937148475914825958612593616972361472718347
17967414442817852555392896366641391747775241285888
46434524615754563572686567468379767857948187896815
46833457545794456865681556797679266781878137789298
64587549637569865174867197628597821873961893298417
45364628545647573965675868417678697952878971816398
56443778246755488935786659914689776112579188722368
55172952866628316397773942741888415385299952649631
57357271266846838237795794934881681514599279262561
65719557637682166874879327798598143881961925499217
71484759148259586125936169723614727183472583829458
28178525553928963666413917477752412858886352396999
57545635726865674683797678579481878968159298917926
57944568656815567976792667818781377892989248891319
75698651748671976285978218739618932984172914319528
56475739656758684176786979528789718163989182927419
67554889357866599146897761125791887223681299833479
The total risk of this path is 315 (the starting position is still never entered, so its risk is not counted).

Using the full map, what is the lowest total risk of any path from the top left to the bottom right?
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

fn read_larger_board() -> Vec<Vec<u32>> {
    let original_board = read_board();

    let mut large_board = Vec::new();

    for vertical in 0..5 {
        for i in 0..100 {
            let mut row = Vec::new();
            for horizontal in 0..5 {
                for j in 0..100 {
                    let v = original_board[i][j];
                    let mut new_v = v + vertical + horizontal;
                    if new_v > 9 {
                        new_v -= 9;
                    }
                    row.push(new_v);

                }
            }
            large_board.push(row);
        }
    }

    return large_board;
}

fn create_larger_costs() -> Vec<Vec<u32>> {
    let mut vec = Vec::with_capacity(500);
    for _i in 0..500 {
        let mut row_vec = Vec::with_capacity(500);
        row_vec.resize(500, 100000000);
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
    let board: Vec<Vec<u32>> = read_larger_board();
    //let board: Vec<Vec<u32>> = read_board();

    let end_x = 499;
    let end_y = 499;

    println!("Board height: {}, width: {}", board.len(), board[0].len());

    //println!("Board {:?}", board);

    // keep track of the lowest cost in a given position
    let mut lowest_costs = create_larger_costs();
    let mut lowest_cost_steps = create_larger_costs();

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
            if x == end_x && y == end_y {
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
            if y < end_y {
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
            if x < end_x {
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
    let lowest_risk_level = lowest_costs[end_y as usize][end_x as usize];
    println!("Lowest risk level: {}", &lowest_risk_level);
    println!("Steps required: {}", lowest_cost_steps[end_y as usize][end_x as usize]);

    println!("Max ever queue size: {:?}", &max_queue_size);
}
