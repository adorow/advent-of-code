
/*
--- Part Two ---
It seems like the individual flashes aren't bright enough to navigate. However, you might have a better option: the flashes seem to be synchronizing!

In the example above, the first time all octopuses flash simultaneously is step 195:

After step 193:
5877777777
8877777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777
7777777777

After step 194:
6988888888
9988888888
8888888888
8888888888
8888888888
8888888888
8888888888
8888888888
8888888888
8888888888

After step 195:
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
If you can calculate the exact moments when the octopuses will all flash simultaneously, you should be able to navigate through the cavern. What is the first step during which all octopuses flash?
 */

// run: Using deno (https://deno.land/): `deno run --allow-read  ./2021/src/Day11_DumboOctopus_Part2.ts`

const text = await Deno.readTextFile("./2021/resources/day11.in");
const lines = text.split("\n");
//console.log(lines);

class Point {
    x: number;
    y: number;

    constructor(x: number, y: number) {
        this.x = x;
        this.y = y;
    }
}

let grid: number[][] = [];
for (let i = 0; i < 10; i++) {
    grid.push([]);
    for (let j = 0; j < 10; j++) {
        grid[i].push(Number(lines[i][j]));
    }
}
//console.log(grid);

for (let simulation = 1; simulation < 1000; simulation++) {
    let flashers: Point[] = [];
    let flashedInThisTurn = 0;
    //let nextGrid = cloneGrid(grid);
    // 1. increase all levels by one
    for (let i = 0; i < 10; i++) {
        for (let j = 0; j < 10; j++) {
            if (++grid[i][j] > 9) {
                flashers.push(new Point(j, i));
            }
        }
    }
    // 2. any octopus that went above 9 'flashes', increasing the count of everyone around by 1 (might trigger more flashes)
    while (flashers.length > 0) {
        let flasher = flashers.pop();
        if (flasher == undefined) throw "shouldn't happen";
        flashedInThisTurn++;

        for (let di = -1; di <= 1; di++) {
            for (let dj = -1; dj <= 1; dj++) {
                if (di == 0 && dj == 0) continue;
                let newy = flasher.y + di;
                let newx = flasher.x + dj;
                if (newx >= 0 && newx < 10 && newy >= 0 && newy < 10) {
                    // will it flash now?
                    if (grid[newy][newx] == 9) {
                        flashers.push(new Point(newx, newy));
                    }
                    grid[newy][newx]++;
                }
            }
        }
    }

    if (flashedInThisTurn == 100) {
        console.log("All Octopuses flashed on turn ", simulation);
        break;
    }

    // 3. every octopus that flashed (>9) is set to 0
    for (let i = 0; i < 10; i++) {
        for (let j = 0; j < 10; j++) {
            if (grid[i][j] > 9) {
                grid[i][j] = 0;
            }
        }
    }
}



