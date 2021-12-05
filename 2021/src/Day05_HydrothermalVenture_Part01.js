
/*
--- Day 5: Hydrothermal Venture ---
You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.

They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:

0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:

An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.

So, the horizontal and vertical lines from the above list would produce the following diagram:

.......1..
..1....1..
..1....1..
.......1..
.112111211
..........
..........
..........
..........
222111....
In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.

To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.

Consider only horizontal and vertical lines. At how many points do at least two lines overlap?
 */

// running with: `node ./2021/src/Day05_HydrothermalVenture_Part01.js

var lineReader = require('readline').createInterface({
    input: require('fs').createReadStream('./2021/resources/day05.in')
});

let size = 1000
let matrix = Array(size)
for (let i = 0; i < size; ++i) {
    matrix[i] = Array(size).fill(0);
}

const readLines = async () => {
    for await (const line of lineReader) {
        let [from, to] = line.split(' -> ')
        // console.log('Line from file:', line);
        // console.log('from to:', from,to);
        let [x1, y1] = from.split(',')
        let [x2, y2] = to.split(',')
        // console.log('from:', x1,y1);
        // console.log('to:', x2,y2);

        // horizontal?
        if (y1 === y2) {
            for (let x = Math.min(x1,x2); x <= Math.max(x1,x2); x++) {
                matrix[y1][x]++;
            }
            // vertical?
        } else if (x1 === x2) {
            for (let y = Math.min(y1,y2); y <= Math.max(y1,y2); y++) {
                matrix[y][x1]++;
            }
            // neither? <-> diagonal?
        } else {
            // should be ignored
        }

    }
}

readLines().then(() => {

    //console.log('Matrix:', matrix);
    // console.log('Done');

    let num_dangerous_points = 0;

    matrix.forEach((row, y) => {
        row.forEach((value, x) => {
            if (value > 1) {
                num_dangerous_points++;
                console.log('Danger at (', x, ', ', y, '): ', value);
            }
        })
    })

    console.log('Dangerous points: ', num_dangerous_points);

})

// lineReader.on('line', function (line) {
//     console.log('Line from file:', line);
// });
// console.log('Matrix:', matrix);

// const fs = require('fs')
//
// const data = fs.readFileSync(', 'utf8')


