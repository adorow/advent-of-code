
/*
--- Part Two ---
Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider diagonal lines.

Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:

An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
Considering all lines from the above example would now produce the following diagram:

1.1....11.
.111...2..
..2.1.111.
...1.2.2..
.112313211
...1.2....
..1...1...
.1.....1..
1.......1.
222111....
You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.

Consider all of the lines. At how many points do at least two lines overlap?
 */

// running with: `node ./2021/src/Day05_HydrothermalVenture_Part02.js`

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
        let [x1, y1] = from.split(',').map((str) => parseInt(str))
        let [x2, y2] = to.split(',').map((str) => parseInt(str))

        let distance = Math.max(Math.abs(x1 - x2), Math.abs(y1 - y2));
        let xinc = (x1 === x2) ? 0 : ((x1 < x2) ? 1 : -1);
        let yinc = (y1 === y2) ? 0 : (y1 < y2) ? 1 : -1;
        // console.log("X1: ", x1);
        // console.log("X2: ", x2);
        // console.log("Y1: ", y1);
        // console.log("Y2: ", y2);
        // console.log("Distance: ", distance);
        // console.log("xinc: ", xinc);
        // console.log("yinc: ", yinc);
        for (let step = 0; step <= distance; step++) {
            //console.log("Marking(", x1 + (xinc * step), ", ", y1 + (yinc * step), ")");
            matrix[y1 + (yinc * step)][x1 + (xinc * step)]++;
        }
    }
}

readLines().then(() => {
    //console.log('Matrix:', matrix);

    let num_dangerous_points = 0;

    matrix.forEach((row, y) => {
        row.forEach((value, x) => {
            if (value > 1) {
                num_dangerous_points++;
                //console.log('Danger at (', x, ', ', y, '): ', value);
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


