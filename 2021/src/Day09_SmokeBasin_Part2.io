
/*
--- Part Two ---
Next, you need to find the largest basins so you know what areas are most important to avoid.

A basin is all locations that eventually flow downward to a single low point. Therefore, every low point has a basin, although some basins are very small. Locations of height 9 do not count as being in any basin, and all other locations will always be part of exactly one basin.

The size of a basin is the number of locations within the basin, including the low point. The example above has four basins.

The top-left basin, size 3:

2199943210
3987894921
9856789892
8767896789
9899965678
The top-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678
The middle basin, size 14:

2199943210
3987894921
9856789892
8767896789
9899965678
The bottom-right basin, size 9:

2199943210
3987894921
9856789892
8767896789
9899965678
Find the three largest basins and multiply their sizes together. In the above example, this is 9 * 14 * 9 = 1134.

What do you get if you multiply together the sizes of the three largest basins?
*/

// run with: `io ./2021/src/Day09_SmokeBasin_Part2.io`

Point := Object clone do(
    x ::= nil
    y ::= nil
    mapKey := method("#{x}-#{y}" interpolate)
)

makePoint := method(i, j,
    Point clone setX(j) setY(i)
)

isLowPoint := method(heightMap, i, j,
    cur := heightMap at(i) at(j)
    // top
    if(i > 0 and heightMap at(i-1) at(j) <= cur) then(return(false))
    // bottom
    if(i < 99 and heightMap at(i+1) at(j) <= cur) then(return(false))
    // left
    if(j > 0 and heightMap at(i) at(j-1) <= cur) then(return(false))
    //right := heightMap at(i) at(j+1)
    if(j < 99 and heightMap at(i) at(j+1) <= cur) then(return(false))
    true
)

basinSize := method(heightMap, initialI, initialJ,
    initialPoint := makePoint(initialI, initialJ)
    nextPoints := List clone append(initialPoint)
    visitedPoints := Map clone

    while ((nextPoints size) > 0,
        // ("Points remaining (" .. (nextPoints size) .. "): " .. nextPoints map(mapKey)) println
        nextPoint := nextPoints pop
        i := nextPoint y
        j := nextPoint x
        key := nextPoint mapKey
        //("Next point: " .. nextPoint) println
        //("Map key: " .. nextPoint mapKey) println
        visitedPoints hasKey(key) ifFalse(
            visitedPoints atPut(key, nextPoint)
            height := heightMap at(nextPoint y) at(nextPoint x)
            // ("Height: " .. height) println

            // top
            if(i > 0 and heightMap at(i-1) at(j) < 9 and heightMap at(i-1) at(j) > height) then(nextPoints append(makePoint(i-1, j)))
            // bottom
            if(i < 99 and heightMap at(i+1) at(j) < 9 and heightMap at(i+1) at(j) > height) then(nextPoints append(makePoint(i+1, j)))
            // left
            if(j > 0 and heightMap at(i) at(j-1) < 9 and heightMap at(i) at(j-1) > height) then(nextPoints append(makePoint(i, j-1)))
            //right := heightMap at(i) at(j+1)
            if(j < 99 and heightMap at(i) at(j+1) < 9 and heightMap at(i) at(j+1) > height) then(nextPoints append(makePoint(i, j+1)))
        )
    )
    ("Basin size: " .. visitedPoints size) println
    visitedPoints size
)

inputFile := File with("./2021/resources/day09.in")
inputFile openForReading
inputLines := (inputFile readLines)
inputFile close

heightMap := List clone

for(i, 0, 99,
    heightMap append(List clone)
    for(j, 0, 99,
        heightMap at(i) append(inputLines at(i) at(j) asCharacter asNumber)
    )
)


lowPoints := List clone
for(i, 0, 99,
    for(j, 0, 99,
        isLowPoint(heightMap, i, j) ifTrue(lowPoints append(Point clone setX(j) setY(i)))
    )
)

(lowPoints size) println
product := method(reduce(*))
// find the size of all basins, sort it, get the last 3, and multiply them
basinSizes := lowPoints map(point, basinSize(heightMap, point y, point x))
basinSizes sort println
basinSizes sort slice(-3) println
basinSizes sort slice(-3) product println

