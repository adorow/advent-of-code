
/*
--- Day 9: Smoke Basin ---
These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.

If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).

Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:

2199943210
3987894921
9856789892
8767896789
9899965678
Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.

Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)

In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.

The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.

Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?
*/

// run with: `io ./2021/src/Day09_SmokeBasin_Part1.io`

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

inputFile := File with("./2021/resources/day09.in")
inputFile openForReading
inputLines := (inputFile readLines)
inputFile close

heightMap := List clone

for(i, 0, 99,
    heightMap append(List clone)
    for(j, 0, 99,
        heightMap at(i) append(inputLines at(i) at(j) asCharacter asNumber)
        //"Looping i=#{i}, j=#{j}: #{inputLines at(i) at(j) asCharacter asNumber}" interpolate println
    )
)


lowPoints := List clone
for(i, 0, 99,
    for(j, 0, 99,
        isLowPoint(heightMap, i, j) ifTrue(lowPoints append(heightMap at(i) at(j)))
        //"Looping i=#{i}, j=#{j}: #{inputLines at(i) at(j) asCharacter asNumber}" interpolate println
    )
)

// the risk level is 1 + its height, so we simplify by doing the sum of values plus the size (+1 for ech entry)
(lowPoints sum + lowPoints size) println
