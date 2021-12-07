/*
--- Part Two ---
The crabs don't seem interested in your proposed solution. Perhaps you misunderstand crab engineering?

As it turns out, crab submarine engines don't burn fuel at a constant rate. Instead, each change of 1 step in horizontal position costs 1 more unit of fuel than the last: the first step costs 1, the second step costs 2, the third step costs 3, and so on.

As each crab moves, moving further becomes more expensive. This changes the best horizontal position to align them all on; in the example above, this becomes 5:

Move from 16 to 5: 66 fuel
Move from 1 to 5: 10 fuel
Move from 2 to 5: 6 fuel
Move from 0 to 5: 15 fuel
Move from 4 to 5: 1 fuel
Move from 2 to 5: 6 fuel
Move from 7 to 5: 3 fuel
Move from 1 to 5: 10 fuel
Move from 2 to 5: 6 fuel
Move from 14 to 5: 45 fuel
This costs a total of 168 fuel. This is the new cheapest possible outcome; the old alignment position (2) now costs 206 fuel instead.

Determine the horizontal position that the crabs can align to using the least fuel possible so they can make you an escape route! How much fuel must they spend to align to that position?
*/

// running: go run ./2021/src/Day07_TheTreacheryOfWhales_Part1.go
package main

import (
    "strings"
    "bufio"
    "fmt"
    "os"
    "log"
    "strconv"
//     "time"
)

func sum(array []int) int {
 result := 0
 for _, v := range array {
  result += v
 }
 return result
}
func min(array []int) int {
 min := array[0]
 for _, v := range array {
  if min > v {
    min = v
  }
 }
 return min
}
func max(array []int) int {
 max := array[0]
 for _, v := range array {
  if max < v {
    max = v
  }
 }
 return max
}
func singleCost(n int) int {
    return (n*(n+1))/2
}

func cost(array []int, pos int) int {
 cost := 0
 for _, v := range array {
  var n int
  if v > pos {
    n = v-pos
  } else {
    n = pos-v
  }
  if (n > 0) {
    cost += singleCost(n)
  }
  if (cost < 0) {
    panic("cost went below zero")
  }
 }
 return cost
}

func main() {
    fmt.Println("Hello, World!")

    file, err := os.Open("./2021/resources/day07.in")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    var input string
    // optionally, resize scanner's capacity for lines over 64K, see next example
    if scanner.Scan() {
        input = scanner.Text() // update value (=), not assign initially (:=)
        fmt.Println(input)
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }

    // convert input
    var horizontalPositions = []int{}

    fmt.Println("Input: ", input)
    fmt.Println("Split: ", strings.Split(input, ","))

    for _, i := range strings.Split(input, ",") {
        j, err := strconv.Atoi(i)
        if err != nil {
            panic(err)
        }
        horizontalPositions = append(horizontalPositions, j)
    }

    sumPositions := sum(horizontalPositions)
    minPosition := min(horizontalPositions)
    maxPosition := max(horizontalPositions)

    fmt.Println("Min: ", minPosition)
    fmt.Println("Max: ", maxPosition)
    fmt.Println("Sum: ", sumPositions)

    minFuel := 999999999999
    minFuelPos := 0
    for i := minPosition; i <= maxPosition; i++ {
        fuel := cost(horizontalPositions, i)
        fmt.Printf("%d: %d\n", i, fuel)
        if fuel < minFuel {
            minFuelPos = i
            minFuel = fuel
        }
    }

    fmt.Println("Min Fuel Pos: ", minFuelPos)
    fmt.Println("Min Fuel: ", minFuel)

}
