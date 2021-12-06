/*
--- Part Two ---
Suppose the lanternfish live forever and have unlimited food and space. Would they take over the entire ocean?

After 256 days in the example above, there would be a total of 26984457539 lanternfish!

How many lanternfish would there be after 256 days?
 */

// running with: `groovy ./2021/src/Day06_Lanternfish_Part1.groovy`

maxAge = 8

def newAgeMap() {
    map = [:]
    for (int i in 0..maxAge) {
        map[i] = new BigInteger("0")
    }
    return map
}

fishAges = new File('./2021/resources/day06.in').text.split(",").collect { it.toBigInteger() }
println "Fish ages: $fishAges"

ageMap = newAgeMap()
for (int i in fishAges) {
    map[i]++
}
println "Age map (day 0): $ageMap"

for (int day in 1..256) {
    nextAgeMap = newAgeMap()

    // age = 0
    // these lanternfish reset to 6
    // and create new lanternfish with 8
    nextAgeMap[6] = ageMap[0]
    nextAgeMap[8] = ageMap[0]

    // age in 1..maxAge
    // reduce the number of days of each other count
    for (int age in 1..maxAge) {
        nextAgeMap[age-1] += ageMap[age]
    }

    ageMap = nextAgeMap
    println "Age map (day ${day}): $ageMap"
}

numFishes = ageMap.collect { it.value }.sum()
println "Total fishes at last day: $numFishes"

