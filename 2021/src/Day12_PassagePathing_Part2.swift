/*
--- Part Two ---
After reviewing the available paths, you realize you might have time to visit a single small cave twice. Specifically, big caves can be visited any number of times, a single small cave can be visited at most twice, and the remaining small caves can be visited at most once. However, the caves named start and end can only be visited exactly once each: once you leave the start cave, you may not return to it, and once you reach the end cave, the path must end immediately.

Now, the 36 possible paths through the first example above are:

start,A,b,A,b,A,c,A,end
start,A,b,A,b,A,end
start,A,b,A,b,end
start,A,b,A,c,A,b,A,end
start,A,b,A,c,A,b,end
start,A,b,A,c,A,c,A,end
start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,d,b,A,c,A,end
start,A,b,d,b,A,end
start,A,b,d,b,end
start,A,b,end
start,A,c,A,b,A,b,A,end
start,A,c,A,b,A,b,end
start,A,c,A,b,A,c,A,end
start,A,c,A,b,A,end
start,A,c,A,b,d,b,A,end
start,A,c,A,b,d,b,end
start,A,c,A,b,end
start,A,c,A,c,A,b,A,end
start,A,c,A,c,A,b,end
start,A,c,A,c,A,end
start,A,c,A,end
start,A,end
start,b,A,b,A,c,A,end
start,b,A,b,A,end
start,b,A,b,end
start,b,A,c,A,b,A,end
start,b,A,c,A,b,end
start,b,A,c,A,c,A,end
start,b,A,c,A,end
start,b,A,end
start,b,d,b,A,c,A,end
start,b,d,b,A,end
start,b,d,b,end
start,b,end
The slightly larger example above now has 103 paths through it, and the even larger example now has 3509 paths through it.

Given these new rules, how many paths through this cave system are there?
*/

// run: `swift ./2021/src/Day12_PassagePathing_Part2.swift`
import Foundation

func addKey(connections : inout [String : Set<String>], from: String, to: String) {
    if var current = connections[from] {
        current.insert(to)
        connections[from] = current
    } else {
        var current = Set<String>()
        current.insert(to)
        connections[from] = current
    }
}

func isBigCave(name: String) -> Bool {
    return name.first!.isUppercase
}

func isSmallCave(name: String) -> Bool {
    return name.first!.isLowercase
}

let path = "./2021/resources/day12.in"

var contents: String = ""
var connections = [String : Set<String>]()
do {
    let contents = try String(contentsOfFile: path, encoding: .utf8)
    contents.components(separatedBy: "\n").forEach { path in
        let edges = path.components(separatedBy: "-")
        addKey(connections: &connections, from: edges[0], to: edges[1])
        addKey(connections: &connections, from: edges[1], to: edges[0])
    }
}
catch let error as NSError {
    print("Ooops! Something went wrong: \(error)")
}

// print(connections)

let start = "start"
let end = "end"

var numberOfPaths = 0

let initial: (Set<String>, String, [String], Bool) = (visitedCaves: ["start"], latest: "start", path: ["start"], visitedOneSmallCaveTwice: false)
var queue = [(Set<String>, String, [String], Bool)]()
queue.append(initial)

while (!queue.isEmpty) {
    let (visitedCaves, latest, path, visitedOneSmallCaveTwice) = queue.removeFirst()
//     print("Current:", latest, "Q:", queue)
    // print("Pop Q:", queue)
    var nextVisitedCaves = visitedCaves
    nextVisitedCaves.insert(latest)
//     print("Visited caves:", nextVisitedCaves)

    connections[latest]!.forEach { connectedTo in
        if connectedTo == end {
            // reached the end - count the paths
            numberOfPaths += 1
            var finalPath = path
            finalPath.append(connectedTo)
            //print("Final: ", finalPath)
        } else if connectedTo == start {
            // we should never go back to the start cave
        } else {
            //print(latest, "->", connectedTo)

            // add a new entry for every connection if: (1) is a big cave, or (2) is a small cave and hasn't been visited yet
            if isBigCave(name: connectedTo) || !visitedCaves.contains(connectedTo) || !visitedOneSmallCaveTwice {
                var newPath = path
                newPath.append(connectedTo)
                let willVisitSmallCaveSecondTime = (visitedCaves.contains(connectedTo) && isSmallCave(name: connectedTo))
                let newItem = (visitedCaves: nextVisitedCaves, latest: connectedTo, path: newPath, visitedOneSmallCaveTwice: visitedOneSmallCaveTwice || willVisitSmallCaveSecondTime)
                //print("Appending ", newItem)
                queue.append(newItem)
//                 print("Q:", queue)
            }
        }
    }

}
print("Q:", queue)
print("Total paths:", numberOfPaths)

