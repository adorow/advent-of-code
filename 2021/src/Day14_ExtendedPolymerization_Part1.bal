//--- Day 14: Extended Polymerization ---
//The incredible pressures at this depth are starting to put a strain on your submarine. The submarine has polymerization equipment that would produce suitable materials to reinforce the submarine, and the nearby volcanically-active caves should even have the necessary input elements in sufficient quantities.
//
//The submarine manual contains instructions for finding the optimal polymer formula; specifically, it offers a polymer template and a list of pair insertion rules (your puzzle input). You just need to work out what polymer would result after repeating the pair insertion process a few times.
//
//For example:
//
//NNCB
//
//CH -> B
//HH -> N
//CB -> H
//NH -> C
//HB -> C
//HC -> B
//HN -> C
//NN -> C
//BH -> H
//NC -> B
//NB -> B
//BN -> B
//BB -> N
//BC -> B
//CC -> N
//CN -> C
//The first line is the polymer template - this is the starting point of the process.
//
//The following section defines the pair insertion rules. A rule like AB -> C means that when elements A and B are immediately adjacent, element C should be inserted between them. These insertions all happen simultaneously.
//
//So, starting with the polymer template NNCB, the first step simultaneously considers all three pairs:
//
//The first pair (NN) matches the rule NN -> C, so element C is inserted between the first N and the second N.
//The second pair (NC) matches the rule NC -> B, so element B is inserted between the N and the C.
//The third pair (CB) matches the rule CB -> H, so element H is inserted between the C and the B.
//Note that these pairs overlap: the second element of one pair is the first element of the next pair. Also, because all pairs are considered simultaneously, inserted elements are not considered to be part of a pair until the next step.
//
//After the first step of this process, the polymer becomes NCNBCHB.
//
//Here are the results of a few steps using the above rules:
//
//Template:     NNCB
//After step 1: NCNBCHB
//After step 2: NBCCNBBBCBHCB
//After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
//After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
//This polymer grows quickly. After step 5, it has length 97; After step 10, it has length 3073. After step 10, B occurs 1749 times, C occurs 298 times, H occurs 161 times, and N occurs 865 times; taking the quantity of the most common element (B, 1749) and subtracting the quantity of the least common element (H, 161) produces 1749 - 161 = 1588.
//
//Apply 10 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?
//
// run with : `bal run ./2021/src/Day14_ExtendedPolymerization_Part1.bal`

import ballerina/io;
import ballerina/log;

function countChar(string char, string template) returns int {
    int count = 0;
    foreach int i in 0 ..< template.length() {
        if template.substring(i,i+1) == char {
            count += 1;
        }
    }
    return count;
}

// returns a tuple of [template, map of rules]
function readInput() returns @tainted [string, map<string>] | error {
    io:ReadableByteChannel readableFieldResult =
                            check io:openReadableFile("./2021/resources/day14.in");
    io:ReadableDataChannel dc = new io:ReadableDataChannel(readableFieldResult);

    string|io:Error template = dc.readString(20, "UTF-8");
    //io:println(template);

    // skip
    string|io:Error discard = dc.readString(1, "UTF-8");

    map<string> mappings = {};

    foreach int i in 0 ..< 100 {
        string|io:Error discardLn = dc.readString(1, "UTF-8");

        string|io:Error pair = dc.readString(2, "UTF-8");

        //io:println(pair);
        string|io:Error arrow = dc.readString(4, "UTF-8");
        //io:println(arrow);
        string|io:Error char = dc.readString(1, "UTF-8");
        //io:println(char);

        if pair is string && char is string {
            [string, string] rule = [pair, char];
            mappings[pair] = char;
            //io:println(rule);
        }
    }
    closeRc(dc);

    if template is string {
        return [template, mappings];
    } else {
        return ["", mappings];
    }
}

function closeRc(io:ReadableDataChannel ch) {
    var cr = ch.close();
    if (cr is error) {
        log:printError("Error occurred while closing the channel: ", err = cr);
    }
}

public function main() returns @tainted error? {
    match readInput() {
        var [template, mappings] => {
            io:println("Mappings" + mappings.toString());
            io:println("Template: " + template);

            foreach int step in 1 ... 40 {
                string newTemplate = "";

                foreach int i in 0 ..< template.length()-1 {
                    string firstChar = template.substring(i, i+1);
                    string pair = template.substring(i, i+2);
                    string? mappedChar = mappings[pair];

                    if (mappedChar is string) {
                        //io:println("Pair: " + pair);
                        //io:println("Char: " + mappedChar);
                        newTemplate = newTemplate + firstChar + mappedChar;
                    } else {
                        io:println("!!! ERROR !!!");
                        //throw error("Failure", message = "Char was not found");
                    }
                }
                newTemplate = newTemplate + template[template.length()-1];

                //io:println("After step " + step.toString() + ": " + newTemplate);
                io:println("After step " + step.toString() + ": " + newTemplate.length().toString() + " chars");
                template = newTemplate;
            }

            io:println("At the end: " + template.length().toString() + " chars");

            map<string> checkedChars = {};

            int max = 0;
            int min = 1000000;

            foreach int i in 0 ..< template.length() {
                string char = template.substring(i,i+1);
                if !checkedChars.hasKey(char) {
                    checkedChars[char] = "xx";
                    int count = countChar(char, template);
                    if count < min {
                        min = count;
                    }
                    if count > max {
                        max = count;
                    }
                }
            }

            io:println("Min: " + min.toString());
            io:println("Max: " + max.toString());
            int diff = (max-min);
            io:println("Max-Min= " + diff.toString());
        }
    }
}
