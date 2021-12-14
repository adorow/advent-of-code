
//--- Part Two ---
//The resulting polymer isn't nearly strong enough to reinforce the submarine. You'll need to run more steps of the pair insertion process; a total of 40 steps should do it.
//
//In the above example, the most common element is B (occurring 2192039569602 times) and the least common element is H (occurring 3849876073 times); subtracting these produces 2188189693529.
//
//Apply 40 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?

// run with : `bal run ./2021/src/Day14_ExtendedPolymerization_Part2.bal`

import ballerina/io;
import ballerina/log;

function verifyCounts(map<int> charCounter) {
    int min = 1000000000000000000;
    int max = 0;

    foreach var [ch, count] in charCounter.entries() {
        // discard when a letter never shows up
        if count < min && count > 0 {
            min = count;
        }
        if count > max {
            max = count;
        }
    }

    io:println("Min: " + min.toString());
    io:println("Max: " + max.toString());
    int diff = (max-min);
    io:println("Max-Min= " + diff.toString());
}

// returns a tuple of [template, map of rules] -> could not learn a simpler way to do this
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

function initPairCounter(string template, map<string> mappings) returns map<int> {
    map<int> pairCounter = {};
    foreach var pair in mappings.keys() {
        pairCounter[pair] = 0;
    }

    // fill pairCounter with initial template
    foreach int i in 0 ..< template.length()-1 {
        string pair = template.substring(i, i+2);

        pairCounter[pair] = pairCounter.get(pair) + 1;
    }
    return pairCounter;
}

function initCharCounter(string template) returns map<int> {
    map<int> charCounter = {
       "S":0,"C":0,"K":0,"V":0,"O":0,"B":0,"N":0,"H":0,"P":0,"F":0
    };

   foreach int i in 0 ..< template.length() {
       string char = template.substring(i,i+1);
       charCounter[char] = charCounter.get(char) + 1;
   }

   return charCounter;
}

public function main() returns @tainted error? {
    match readInput() {
        var [template, mappings] => {
            io:println("Mappings" + mappings.toString());
            io:println("Template: " + template);

            // int in ballerina is a 64-bit signed integer
            map<int> pairCounter = initPairCounter(template, mappings);
            map<int> charCounter = initCharCounter(template);

            io:println("Chars: ");
            verifyCounts(charCounter);
            io:println("Pairs: ");
            verifyCounts(pairCounter);

            int length = 20;

            foreach int step in 1 ... 40 {
                // the new pairs, to be added to the counter after the turn
                map<int> newPairs = initPairCounter("", mappings);

                foreach var [pair, count] in pairCounter.entries() {
                    string ch = mappings.get(pair);
                    int currentCharCount = charCounter.get(ch);

                    // increment the chars
                    charCounter[ch] = currentCharCount + count;
                    // increment the 2 new pairs being created
                    string onePair = pair.substring(0,1) + ch;
                    string otherPair = ch + pair.substring(1,2);

                    newPairs[onePair] = newPairs.get(onePair) + count;
                    newPairs[otherPair] = newPairs.get(otherPair) + count;

                    // the current pair is reset (since it no longer exists)
                    pairCounter[pair] = 0;
                }
                // add the new pair counts
                foreach var [pair, count] in newPairs.entries() {
                    pairCounter[pair] = pairCounter.get(pair) + count;
                }

                int newLength = (length * 2) - 1;
                length = newLength;

                io:println("After step " + step.toString() + ": " + newLength.toString() + " chars");
                verifyCounts(charCounter);
            }
        }
    }
}

