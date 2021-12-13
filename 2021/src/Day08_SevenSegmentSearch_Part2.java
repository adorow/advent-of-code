import java.io.File;
import java.util.*;
import java.util.stream.Stream;

import static java.util.Arrays.asList;
import static java.util.Arrays.stream;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

/*
--- Part Two ---
Through a little deduction, you should now be able to determine the remaining digits. Consider again the first example above:

acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf
After some careful analysis, the mapping between signal wires and segments only make sense in the following configuration:

 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc
So, the unique signal patterns would correspond to the following digits:

acedgfb: 8
cdfbe: 5
gcdfa: 2
fbcad: 3
dab: 7
cefabd: 9
cdfgeb: 6
eafb: 4
cagedb: 0
ab: 1
Then, the four digits of the output value can be decoded:

cdfeb: 5
fcadb: 3
cdfeb: 5
cdbaf: 3
Therefore, the output value for this entry is 5353.

Following this same process for each entry in the second, larger example above, the output value of each entry can be determined:

fdgacbe cefdb cefbgd gcbe: 8394
fcgedb cgb dgebacf gc: 9781
cg cg fdcagb cbg: 1197
efabcd cedba gadfec cb: 9361
gecf egdcabf bgf bfgea: 4873
gebdcfa ecba ca fadegcb: 8418
cefg dcbef fcge gbcadfe: 4548
ed bcgafe cdgba cbgef: 1625
gbdfcae bgc cg cgb: 8717
fgae cfgab fg bagce: 4315
Adding all of the output values in this larger example produces 61229.

For each entry, determine all of the wire/segment connections and decode the four-digit output values. What do you get if you add up all of the output values?

 */
// run (java 11): `java ./2021/src/Day08_SevenSegmentSearch_Part1.java`
public class Day08_SevenSegmentSearch_Part2 {

    // 1 : 2 segments

    // 7 : 3 segments

    // 4 : 4 segments

    // 2 : 5 segments
    // 3 : 5 segments
    // 5 : 5 segments

    // 0 : 6 segments
    // 6 : 6 segments
    // 9 : 6 segments

    // 8 : 7 segments

    public static void main(String[] args) throws Throwable {
        Scanner in = new Scanner(new File("./2021/resources/day08.in"));

        long sum = 0;

        while (in.hasNextLine()) {
            String line = in.nextLine();
            System.out.println(line);
            String[] input = line.split("\\|");
            // sort each string so they will look the same
            String[] inputValues = stream(input[0].trim().split(" ")).map(str -> sorted(str)).toArray(String[]::new);
            String[] outputValues = stream(input[1].trim().split(" ")).map(str -> sorted(str)).toArray(String[]::new);

            final Map<String, String> codex = getCodex(inputValues);
            String numberAsString = stream(outputValues)
                    .map(codex::get)
                    .collect(joining());

            sum += Long.parseLong(numberAsString);
            System.out.printf("%s = %s%n", asList(outputValues), numberAsString);
        }

        System.out.printf("What do you get if you add up all of the output values? %d%n", sum);
    }


    private static Map<String, String> getCodex(String[] inputValues) {
        final Map<String, String> codex = new HashMap<>();

        final String one = stream(inputValues).filter(str -> str.length() == 2).findFirst().get();
        codex.put(one, "1");
        final String seven = stream(inputValues).filter(str -> str.length() == 3).findFirst().get();
        codex.put(seven, "7");
        final String four = stream(inputValues).filter(str -> str.length() == 4).findFirst().get();
        codex.put(four, "4");
        final String eight = stream(inputValues).filter(str -> str.length() == 7).findFirst().get();
        codex.put(eight, "8");

        // 0 -> superset of (1, 7) not of (4)
        // 6 -> superset of none of (1,4,7)
        // 9 -> superset of (1, 4, 7)
        List<String> sixSegments = stream(inputValues).filter(str -> str.length() == 6).collect(toList());
        final String zero = sixSegments.stream().filter(str -> containsAllCharsOfStrings(str, one, seven) && doesNotContainAllCharsOf(str, four)).findFirst().get();
        codex.put(zero, "0");
        final String six = sixSegments.stream().filter(str -> doesNotContainAllCharsOf(str, one, four, seven)).findFirst().get();
        codex.put(six, "6");
        final String nine = sixSegments.stream().filter(str -> containsAllCharsOfStrings(str, one, four, seven)).findFirst().get();
        codex.put(nine, "9");

        // 2 -> superset of () not of (1, 4, 7)
        // 3 -> superset of (1, 7) not of (4)
        // 5 -> superset of () not of (1, 4, 7)
        List<String> fiveSegments = stream(inputValues).filter(str -> str.length() == 5).collect(toList());
        final String three = fiveSegments.stream().filter(str -> containsAllCharsOfStrings(str, one, seven) && doesNotContainAllCharsOf(str, four)).findFirst().get();
        codex.put(three, "3");
        // two and five are mirrored, an easy distinction is that 6 is a superset of the segments of 5
        final String two = fiveSegments.stream().filter(str -> doesNotContainAllCharsOf(str, one, four, seven) && !isSupersetOf(six, str)).findFirst().get();
        codex.put(two, "2");
        final String five = fiveSegments.stream().filter(str -> doesNotContainAllCharsOf(str, one, four, seven) && isSupersetOf(six, str)).findFirst().get();
        codex.put(five, "5");

        return codex;
    }

    private static boolean doesNotContainAllCharsOf(String str, String... otherStrings) {
        return Stream.of(otherStrings)
                .noneMatch(otherString -> isSupersetOf(str, otherString));
    }

    private static boolean containsAllCharsOfStrings(String str, String... otherStrings) {
        return Stream.of(otherStrings)
                .allMatch(otherString -> isSupersetOf(str, otherString));
    }

    private static boolean isSupersetOf(String superset, String subset) {
        for (char ch : subset.toCharArray()) {
            if (!superset.contains(String.valueOf(ch))) {
                return false;
            }
        }
        return true;
    }

    private static String sorted(String str) {
        char[] chars = str.toCharArray();
        Arrays.sort(chars);
        return new String(chars);
    }

//    (correct) segments:
//     aaaa
//    b    c
//    b    c
//     dddd
//    e    f
//    e    f
//     gggg
    static int[] size2 = {2,5}; // can only be 1 (2 segments)
    static int[] size3 = {0,2,5}; // can only be 7 (3 segments)
    static int[] size4 = {1,2,3,5}; // can only be 4 (4 segments)
    static int[] size5 = {0,1,2,3,4,5,6}; // can be 3 options, covers all 8 segments (5 segments)
    static int[] size6 = {0,1,2,3,4,5,6}; // can be 3 options, covers all 8 segments (6 segments)
    static int[] size7 = {0,1,2,3,4,5,6}; // can only be 8 -> but 8 has all segments


    static int[] a = {0,2,3,5,6,7,8,9}; // a=0
    static int[] b = {0,4,5,6,8,9};     // b=1
    static int[] c = {0,1,2,3,4,7,8,9}; //c=2
    static int[] d = {2,3,4,5,6,8,9}; //d=3
    static int[] e = {0,2,6,8}; //e=4
    static int[] f = {0,1,3,4,5,6,7,8,9}; //f=5
    static int[] g = {0,2,3,5,6,8,9};//g=6
    static int[][] segments = {a,b,c,d,e,f,g};

    static int[] zero = {0,1,2,4,5,6};
    static int[] one = {2,4};
    static int[] two = {0,2,3,4,6};
    static int[] three = {0,2,3,5,6};
    static int[] four = {1,2,3,5};
    static int[] five = {0,1,3,5,6};
    static int[] six = {0,1,3,4,5,6};
    static int[] seven = {0,2,5};
    static int[] eight = {0,1,2,3,4,5,6};
    static int[] nine = {0,1,2,3,5,6};
    static int[][] numbers = {zero,one,two,three,four,five,six,seven,eight,nine};

}

