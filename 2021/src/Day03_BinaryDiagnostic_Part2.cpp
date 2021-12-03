/*
--- Part Two ---
Next, you should verify the life support rating, which can be determined by multiplying the oxygen generator rating by the CO2 scrubber rating.

Both the oxygen generator rating and the CO2 scrubber rating are values that can be found in your diagnostic report - finding them is the tricky part. Both values are located using a similar process that involves filtering out values until only one remains. Before searching for either rating value, start with the full list of binary numbers from your diagnostic report and consider just the first bit of those numbers. Then:

Keep only numbers selected by the bit criteria for the type of rating value for which you are searching. Discard numbers which do not match the bit criteria.
If you only have one number left, stop; this is the rating value for which you are searching.
Otherwise, repeat the process, considering the next bit to the right.
The bit criteria depends on which type of rating value you want to find:

To find oxygen generator rating, determine the most common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. If 0 and 1 are equally common, keep values with a 1 in the position being considered.
To find CO2 scrubber rating, determine the least common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. If 0 and 1 are equally common, keep values with a 0 in the position being considered.
For example, to determine the oxygen generator rating value using the same example diagnostic report from above:

Start with all 12 numbers and consider only the first bit of each number. There are more 1 bits (7) than 0 bits (5), so keep only the 7 numbers with a 1 in the first position: 11110, 10110, 10111, 10101, 11100, 10000, and 11001.
Then, consider the second bit of the 7 remaining numbers: there are more 0 bits (4) than 1 bits (3), so keep only the 4 numbers with a 0 in the second position: 10110, 10111, 10101, and 10000.
In the third position, three of the four numbers have a 1, so keep those three: 10110, 10111, and 10101.
In the fourth position, two of the three numbers have a 1, so keep those two: 10110 and 10111.
In the fifth position, there are an equal number of 0 bits and 1 bits (one each). So, to find the oxygen generator rating, keep the number with a 1 in that position: 10111.
As there is only one number left, stop; the oxygen generator rating is 10111, or 23 in decimal.
Then, to determine the CO2 scrubber rating value from the same example above:

Start again with all 12 numbers and consider only the first bit of each number. There are fewer 0 bits (5) than 1 bits (7), so keep only the 5 numbers with a 0 in the first position: 00100, 01111, 00111, 00010, and 01010.
Then, consider the second bit of the 5 remaining numbers: there are fewer 1 bits (2) than 0 bits (3), so keep only the 2 numbers with a 1 in the second position: 01111 and 01010.
In the third position, there are an equal number of 0 bits and 1 bits (one each). So, to find the CO2 scrubber rating, keep the number with a 0 in that position: 01010.
As there is only one number left, stop; the CO2 scrubber rating is 01010, or 10 in decimal.
Finally, to find the life support rating, multiply the oxygen generator rating (23) by the CO2 scrubber rating (10) to get 230.

Use the binary numbers in your diagnostic report to calculate the oxygen generator rating and CO2 scrubber rating, then multiply them together. What is the life support rating of the submarine? (Be sure to represent your answer in decimal, not binary.)
*/

// running as: g++ -o day03 ./2021/src/Day03_BinaryDiagnostic_Part2.cpp && ./day03 < ./2021/resources/day03.in

#include <iostream>
#include <string>
#include <vector>

#define NUM_BITS 12

using namespace std;

int bitCount[NUM_BITS]; // used as shared bit counter

int bin_to_int(string str) {
    return stoi(str, nullptr, 2);
}

void fill_bit_counter(vector<string>& input, int bit) {
    // reset counter for the bit
    bitCount[bit] = 0;

    // refill that bit
    for (int i = 0; i < input.size(); i++) {
        string str = input[i];
        if (str[bit] == '1') {
            bitCount[bit]++;
        }
    }
}

// when bit [bit] has value [ch], remove it from the input
void with_entries_removed(vector<string>& input, int bit, char ch) {
    //cout << "Bit: " << bit << ", Before: " << input.size() << endl;
    vector<string>::iterator it = input.begin();
    while (it != input.end()) {
        if (it->at(bit) == ch) {
            it = input.erase(it);
        } else {
            it++;
        }
    }
    //cout << "Bit: " << bit << ", After: " << input.size() << endl;
}

void fill_bit_counter(vector<string>& input) {
    // reset counter
    memset(bitCount, 0, sizeof(bitCount));

    // refill
    for (int i = 0; i < input.size(); i++) {
        string str = input[i];
        for (int j = 0; j < NUM_BITS; j++) {
            if (str[j] == '1') {
                bitCount[j]++;
            }
        }
    }
}

void print_bit_counter() {
    cout << "Bits:" << endl;
    for (int i = 0; i < NUM_BITS; i++) {
        cout << bitCount[i] << endl;
    }
}

char opposite_bit(char value) {
    return value == '1' ? '0' : '1';
}

int find_oxygen_generator_rating(vector<string> input/*copy the vector*/, int bit) {
    // found the result?
    if (input.size() == 1) {
        // return the decimal value
        return bin_to_int(input[0]);
    }
    fill_bit_counter(input, bit);

    int n = input.size();
    int num_ones = bitCount[bit];
    int num_zeros = n - num_ones;

    char most_common = num_ones >= num_zeros ? '1' : '0';
    char least_common = opposite_bit(most_common);

    // keep the most_common, so remove the least_common
    with_entries_removed(input, bit, least_common);

    return find_oxygen_generator_rating(input, bit+1);
}

int find_oxygen_generator_rating(vector<string>& input) {
    return find_oxygen_generator_rating(input, 0);
}

int find_co2_scrubber_rating(vector<string> input/*copy the vector*/, int bit) {
    // found the result?
    if (input.size() == 1) {
        // return the decimal value
        return bin_to_int(input[0]);
    }
    fill_bit_counter(input, bit);

    int n = input.size();
    int num_ones = bitCount[bit];
    int num_zeros = n - num_ones;

    char most_common = num_ones >= num_zeros ? '1' : '0';
    char least_common = opposite_bit(most_common);

    // keep the least_common, so remove the most_common
    with_entries_removed(input, bit, most_common);

    return find_co2_scrubber_rating(input, bit+1);
}

int find_co2_scrubber_rating(vector<string>& input) {
    return find_co2_scrubber_rating(input, 0);
}

int get_gamma_rate(int* bitCount, int n) {
    int min = n/2;
    int gr = 0;
    for (int i = 0; i < NUM_BITS; i++) {
        if (bitCount[i] > min) {
            gr |= 1 << (NUM_BITS-(i+1));
        }
    }
    return gr;
}

int get_epsilon_rate(int gammaRate) {
    int mask = (1 << NUM_BITS) - 1; // all NUM_BITS least significant bits set to 1, the rest 0
    return (~gammaRate & mask);
}

int main() {
    string input;
    vector<string> inputVector;

    while (getline(cin, input)) {
        inputVector.push_back(input);
    }

//    cout << "Input size: " << inputVector.size() << endl;

//    for (int i = 0; i < inputVector.size(); i++) {
//        cout << inputVector[i] << endl;
//    }

//    fill_bit_counter(inputVector);
//    cout << "Bits:" << endl;
//    for (int i = 0; i < NUM_BITS; i++) {
//        cout << bitCount[i] << endl;
//    }

    //cout << "Start oxygen generator rating" << endl;
    int oxygen_generator_rating = find_oxygen_generator_rating(inputVector);
    //cout << "Start CO2 scrubber rating" << endl;
    int co2_scrubber_rating = find_co2_scrubber_rating(inputVector);
    int life_support_rating = oxygen_generator_rating * co2_scrubber_rating;

    cout << "Oxygen generator rating: " << oxygen_generator_rating << endl;
    cout << "CO2 scrubber rating: " << co2_scrubber_rating << endl;
    cout << "Life support rating: " << life_support_rating << endl;

    return 0;
}
