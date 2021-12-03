/*
--- Day 3: Binary Diagnostic ---
The submarine has been making some odd creaking noises, so you ask it to produce a diagnostic report just in case.

The diagnostic report (your puzzle input) consists of a list of binary numbers which, when decoded properly, can tell you many useful things about the conditions of the submarine. The first parameter to check is the power consumption.

You need to use the binary numbers in the diagnostic report to generate two new binary numbers (called the gamma rate and the epsilon rate). The power consumption can then be found by multiplying the gamma rate by the epsilon rate.

Each bit in the gamma rate can be determined by finding the most common bit in the corresponding position of all numbers in the diagnostic report. For example, given the following diagnostic report:

00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
Considering only the first bit of each number, there are five 0 bits and seven 1 bits. Since the most common bit is 1, the first bit of the gamma rate is 1.

The most common second bit of the numbers in the diagnostic report is 0, so the second bit of the gamma rate is 0.

The most common value of the third, fourth, and fifth bits are 1, 1, and 0, respectively, and so the final three bits of the gamma rate are 110.

So, the gamma rate is the binary number 10110, or 22 in decimal.

The epsilon rate is calculated in a similar way; rather than use the most common bit, the least common bit from each position is used. So, the epsilon rate is 01001, or 9 in decimal. Multiplying the gamma rate (22) by the epsilon rate (9) produces the power consumption, 198.

Use the binary numbers in your diagnostic report to calculate the gamma rate and epsilon rate, then multiply them together. What is the power consumption of the submarine? (Be sure to represent your answer in decimal, not binary.)
*/

// running as: g++ -o day03 ./2021/src/Day03_BinaryDiagnostic_Part1.cpp && ./day03 < ./2021/resources/day03.in

#include <iostream>
#include <string>

#define NUM_BITS 12

using namespace std;


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
    int n = 0; // counts the number of inputs given
    int bitCount[NUM_BITS] = {0}; // counts the amount of 1s found in each bit
    while (getline(cin, input)) {
        for (int i = 0; i < NUM_BITS; i++) {
            if (input[i] == '1') {
                bitCount[i]++;
            }
        }
        //cout << input << endl;
        n++;
    }

    cout << "N: " << n << endl;
    cout << "Bits:" << endl;
    for (int i = 0; i < NUM_BITS; i++) {
        cout << bitCount[i] << endl;
    }

    int gammaRate = get_gamma_rate(bitCount, n);
    int epsilonRate = get_epsilon_rate(gammaRate);
    int powerConsumption = gammaRate * epsilonRate;

    cout << "Gamma rate: " << gammaRate << endl;
    cout << "Epsilon rate: " << epsilonRate << endl;
    cout << "Power consumption: " << powerConsumption << endl;

    return 0;
}
