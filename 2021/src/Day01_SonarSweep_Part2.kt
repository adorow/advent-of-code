import java.io.File

/**
 * Considering every single measurement isn't as useful as you expected: there's just too much noise in the data.

Instead, consider sums of a three-measurement sliding window. Again considering the above example:

199  A
200  A B
208  A B C
210    B C D
200  E   C D
207  E F   D
240  E F G
269    F G H
260      G H
263        H
Start by comparing the first and second three-measurement windows. The measurements in the first window are marked A (199, 200, 208); their sum is 199 + 200 + 208 = 607. The second window is marked B (200, 208, 210); its sum is 618. The sum of measurements in the second window is larger than the sum of the first, so this first comparison increased.

Your goal now is to count the number of times the sum of measurements in this sliding window increases from the previous sum. So, compare A with B, then compare B with C, then C with D, and so on. Stop when there aren't enough measurements left to create a new three-measurement sum.

In the above example, the sum of each three-measurement window is as follows:

A: 607 (N/A - no previous sum)
B: 618 (increased)
C: 618 (no change)
D: 617 (decreased)
E: 647 (increased)
F: 716 (increased)
G: 769 (increased)
H: 792 (increased)
In this example, there are 5 sums that are larger than the previous sum.

Consider sums of a three-measurement sliding window. How many sums are larger than the previous sum?
 */
object Day01_SonarSweep_Part2 {

    fun solve() {
        val file = FileUtil.getInputFile(day = 1)
        val measurements = file.readLines().map { it.toInt() }

        val windowSize = 3

        val measurementWindows =
            (0 until measurements.size - (windowSize - 1))
                .map {
                    measurements.subList(it, it + windowSize).sum()
                }

        val increases = measurementWindows.fold(
            Accumulator()
        ) { acc, measurement ->
            when {
                measurement > acc.lastValue -> acc.inc(newValue = measurement)
                else -> acc.keep(newValue = measurement)
            }
        }.increases

        println(increases)
    }

    data class Accumulator(
        val lastValue: Int = 100_000, // starts at a value larger than any input, in order to not "increase" in the first value
        val increases: Int = 0
    ) {
        fun inc(newValue: Int) =
            Accumulator(
                lastValue = newValue,
                increases = increases.inc()
            )

        fun keep(newValue: Int) =
            Accumulator(
                lastValue = newValue,
                increases = increases
            )
    }

    /**
     * Simple utility to handle file input.
     */
    object FileUtil {

        fun getInputFile(day: Int) =
            File(
                this::class.java
                    .getResource("day${day.toString().padStart(2, '0')}.in")
                    .file!!
            )
    }
}

fun main() {
    Day01_SonarSweep_Part2.solve()
}

