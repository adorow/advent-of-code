import java.io.File

/**
 * As the submarine drops below the surface of the ocean, it automatically performs a sonar sweep of the nearby sea floor. On a small screen, the sonar sweep report (your puzzle input) appears: each line is a measurement of the sea floor depth as the sweep looks further and further away from the submarine.

For example, suppose you had the following report:

199
200
208
210
200
207
240
269
260
263
This report indicates that, scanning outward from the submarine, the sonar sweep found depths of 199, 200, 208, 210, and so on.

The first order of business is to figure out how quickly the depth increases, just so you know what you're dealing with - you never know if the keys will get carried into deeper water by an ocean current or a fish or something.

To do this, count the number of times a depth measurement increases from the previous measurement. (There is no measurement before the first measurement.) In the example above, the changes are as follows:

199 (N/A - no previous measurement)
200 (increased)
208 (increased)
210 (increased)
200 (decreased)
207 (increased)
240 (increased)
269 (increased)
260 (decreased)
263 (increased)
In this example, there are 7 measurements that are larger than the previous measurement.

How many measurements are larger than the previous measurement?
 */
object Day01_SonarSweep_Part1 {

    fun solve() {
        val file = FileUtil.getInputFile(day = 1)
        val increases = file.useLines {
            it.fold(
                Accumulator()
            ) { acc, measurementString ->
                val measurement = measurementString.toInt()
                when {
                    measurement > acc.lastValue -> acc.inc(newValue = measurement)
                    else -> acc.keep(newValue = measurement)
                }
            }.increases
        }
        println(increases)
    }

    data class Accumulator(
        val lastValue: Int = 10_000, // starts at a value larger than any input, in order to not "increase" in the first value
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
    Day01_SonarSweep_Part1.solve()
}

