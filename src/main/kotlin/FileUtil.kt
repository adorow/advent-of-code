import java.io.File

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