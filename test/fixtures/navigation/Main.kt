import demo.Greeter
import java.util.ArrayList

fun main() {
    val greeter = Greeter()
    val names = ArrayList<String>()
    names.add(greeter.greet())
}
