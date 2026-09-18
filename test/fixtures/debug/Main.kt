package demo

fun main(args: Array<String>) {
    val greeting = "debug-ready"
    System.out.println(greeting) // BREAKPOINT
    System.out.println("arg=" + args.joinToString("|"))
    System.out.println("env=" + System.getenv("JB_DEBUG_VALUE"))
    if (args.contains("stdin")) {
        val line = System.`in`.bufferedReader().readLine()
        System.out.println("input=" + line)
    }
    System.out.println("debug-done")
}
