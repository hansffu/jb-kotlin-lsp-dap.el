fun demo() {
    when (System.currentTimeMillis()) {
        0L -> System.out.println("zero")
        1L -> System.out.println("one")
        else -> System.out.println("other")
    }
}
