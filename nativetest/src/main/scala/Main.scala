import scalanative.unsafe._


@extern
def inc(x: CLongLong): CLongLong = extern


@main def start() = {
    println(inc(1))
}
