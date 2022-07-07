// mport scalanative.unsafe._

def time[R](block: => R): Long = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    t1 - t0
}

class ScalaMatrix private (val M: Array[Array[Float]]):

    assert(M.size == M(0).size && M.forall(_.size == M(0).size))

    private val N = M.size

    def *(other: ScalaMatrix): ScalaMatrix = 
        /* funny but probably needlessly slow and not much more compact
        ScalaMatrix(Array.fill(N * N)(0f).zipWithIndex.map((v, i) => {
            val (x: Int, y: Int) = (i % N, i / N)
            M(y).zip(other.M.map(_(x))).map((a, b) => a * b).sum
        }).grouped(N).toArray)
        */
        val res = Array.fill(N)(Array.fill(N)(0f))
        for i <- 0 until N do
            for j <- 0 until N do
                for k <- 0 until N do
                    res(i)(j) += M(i)(k) * other.M(k)(j)
        ScalaMatrix(res)

    override def toString(): String = 
        M.map(_.mkString(", ")).map("[" + _ + "]").mkString("\n")

object ScalaMatrix:
    def empty(n: Int): ScalaMatrix = 
        ScalaMatrix(Array.fill(n)(Array.ofDim[Float](n)))

    def fillRandom(n: Int): ScalaMatrix = 
        import util.Random.nextFloat
        ScalaMatrix(Array.fill(n)(Array.fill(n)(nextFloat())))

@main def start() = 
    val its = 10
    val N = 500
    val t = (1 to its).map(it => {
        println(s"Iteration: ${it}")
        val A = ScalaMatrix.fillRandom(N)
        val B = ScalaMatrix.fillRandom(N)
        var x: ScalaMatrix = ScalaMatrix.empty(N)
        time({x = A * B})
    }).sum / (its * 1e9)
    println(t)
