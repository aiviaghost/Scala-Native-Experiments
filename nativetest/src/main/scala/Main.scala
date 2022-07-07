import scalanative.unsafe.*, Nat.*

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
        val res = Array.fill(N)(Array.ofDim[Float](N))
        for i <- 0 until N do
            for j <- 0 until N do
                for k <- 0 until N do
                    res(i)(j) += M(i)(k) * other.M(k)(j)
        ScalaMatrix(res)

    override def toString(): String = 
        M.map(_.mkString(", ")).map("[" + _ + "]").mkString("\n")

object ScalaMatrix:
    private val defaultSize = 500

    def empty(n: Int = defaultSize): ScalaMatrix = 
        ScalaMatrix(Array.fill(n)(Array.ofDim[Float](n)))

    def fillRandom(n: Int = defaultSize): ScalaMatrix = 
        import util.Random.nextFloat
        ScalaMatrix(Array.fill(n)(Array.fill(n)(nextFloat())))



type _500 = Digit3[_5, _0, _0]
type CMat500 = CArray[CArray[CFloat, _500], _500]

class CMatrix private (val M: CMat500):
    @extern
    private def mult(A: CMat500, B: CMat500, res: CMat500): Unit = extern

    def *(other: CMatrix): CMatrix = 
        val res = stackalloc[CMat500]()
        mult(M, other.M, res)
        CMatrix(res)
    
    override def toString(): String = 
        var res = ""
        for i <- 0 until 500 do
            res += "["
            for j <- 0 until 500 do
                res += (!M.at(i).at(j)).toString
            res += "]\n"
        res

object CMatrix:
    def empty(): CMatrix = CMatrix(stackalloc[CMat500]())

    def fillRandom(): CMatrix = 
        import util.Random.nextFloat
        val m = stackalloc[CMat500]()
        for i <- 0 until 500 do
            for j <- 0 until 500 do
                !m.at(i).at(j) = nextFloat()
        CMatrix(m)

def time[R](block: => R): Long = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    t1 - t0
}

@main def start() = 
    val its = 10
    val N = 500
    val t = (1 to its).map(it => {
        println(s"Iteration: ${it}")
        val A = CMatrix.fillRandom()
        val B = CMatrix.fillRandom()
        time({A * B})
    }).sum / (its * 1e9)
    println(t)
