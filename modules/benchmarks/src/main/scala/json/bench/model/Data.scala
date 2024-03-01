package json.bench.model

import scala.util.Random

case class Data(
    string: String,
    int: Int,
    boolean: Boolean,
    bigDecimal: BigDecimal,
    seqInt: Seq[Int],
    mapStringInt: Map[String, Int]
)

object Data {

  def samples[JAst](
      dataBuilder: DataBuilder[JAst],
      count: Int,
      seed: Int
  ): JAst = {
    val asts = dataSamples(count, seed).map(dataBuilder.ast)
    dataBuilder.array(asts)
  }

  def dataSamples(count: Int, seed: Int): Seq[Data] = {
    val rnd = new Random(seed = seed)
    def rndString(len: Int) = rnd.alphanumeric.take(len).mkString

    def rndInt(len: Int) = {
      val tenPow = (1 to len).foldLeft(1)((acc, _) => acc * 10)
      tenPow + rnd.nextInt(tenPow)
    }

    (1 to count).toList.map { i =>
      val flag = (i % 2) == 0
      // 128 bytes entity
      Data( // 2 bytes
        string = rndString(6 - (if (flag) 0 else 1)), // 9 + 8 (7) + 1 bytes
        int = rndInt(3), // 6 + 3 + 1 bytes
        boolean = rnd.nextBoolean(), // 10 + 4 (5) + 1 bytes
        bigDecimal = BigDecimal(rndInt(3)), // 13 + 3 + 1 bytes
        seqInt = List( // 9 + 2 + 5 + 4 + 1 bytes
          rndInt(1),
          rndInt(1),
          rndInt(1),
          rndInt(1),
          rndInt(1)
        ),
        mapStringInt = Map( // 15 + (6 + 3 + 1) * 3
          rndString(3) -> rndInt(3),
          rndString(3) -> rndInt(3),
          rndString(3) -> rndInt(3)
        )
      )
    }
  }

  trait DataBuilder[JAst] {
    def ast(d: Data): JAst

    def array(seq: Seq[JAst]): JAst
  }

}
