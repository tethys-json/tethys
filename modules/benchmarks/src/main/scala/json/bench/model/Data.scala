package json.bench.model

import pushka.annotation.pushka
import _root_.pushka.{Ast, RW}

import scala.math.BigDecimal.RoundingMode
import scala.util.Random
import Data.bigDecimalRW

@pushka
case class Data(string: String,
                int: Int,
                boolean: Boolean,
                bigDecimal: BigDecimal,
                seqInt: Seq[Int],
                mapStringInt: Map[String, Int])

object Data {

  implicit val bigDecimalRW: RW[BigDecimal] = new RW[BigDecimal] {

    override def write(value: BigDecimal): Ast = Ast.Num(value.toString())

    override def read(value: Ast): BigDecimal = BigDecimal(value.asInstanceOf[Ast.Num].value)
  }

  def samples[JAst](dataBuilder: DataBuilder[JAst], count: Int, seed: Int): JAst = {
    val asts = dataSamples(count, seed).map(dataBuilder.ast)
    dataBuilder.array(asts)
  }

  def dataSamples(count: Int, seed: Int): Seq[Data] = {
    val rnd = new Random(seed = seed)

    (1 to count).map(_ => Data(
      string = rnd.alphanumeric.take(rnd.nextInt(20)).mkString,
      int = rnd.nextInt(1000),
      boolean = rnd.nextBoolean(),
      bigDecimal = BigDecimal(rnd.nextDouble() * 1000).setScale(5, RoundingMode.HALF_UP),
      seqInt = (1 to (1 + rnd.nextInt(99))).toVector.map(_ => rnd.nextInt(1000)),
      mapStringInt = (1 to (1 + rnd.nextInt(9))).toVector.map(_ => rnd.alphanumeric.take(rnd.nextInt(20)).mkString -> rnd.nextInt(1000)).toMap
    )).toVector
  }

  trait DataBuilder[JAst] {
    def ast(d: Data): JAst

    def array(seq: Seq[JAst]): JAst
  }
}
