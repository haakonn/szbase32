package szbase32

// TODO: don't break on invalid z-base-32 input.

import java.io.ByteArrayOutputStream
import java.io.OutputStream

/**
 * Implements the <a href="http://philzimmermann.com/docs/human-oriented-base-32-encoding.txt">
 * z-base-32 encoding</a>.
 * 
 * @author Haakon Nilsen
 */
object ZBase32 {

  val encTable = "ybndrfg8ejkmcpqxot1uwisza345h769".toList
  val decTable = encTable.map(c => (c, encTable indexOf c)).toMap

  def encode(src: Seq[Byte]) = {
    val oddLength = src.size % 5
    val evenLength = src.size - oddLength
    val buf = new StringBuilder
    def d(x : Int) = src(x) & 0xff
    def add(b: Int) = buf += encTable(b & 0x1f)
    for (i <- 0 until evenLength by 5) {
      val b1 = d(i)
      val b2 = d(i + 1)
      val b3 = d(i + 2)
      val b4 = d(i + 3)
      val b5 = d(i + 4)
      add(b1 >> 3)
      add((b1 << 2) | (b2 >> 6))
      add(b2 >> 1)
      add((b2 << 4) | (b3 >> 4))
      add((b3 << 1) | (b4 >> 7))
      add(b4 >> 2)
      add((b4 << 3) | (b5 >> 5))
      add(b5)
    }
    if (oddLength > 0) {
      val b1 = d(evenLength)
      lazy val b2 = d(evenLength + 1)
      lazy val b3 = d(evenLength + 2)
      add(b1 >> 3)
      if (oddLength == 1) add(b1 << 2)
      if (oddLength > 1) {
        add((b1 << 2) | (b2 >> 6))
        add(b2 >> 1)
      }
      if (oddLength == 2) add(b2 << 4)
      if (oddLength > 2) add((b2 << 4) | (b3 >> 4))
      if (oddLength == 3) add(b3 << 1)
      if (oddLength == 4) {
        val b4 = d(evenLength + 3)
        add((b3 << 1) | (b4 >> 7))
        add(b4 >> 2)
        add(b4 << 3)
      }
    }
    buf.toString
  }
  
  def decode(src: String) = {
    /*We allow whitespace and dashes in the z-base-32 input, but remove
     * it before decoding:*/
    val d = src.replaceAll("([\\s-])", "")
    val out = new ByteArrayOutputStream
    val oddLength = d.size % 8
    val evenLength = d.size - oddLength - 1
    for (i <- 0 to evenLength by 8) {
      val b = Range(0, 8).map(j => j -> decTable(d(i + j))).toMap
      out.write((b(0) << 3) | (b(1) >> 2))
      out.write((b(1) << 6) | (b(2) << 1) | (b(3) >> 4))
      out.write((b(3) << 4) | (b(4) >> 1))
      out.write((b(4) << 7) | (b(5) << 2) | (b(6) >> 3))
      out.write((b(6) << 5) | b(7))
    }
    if (oddLength > 1) { // oddLength ∈ {2,4,5,7}
      val e = evenLength + 1
      def b(x: Int) = decTable(d(x))
      out.write((b(e) << 3) | (b(e + 1) >> 2))
      if (oddLength > 3) {
        out.write((b(e + 1) << 6) | (b(e + 2) << 1) | (b(e + 3) >> 4))
      }
      if (oddLength > 4) {
        out.write((b(e + 3) << 4) | (b(e + 4) >> 1))
      }
      if (oddLength > 6) {
        out.write((b(e + 4) << 7) | (b(e + 5) << 2) | (b(e + 6) >> 3))
      }
    }
    out.toByteArray
  }
  
}
