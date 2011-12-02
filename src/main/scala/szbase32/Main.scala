package szbase32

/**
 * Basic CLI program that encodes strings to z-base-32, and decodes z-base-32
 * to UTF-8 strings.
 */
object Main extends App {

  def usage = println("Usage: zbase32.Main [encode|decode] [input]")
  
  if (args.length < 2) {
    usage
  } else {
    val in = args slice(1, args.length) mkString " "
    println(args(0) match {
      case "encode" => ZBase32.encode(in getBytes)
      case "decode" => new String(ZBase32.decode(in), "UTF-8")
      case _ => usage
    })
  }
  
}
