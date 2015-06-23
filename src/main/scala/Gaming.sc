implicit def hex2int (hex: String): Int = Integer.parseInt(hex, 16)

val input: Int = "0x781002".drop(2)

val s = input.toBinaryString
val b = s.reverse.padTo(32, '0').reverse

val Pattern = """[01]{1}([01]{12})([01]{8})([01]{8})([01]{1})([01]{2})""".r

val Pattern(time, team1, team2, whoScored, pointsScored) = b
//val Pattern(time, team1, team2, whoScored, pointsScored) = "00000000111100000001000000011111"

Integer.parseInt(time, 2)

