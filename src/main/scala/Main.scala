package ttbf
import ttbf.parser._

object Main  {
    def main(args: Array[String]) {
        // val prog = ""
        if (args.length > 0) TTBFParser.parseProg(args(0))
    }
}
