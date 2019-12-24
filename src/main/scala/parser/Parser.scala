package Parser

import scala.util.parsing.combinator._
import common.AST._

object TTBF extends RegexParsers {
    override def skipWhitespace = true
    def astConst: Parser[Int] = {
        """(+|-)?(0|[1-9]\d*)""".r ^^ { str => str.toInt }
    }
    private class CaseinsensitiveLifter(str: String) {
        def ic: Parser[String] = ("""(?i)\Q""" + str + """\E""").r
    }
    private implicit def liftString(str: String): CaseinsensitiveLifter = new CaseinsensitiveLifter(str)

    def astId: Parser[ASTId] = {
         "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { ASTId(_) };
    }
    def astProg: Parser[ASTProg] = {
        "program".ic ~ astId ~>
        astVarDecls ~
        astSubrt.* ~
        astBlockDot ^^ {
            case globalVars ~ subrts ~ mainBody =>
                ASTProg(globalVars, subrts, mainBody)
        }
    }

    def astVarDecls: Parser[ASTVarDecls] = {
        ("var".ic ~> astVarDeclClause.+).? ^^ {
            case Some(cls) => ASTVarDecls(cls.flatten)
            case None => ASTVarDecls(Nil)
        }
    }
    def astVarDeclClause: Parser[List[ASTVarDecl]] = {
        astId.+ ~ ":" ~ astVarType <~ ";" ^^ {
            case ids ~ _ ~ valType =>
                ids.map(id => ASTVarDecl(id, valType))
        }
    }
    def astVarType: Parser[ASTVarType] = {
        astBaseVarType |
        ( "array".ic ~ "[" ~ astConst ~ ".." ~ astConst ~ "]" ~ "of".ic ~ astBaseVarType ^^ {
            case _ ~ "[" ~ lb ~ ".." ~ ub ~ "]" ~ _ ~ varType =>
                ASTArr(lb, ub, varType)
        } )
    }
    def astBaseVarType: Parser[ASTBaseVarType] = {
        "integer".ic ^^^ ASTInt |
        "char".ic ^^^ ASTChar
    }
    def astSubrt: Parser[ASTSubrt] = {
        astProc | astFun
    }
    def astProc: Parser[ASTProc] = {
        "procedure".ic ~ astId ~ "(" ~ repsep(astParam, ",") ~ ")" ~ ";" ~ astSubrtBody ^^ {
            case _ ~ procName ~ _ ~ params ~ _ ~ _ ~ subrt => {
                val actualParams = ASTParamDecls(params.flatten)
                ASTProc(procName, actualParams, subrt._1, subrt._2)
            }
        }
    }
    def astFun: Parser[ASTFun] = {
        "function".ic ~ astId ~
            "(" ~ repsep(astParam, ",") ~ ")" ~ ":" ~ astBaseVarType ~ ";" ~ astSubrtBody ^^ {
            case _ ~ procName ~ _ ~ params ~ _ ~ _ ~ retType ~ _ ~ subrt => {
                val actualParams = ASTParamDecls(params.flatten)
                ASTFun(procName, actualParams, subrt._1, subrt._2, retType)
            }
        }
    }
    def astSubrtBody: Parser[(ASTVarDecls, ASTStmts)] = {
        astVarDecls ~ astBlockSemicolon ^^ {
            case varDecls ~ stmts => (varDecls, stmts)
        }
    }
    def astParam: Parser[List[ASTParamDecl]] = {
        "var".ic.? ~ astId.+ ~ ":" ~ astVarType ^^ {
            case varModifier ~ varNames ~ _ ~ varType => {
                val isRef = varModifier.isDefined
                varNames.map(varName => ASTParamDecl(varName, varType, isRef))
            }
        }
    }
    def astBlock(endMarker: String): Parser[ASTStmts] = {
        "begin".ic ~> astStmt.* <~ "end".ic ~ endMarker ^^ {
            ASTStmts(_)
        }
    }
    val astBlockDot = astBlock(".")
    val astBlockSemicolon = astBlock(";")
    val astStmt: Parser[ASTStmt] = ???
}