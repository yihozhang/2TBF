package ttbf.parser
import org.scalatest.funsuite._
import ttbf.common.AST._

class ParserSuite extends AnyFunSuite {
    test("var declarations") {
        val fn = (str:String, ast: ASTVarDecls) =>
            assert(TTBFParser.parse(TTBFParser.astVarDecls, str).get== ast)
        fn("""var a:integer;""", ASTDecls(List(ASTVarDecl(ASTId("a"),ASTInt))))
        fn("""var a,b:integer;""", ASTDecls(List(ASTVarDecl(ASTId("a"),ASTInt),ASTVarDecl(ASTId("b"),ASTInt))))
        fn("""var a:integer;b:integer;""", ASTDecls(List(ASTVarDecl(ASTId("a"),ASTInt),ASTVarDecl(ASTId("b"),ASTInt))))
    }
    test("expressions") {
        val fn = (str:String, ast: ASTAsg) =>
            assert(TTBFParser.parse(TTBFParser.astAsg, str).get== ast)
        fn("a:=a;",ASTAsg(ASTVar(ASTId("a")),ASTVar(ASTId("a"))))
        fn("a:=a+b;",ASTAsg(ASTVar(ASTId("a")),ASTPlus(ASTVar(ASTId("a")), ASTVar(ASTId("b")))))
    }
    test("parse a+b program") {
        val prog = """
program aplusb;
var
    a, b: integer;
    c: integer;
begin
    read(a);
    read(b);
    a:=a+b;
    write(a+b);
end.
"""
        val ast = ASTProg(
            ASTDecls(List(ASTVarDecl(ASTId("a"),ASTInt),
                          ASTVarDecl(ASTId("b"),ASTInt),
                          ASTVarDecl(ASTId("c"),ASTInt))),
                     List(),
                     ASTStmts(List(ASTRead(ASTVar(ASTId("a"))),
                                   ASTRead(ASTVar(ASTId("b"))),
                                   ASTAsg(ASTVar(ASTId("a")),ASTPlus(ASTVar(ASTId("a")),ASTVar(ASTId("b")))),
                                   ASTWrite(ASTPlus(ASTVar(ASTId("a")),ASTVar(ASTId("b")))))))
        
        val fn = (str:String, ast: ASTProg) =>
            assert(TTBFParser.parse(TTBFParser.astProg, str).get == ast)
        fn(prog, ast)
    }
    test("parse subrt") {
        val prog = """
procedure aplusb(a,b: integer, var c,d: integer);
var
    e,f: integer;
begin
    e:=a+b;
    f:=c+d;
    d:=a-(b+c)-d-e-f;
end;"""
        TTBFParser.parse(TTBFParser.astSubrt, prog).get
    }
}



