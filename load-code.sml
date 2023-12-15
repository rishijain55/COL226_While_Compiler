Control.Print.printDepth := 1000;

structure codeLrVals = codeLrValsFun(structure Token = LrParser.Token)
structure codeLex = codeLexFun(structure Tokens = codeLrVals.Tokens);
structure codeParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = codeLrVals.ParserData
     	       structure Lex = codeLex)
     
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error at line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
		in
		    codeParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  codeParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val EOFn = codeLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = codeParser.Stream.get lexer
    in
        if codeParser.sameToken(nextToken, EOFn) then postfix(result)
 	else (TextIO.output(TextIO.stdOut, "Warning: input not taken \n"); postfix(result))
    end

val parseString = parse o stringToLexer


fun LexerFrFile f =
    let val inStream = TextIO.openIn f
		val str = TextIO.inputAll inStream 
        val _ = TextIO.closeIn inStream
        val done = ref false
        val lexer=  codeParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in 
        lexer
    end

val parseIt = parse o LexerFrFile