structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  val line = ref 1
  val column = ref 0
  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e,l,c) => (
  print("Error at " ^  (Int.toString l) ^ " : " ^ (Int.toString c) ^ " with the error message: Error at " ^ e ^ "\n");
  raise invalidTokenErr("Error at " ^  (Int.toString l) ^ " : " ^ (Int.toString c) ^ " with the error message: Error at " ^ e ^ "\n")
  )
  
%%
%header (functor codeLexFun(structure Tokens:code_TOKENS));
letter=[A-Za-z];
digit = [0-9];
ws = [\ \t];
sign = [+~];
%%
"\n"       => (line := (!line) + 1;
            column := 0;
            pos := (!pos) + 1; lex());
{ws}+    => (lex());
"~" => (column := (!column) + size yytext;
            Tokens.NEGATE(!line,!column));
"="    => (column := (!column) + size yytext;
            Tokens.EQ(!line,!column));
"<"    => (column := (!column) + size yytext;
            Tokens.LT(!line,!column));   
">"    => (column := (!column) + size yytext;
            Tokens.GT(!line,!column));  
"<="    => (column := (!column) + size yytext;
            Tokens.LEQ(!line,!column));   
">="    => (column := (!column) + size yytext;
            Tokens.GEQ(!line,!column)); 
"<>"    => (column := (!column) + size yytext;
            Tokens.NEQ(!line,!column)); 
"+"    => (column := (!column) + size yytext;
            Tokens.PLUS(!line,!column));
"-"    => (column := (!column) + size yytext;
            Tokens.MINUS(!line,!column));
"*"    => (column := (!column) + size yytext;
            Tokens.TIMES(!line,!column));
"/"    => (column := (!column) + size yytext;
            Tokens.DIV(!line,!column));
"%"    => (column := (!column) + size yytext;
            Tokens.MOD(!line,!column));
"if"    => (column := (!column) + size yytext;
            Tokens.IF(!line,!column));
"then"    => (column := (!column) + size yytext;
            Tokens.THEN(!line,!column));
"else"    => (column := (!column) + size yytext;
            Tokens.ELSE(!line,!column));

"int"    => (column := (!column) + size yytext;
            Tokens.INT(!line,!column));
"bool"    => (column := (!column) + size yytext;
            Tokens.BOOL(!line,!column));
":"    => (column := (!column) + size yytext;
            Tokens.COLON(!line,!column));
"::" => (column := (!column) + size yytext;
            Tokens.DCOL(!line,!column));
":=" => (column := (!column) + size yytext;
            Tokens.SET(!line,!column));

"tt"    => (column := (!column) + size yytext;
            Tokens.TT(!line,!column));
"ff"    => (column := (!column) + size yytext;
            Tokens.FF(!line,!column));
"!"    => (column := (!column) + size yytext;
            Tokens.NOT(!line,!column));
"&&"    => (column := (!column) + size yytext;
            Tokens.AND(!line,!column));
"||"    => (column := (!column) + size yytext;
            Tokens.OR(!line,!column));
"("      => (column := (!column) + size yytext;
            Tokens.LPAREN(!line,!column));
")"      => (column := (!column) + size yytext;
              Tokens.RPAREN(!line,!column));
"{"      => (column := (!column) + size yytext;
            Tokens.LCBRACE(!line,!column));
"}"      => (column := (!column) + size yytext;
              Tokens.RCBRACE(!line,!column));
";"      => (column := (!column) + size yytext;
              Tokens.TERMINAL(!line,!column));
","      => (column := (!column) + size yytext;
              Tokens.COMMA(!line,!column));
"program"=> (column := (!column) + size yytext;
            Tokens.PROG(!line,!column));
"var" =>  (column := (!column) + size yytext;
            Tokens.VAR(!line,!column));
"read" =>  (column := (!column) + size yytext;
            Tokens.READ(!line,!column));
"write" =>  (column := (!column) + size yytext;
            Tokens.WRITE(!line,!column));
"endif" =>  (column := (!column) + size yytext;
            Tokens.ENDIF(!line,!column));
"while" =>  (column := (!column) + size yytext;
            Tokens.WHILE(!line,!column));
"do" =>  (column := (!column) + size yytext;
            Tokens.DO(!line,!column));
"endwh" =>  (column := (!column) + size yytext;
            Tokens.ENDWH(!line,!column));

{sign}?{digit}+ => (column := (!column) + size yytext;
            Tokens.NUM(valOf(Int.fromString(yytext)),!line,!column));
{letter}(({letter}|{digit})*) => (column := (!column) + size yytext;
            Tokens.ID(yytext,!line,!column));
.     => (error (yytext,!line,!column);
             lex());

