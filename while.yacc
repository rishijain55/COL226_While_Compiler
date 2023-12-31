

%%
(* required declarations *)
%name code

%term
  ID of string | NUM of int
  |NEGATE| EQ| LT| GT| GEQ| LEQ|NEQ| PLUS | MINUS| TIMES| DIV| MOD| IF|THEN | ELSE| INT| BOOL| COLON|DCOL|SET| TT| FF| NOT|AND| OR|LPAREN|RPAREN|LCBRACE|RCBRACE|TERMINAL|PROG|VAR|READ|WRITE|ENDIF|WHILE|DO|ENDWH| COMMA| EOF


%nonterm START of AST.Program| PROGRAM of AST.Program|BLOCK of AST.BLK| DECLARATIONSEQ of AST.DEC1 list| DECs of AST.DEC | DECLARATION of AST.DEC1|TY of AST.TY| VARIABLELIST of AST.VARIABLE list| VARs of AST.VARLIST | COMMANDSEQ of AST.CMD| COMMAND of AST.COMMAND |EXPRESSION of AST.EXPRESSION| FACTOR of AST.EXPRESSION | COMPARISON of AST.EXPRESSION |COMMANDS of AST.COMMAND list |VARIABLE of AST.VARIABLE                      

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF


(* %header  *)


%left OR
%left AND
%left EQ NEQ
%left LEQ GEQ GT LT
%left PLUS MINUS 
%left TIMES DIV MOD
%right NOT NEGATE

%start START

%verbose

%%



START : PROGRAM (PROGRAM)
 
PROGRAM : PROG ID DCOL BLOCK (AST.PROG(ID1,BLOCK))

BLOCK: DECs COMMANDSEQ  (AST.BLK(DECs,COMMANDSEQ))
| COMMANDSEQ (AST.BLK(AST.DEC([]),COMMANDSEQ))

DECs: DECLARATIONSEQ (symbolTable(DECLARATIONSEQ,DECLARATIONSEQ))

DECLARATIONSEQ: DECLARATION DECLARATIONSEQ (DECLARATION:: DECLARATIONSEQ)
| DECLARATION ([DECLARATION])

DECLARATION: VAR VARs COLON TY TERMINAL (AST.Dec(VARs,TY))

TY : INT (AST.INT)
| BOOL (AST.BOOL)

VARs : VARIABLELIST (AST.Vars(VARIABLELIST))

VARIABLELIST : VARIABLE COMMA VARIABLELIST (VARIABLE:: VARIABLELIST)
| VARIABLE ([VARIABLE])

COMMANDSEQ : LCBRACE COMMANDS RCBRACE (iscorrect(AST.CMD(COMMANDS)))
| LCBRACE RCBRACE (AST.CMD([]))

COMMANDS: COMMAND TERMINAL COMMANDS (COMMAND:: COMMANDS)
| COMMAND TERMINAL ([COMMAND])

COMMAND : VARIABLE SET EXPRESSION (AST.SET(VARIABLE,EXPRESSION))
| READ VARIABLE (AST.read(VARIABLE))
| WRITE EXPRESSION (AST.write(EXPRESSION))
| IF EXPRESSION THEN COMMANDSEQ ELSE COMMANDSEQ ENDIF (AST.ITE(EXPRESSION,COMMANDSEQ1,COMMANDSEQ2))
| WHILE EXPRESSION DO COMMANDSEQ ENDWH (AST.WH(EXPRESSION,COMMANDSEQ))

EXPRESSION : EXPRESSION PLUS EXPRESSION ( AST.PLUS(EXPRESSION1,EXPRESSION2))
| EXPRESSION TIMES EXPRESSION ( AST.TIMES(EXPRESSION1,EXPRESSION2))
| EXPRESSION MINUS EXPRESSION ( AST.MINUS(EXPRESSION1,EXPRESSION2))
| EXPRESSION DIV EXPRESSION ( AST.DIV(EXPRESSION1,EXPRESSION2))
| EXPRESSION MOD EXPRESSION ( AST.MOD(EXPRESSION1,EXPRESSION2))
| EXPRESSION AND EXPRESSION ( AST.AND(EXPRESSION1,EXPRESSION2))
| EXPRESSION OR EXPRESSION ( AST.OR(EXPRESSION1,EXPRESSION2))
| FACTOR (FACTOR)

FACTOR : TT (AST.TT(true))
| FF (AST.FF(false))
| VARIABLE (AST.VARIABLE(VARIABLE))
| COMPARISON (COMPARISON)
| LPAREN EXPRESSION RPAREN (EXPRESSION)
| NOT EXPRESSION (AST.NOT(EXPRESSION))
| NUM (AST.NUM(NUM1))
| NEGATE EXPRESSION (AST.NEGATE(EXPRESSION))

COMPARISON : EXPRESSION LT EXPRESSION (AST.LT(EXPRESSION1,EXPRESSION2))
| EXPRESSION LEQ EXPRESSION (AST.LEQ(EXPRESSION1,EXPRESSION2))
|EXPRESSION EQ EXPRESSION (AST.EQ(EXPRESSION1,EXPRESSION2))
|EXPRESSION NEQ EXPRESSION (AST.NEQ(EXPRESSION1,EXPRESSION2))
|EXPRESSION GT EXPRESSION (AST.GT(EXPRESSION1,EXPRESSION2))
|EXPRESSION GEQ EXPRESSION (AST.GEQ(EXPRESSION1,EXPRESSION2))

VARIABLE : ID (AST.VAR(ID1))

