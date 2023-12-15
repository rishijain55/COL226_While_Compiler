# HOW TO RUN:-

    run make
    run use "loader.sml";
    run execute(FileName);

# CONTEXT-FREE GRAMMAR : -

The Grammar used is following :-
<N,T,P,S>  
where : -

S := Start

Non Terminals N := START | PROGRAM |BLOCK | DECLARATIONSEQ | DECs | DECLARATION |TY | VARIABLELIST| VARs | COMMANDSEQ | COMMAND |EXPRESSION | FACTOR | COMPARISON |COMMANDS |VARIABLE

Terminals T := "tt" , "ff" , "&&" , "||" , "~" , "+" , "-" , "\*" , "/" , "%" , "!" , "if" , "then" , "else" , "endif" , "while" , "do" , "endwh" , ";" , "::" , ":=" , "," , digits , ID , ">" ,"<>" , ">=" , "<" , "="

Production Rules P :=

Start : Program

PROGRAM : "program" ID :: BLOCK

BLOCK: DECs COMMANDSEQ  
| COMMANDSEQ

DECs: DECLARATIONSEQ

DECLARATIONSEQ: DECLARATION DECLARATIONSEQ  
| DECLARATION

DECLARATION: "var" VARs : TY ;

TY : INT  
| BOOL

VARs : VARIABLELIST

VARIABLELIST : VARIABLE, VARIABLELIST  
| VARIABLE

COMMANDSEQ : "{" COMMANDS "}"  
| "{}"

COMMANDS: COMMAND ";" COMMANDS  
| COMMAND ";"

COMMAND : VARIABLE ":=" EXPRESSION  
| "read" VARIABLE  
| "write" EXPRESSION  
| "if" EXPRESSION "then" COMMANDSEQ "else" COMMANDSEQ "endif"  
| "while" EXPRESSION "do" COMMANDSEQ "endwh"

EXPRESSION : EXPRESSION "+" EXPRESSION  
| EXPRESSION "\*" EXPRESSION  
| EXPRESSION "-" EXPRESSION  
| EXPRESSION "/" EXPRESSION  
| EXPRESSION "%" EXPRESSION  
| EXPRESSION "&&" EXPRESSION  
| EXPRESSION "||" EXPRESSION  
| FACTOR

FACTOR : "tt"  
| "ff"  
| VARIABLE  
| COMPARISON  
| "(" EXPRESSION ")"  
| "!"EXPRESSION  
| digits
| "~"EXPRESSION

COMPARISON : EXPRESSION "<" EXPRESSION  
| EXPRESSION ">" EXPRESSION  
|EXPRESSION "<=" EXPRESSION  
|EXPRESSION "<>" EXPRESSION  
|EXPRESSION "=" EXPRESSION  
|EXPRESSION ">=" EXPRESSION

VARIABLE : ID

# AST datatype definition

AST data type is recursively defined as:-

datatype AST = PROG of ID\* BLK

and BLK = BLK of DEC\*CMD

and DEC = DEC of ((DEC1) list)

and DEC1 = Dec of VARLIST\*TY

and TY = INT  
| BOOL

and VARLIST = Vars of (VARIABLE list)

and CMD = CMD of COMMAND list

and COMMAND = SET of VARIABLE*EXPRESSION  
| read of VARIABLE  
| write of EXPRESSION  
| ITE of EXPRESSION*CMD*CMD  
| WH of EXPRESSION*CMD

and EXPRESSION = PLUS of EXPRESSION _ EXPRESSION  
| MINUS of EXPRESSION _ EXPRESSION  
| TIMES of EXPRESSION _ EXPRESSION  
| DIV of EXPRESSION _ EXPRESSION  
| MOD of EXPRESSION _ EXPRESSION  
| AND of EXPRESSION_ EXPRESSION  
| OR of EXPRESSION* EXPRESSION  
| TT of bool  
| FF of bool  
| VARIABLE of VARIABLE  
| NOT of EXPRESSION  
| NUM of NUM  
| NEGATE of EXPRESSION  
| LT of EXPRESSION*EXPRESSION  
| GT of EXPRESSION*EXPRESSION  
| GEQ of EXPRESSION*EXPRESSION  
| LEQ of EXPRESSION*EXPRESSION  
| NEQ of EXPRESSION*EXPRESSION  
| EQ of EXPRESSION\*EXPRESSION

and VARIABLE = VAR of ID

# SYNTAX DIRECTED TRANSLATION:-

With each grammar rule I assigned a rule by which a node is created or value is passed to the parent node:

On the right side of each production rule the value inside the bracket is passed to that pointer when that rule is followed.

The right side shows the syntax direted translation with AST.datatype declared in the previous section.

PROGRAM : "program" ID :: BLOCK (AST.PROG(ID,BLOCK))

BLOCK: DECs COMMANDSEQ (AST.BLK(DECs,COMMANDSEQ))  
| COMMANDSEQ (AST.BLK(AST.DEC([]),COMMANDSEQ))

DECs: DECLARATIONSEQ (AST.DEC(DECLARATIONSEQ))

DECLARATIONSEQ: DECLARATION DECLARATIONSEQ (DECLARATION:: DECLARATIONSEQ)  
| DECLARATION ([DECLARATION])

DECLARATION: "var" VARs : TY ; (AST.Dec(VARs,TY))

TY : INT (AST.INT)  
| BOOL (AST.BOOL)

VARs : VARIABLELIST (AST.Vars(VARIABLELIST))

VARIABLELIST : VARIABLE , VARIABLELIST (VARIABLE:: VARIABLELIST)  
| VARIABLE ([VARIABLE])

COMMANDSEQ : { COMMANDS } (AST.CMD(COMMANDS))  
| {} (AST.CMD([]))

COMMANDS: COMMAND ; COMMANDS (COMMAND:: COMMANDS)  
| COMMAND ; ([COMMAND])

COMMAND : VARIABLE ":=" EXPRESSION (AST.SET(VARIABLE,EXPRESSION)  
| "read" VARIABLE (AST.read(VARIABLE))  
| "write" EXPRESSION (AST.write(EXPRESSION))  
| "if" EXPRESSION "then" COMMANDSEQ "else" COMMANDSEQ "endif" (AST.ITE(EXPRESSION,COMMANDSEQ1,COMMANDSEQ2))  
| "while" EXPRESSION "do" COMMANDSEQ "endwh" (AST.WH(EXPRESSION,COMMANDSEQ))

EXPRESSION : EXPRESSION "+" EXPRESSION ( AST.PLUS(EXPRESSION1,EXPRESSION2))  
| EXPRESSION "\*" EXPRESSION ( AST.TIMES(EXPRESSION1,EXPRESSION2))  
| EXPRESSION "-" EXPRESSION ( AST.MINUS(EXPRESSION1,EXPRESSION2))  
| EXPRESSION "/" EXPRESSION ( AST.DIV(EXPRESSION1,EXPRESSION2))  
| EXPRESSION "%" EXPRESSION ( AST.MOD(EXPRESSION1,EXPRESSION2))  
| EXPRESSION "&&" EXPRESSION ( AST.AND(EXPRESSION1,EXPRESSION2))  
| EXPRESSION "||" EXPRESSION ( AST.OR(EXPRESSION1,EXPRESSION2))  
| FACTOR (FACTOR)

FACTOR : "tt" (AST.TT(true))  
| "ff" (AST.FF(false))  
| VARIABLE (AST.VARIABLE(VARIABLE))  
| COMPARISON (COMPARISON)  
| ( EXPRESSION) (EXPRESSION)  
| "!" EXPRESSION (AST.NOT(EXPRESSION))  
| digits (AST.NUM(digits))  
| "~" EXPRESSION (AST.NEGATE(EXPRESSION))

COMPARISON : EXPRESSION "<" EXPRESSION (AST.LT(EXPRESSION1,EXPRESSION2))  
| EXPRESSION "<=" EXPRESSION (AST.LEQ(EXPRESSION1,EXPRESSION2))  
|EXPRESSION "=" EXPRESSION (AST.EQ(EXPRESSION1,EXPRESSION2))  
|EXPRESSION "<>" EXPRESSION (AST.NEQ(EXPRESSION1,EXPRESSION2))  
|EXPRESSION ">" EXPRESSION (AST.GT(EXPRESSION1,EXPRESSION2))  
|EXPRESSION ">=" EXPRESSION (AST.GEQ(EXPRESSION1,EXPRESSION2))

VARIABLE : ID (AST.VAR(ID))

# Other Design Decisions

While creating AST, I have not done type checking or checking if a variable is defined or not. This can be handled after creating AST, using symbol table. In SML we can use hashtable for symboltable. Everytime a variable is declared we can keep track of its type and when in command we can check if value assigned to a variable is similar to type of variable. Also we can check if an expression has operators which mathches field of operand. We can do this while creating evaluater for the AST.

# ACKNOWLEDGEMENTS:-

The load-code(glue-code) file is standard file taken from Official site of SML
file:///C:/code/parser/README.md
