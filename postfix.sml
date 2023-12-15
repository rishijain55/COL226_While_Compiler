
datatype tokens = id of string|CMDSEQ of (tokens FunStack.stack)| BOOLEXP of (tokens FunStack.stack)|setvar of string | num of int  | SET| READ | WRITE | ITE | WH | NEG|LT|GT|LEQ|GEQ|EQ|NEQ|PLUS|MINUS|TIMES|DIV|MOD|AND|OR|NOT|SEQ| bo of bool| WASTE

val stk = FunStack.create;

fun printInt( a ) = Int.toString( a );
fun printTokens( id(x))  =x|
    printTokens( CMDSEQ([])) ="" |
    printTokens( CMDSEQ(x)) = let val h1 = FunStack.top(x) val t = FunStack.drop x 1 in (printTokens(h1):string)^", "^(printTokens(CMDSEQ(t)):string) end |
    printTokens( BOOLEXP([])) ="" |
    printTokens( BOOLEXP(x)) = let val h1 = FunStack.top(x) val t = FunStack.drop x 1 in (printTokens(h1):string)^", "^(printTokens(BOOLEXP(t)):string) end |
    printTokens( setvar(x)) = x|
    printTokens( num(x)) =  Int.toString x | 
    printTokens(SET) = "SET"|
    printTokens(READ) = "READ"|
    printTokens(WRITE) = "WRITE"|
    printTokens(ITE) = "ITE"|
    printTokens(WH) = "WH"|
    printTokens(NEG) = "NEG"|
    printTokens(LT) = "LT"|
    printTokens(GT) = "GT"|
    printTokens(LEQ) = "LEQ"|
    printTokens(GEQ) = "GEQ"|
    printTokens(EQ) = "EQ"|
    printTokens(NEQ) = "NEQ"|
    printTokens(PLUS) = "PLUS"|
    printTokens(MINUS) = "MINUS"|
    printTokens(TIMES) = "TIMES"|
    printTokens(DIV) = "DIV"|
    printTokens(MOD) = "MOD"|
    printTokens(AND) = "AND"|
    printTokens(OR) = "OR"|
    printTokens(SEQ) = "SEQ"|
    printTokens(NOT) = "NOT"

fun toast(AST.VAR(name))= [id name]
fun setvari(AST.VAR(name))=[setvar name]
fun postfixexp(AST.TT(true)) = [num 1] |
    postfixexp(AST.FF(false)) = [ num 0 ] |
    postfixexp(AST.NUM(n)) = [ num n ] |
    postfixexp(AST.VARIABLE(var)) = toast(var) |
    postfixexp(AST.PLUS(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[PLUS] |
    postfixexp(AST.MINUS(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[MINUS] |
    postfixexp(AST.DIV(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[DIV] |
    postfixexp(AST.MOD(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[MOD] |
    postfixexp(AST.TIMES(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[TIMES] |
    postfixexp(AST.AND(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[AND] |
    postfixexp(AST.OR(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[OR] |
    postfixexp(AST.NOT(exp))= postfixexp(exp)@[NOT] |
    postfixexp(AST.NEGATE(exp))= postfixexp(exp)@[NEG] |
    postfixexp(AST.LT(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[LT] |
    postfixexp(AST.LEQ(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[LEQ] |
    postfixexp(AST.GT(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[GT] |
    postfixexp(AST.GEQ(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[GEQ] |
    postfixexp(AST.NEQ(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[NEQ] |
    postfixexp(AST.EQ(exp1,exp2))= postfixexp(exp1)@postfixexp(exp2)@[EQ]

fun postfixcmd(AST.CMD([]),f): tokens list = [ SEQ ] |
    postfixcmd(AST.CMD((h::t)),f) = f(h)@postfixcmd(AST.CMD(t),f)

fun postfixcmdlis(AST.CMD(c),f) = let 
    val a = postfixcmd(AST.CMD(c),f) 
    val b = FunStack.list2stack a
    in 
    [CMDSEQ b]
     end; 
fun postfixboollis(exp) = let 
    val a = postfixexp(exp) 
    val b = FunStack.list2stack a
    in 
    [BOOLEXP b]
     end; 

fun postfixcom(AST.SET(var,exp)) = setvari(var)@postfixexp(exp)@[SET] |
    postfixcom(AST.read(var)) = setvari(var)@[READ] |
    postfixcom(AST.write(exp)) = postfixexp(exp)@[WRITE] |
    postfixcom(AST.ITE(exp,cmd1,cmd2)) =postfixboollis(exp)@postfixcmdlis(cmd1,postfixcom)@postfixcmdlis(cmd2,postfixcom)@[ITE] |
    postfixcom(AST.WH(exp,cmd)) = postfixboollis(exp)@postfixcmdlis(cmd,postfixcom)@[WH] 

fun postfix(AST.PROG(a,AST.BLK(d,c)))= 
    let val s = postfixcmd(c,postfixcom)
        in
    FunStack.list2stack(s)
    end ;

exception TypeError

       
        fun checkvar(AST.VAR(name),t)= (HashTable.lookup typeMap name) = t;

        fun checkExp(AST.PLUS(exp1,exp2),AST.INT) = checkExp(exp1,AST.INT) andalso checkExp(exp2,AST.INT) |
            checkExp(AST.MINUS(exp1,exp2),AST.INT) = checkExp(exp1,AST.INT) andalso checkExp(exp2,AST.INT) |
            checkExp(AST.TIMES(exp1,exp2),AST.INT) = checkExp(exp1,AST.INT) andalso checkExp(exp2,AST.INT) |
            checkExp(AST.DIV(exp1,exp2),AST.INT) = checkExp(exp1,AST.INT) andalso checkExp(exp2,AST.INT) |
            checkExp(AST.MOD(exp1,exp2),AST.INT) = checkExp(exp1,AST.INT) andalso checkExp(exp2,AST.INT) |
            checkExp(AST.AND(exp1,exp2),AST.BOOL) = checkExp(exp1,AST.BOOL) andalso checkExp(exp2,AST.BOOL) |
            checkExp(AST.OR(exp1,exp2),AST.BOOL) = checkExp(exp1,AST.BOOL) andalso checkExp(exp2,AST.BOOL) |
            checkExp(AST.GT(exp1,exp2),AST.BOOL) = ((checkExp(exp1,AST.INT)  andalso checkExp(exp2,AST.INT)) orelse (checkExp(exp1,AST.BOOL)  andalso checkExp(exp2,AST.BOOL)))   |
            checkExp(AST.LT(exp1,exp2),AST.BOOL) = ((checkExp(exp1,AST.INT) andalso checkExp(exp2,AST.INT)) orelse (checkExp(exp1,AST.BOOL)  andalso checkExp(exp2,AST.BOOL)))   |
            checkExp(AST.GEQ(exp1,exp2),AST.BOOL) = (checkExp(exp1,AST.INT) andalso checkExp(exp2,AST.INT)) orelse (checkExp(exp1,AST.BOOL)  andalso checkExp(exp2,AST.BOOL))   |
            checkExp(AST.LEQ(exp1,exp2),AST.BOOL) = (checkExp(exp1,AST.INT) andalso checkExp(exp2,AST.INT)) orelse (checkExp(exp1,AST.BOOL)  andalso checkExp(exp2,AST.BOOL))   |
            checkExp(AST.EQ(exp1,exp2),AST.BOOL) = (checkExp(exp1,AST.INT) andalso checkExp(exp2,AST.INT)) orelse (checkExp(exp1,AST.BOOL)  andalso checkExp(exp2,AST.BOOL))   |
            checkExp(AST.PLUS(exp1,exp2),AST.BOOL) = false|
            checkExp(AST.MINUS(exp1,exp2),AST.BOOL) = false|
            checkExp(AST.TIMES(exp1,exp2),AST.BOOL) = false|
            checkExp(AST.DIV(exp1,exp2),AST.BOOL) = false|
            checkExp(AST.MOD(exp1,exp2),AST.BOOL) = false|
            checkExp(AST.AND(exp1,exp2),AST.INT) = false|
            checkExp(AST.OR(exp1,exp2),AST.INT) = false|
            checkExp(AST.GT(exp1,exp2),AST.INT) = false|
            checkExp(AST.LT(exp1,exp2),AST.INT) = false|
            checkExp(AST.GEQ(exp1,exp2),AST.INT) = false|
            checkExp(AST.LEQ(exp1,exp2),AST.INT) = false|
            checkExp(AST.EQ(exp1,exp2),AST.INT) = false|
            checkExp(AST.NEQ(exp1,exp2),AST.INT) = false|
            checkExp(AST.TT(true),AST.INT) = false |
            checkExp(AST.TT(true),AST.BOOL) = true |
            checkExp(AST.FF(false),AST.INT) = false |
            checkExp(AST.FF(false),AST.BOOL) = true |
            checkExp(AST.VARIABLE(variable),t) = checkvar(variable,t) |
            checkExp(AST.NOT(exp),AST.BOOL)= checkExp(exp,AST.BOOL)|
            checkExp(AST.NOT(exp),AST.INT)= false|
            checkExp(AST.NEGATE(exp),AST.INT)= checkExp(exp,AST.INT)|
            checkExp(AST.NEGATE(exp),AST.BOOL)= false|
            checkExp(AST.NUM(x),AST.INT) = true|
            checkExp(AST.NUM(x),AST.BOOL) = false

        fun checkcmd(AST.CMD([]),f)= true|
            checkcmd(AST.CMD((h::t)),f) = f(h) andalso checkcmd(AST.CMD((t)),f)

         fun checkcommand(AST.SET(AST.VAR(name),exp))= let  val b =  (HashTable.lookup typeMap name) in checkExp(exp,b) end |
             checkcommand(AST.read(AST.VAR(name))) = true |
             checkcommand(AST.write(exp))= checkExp(exp,AST.INT) orelse checkExp(exp,AST.BOOL) |
             checkcommand(AST.ITE(exp,cmd1,cmd2))= checkExp(exp,AST.BOOL) andalso checkcmd(cmd1,checkcommand) andalso checkcmd(cmd2,checkcommand) |
             checkcommand(AST.WH(exp,cmd))= checkExp(exp,AST.BOOL) andalso checkcmd(cmd,checkcommand)

        fun iscorrect(AST.CMD(x)) = let val a = checkcmd(AST.CMD(x),checkcommand) in if ( a ) then AST.CMD(x) else raise TypeError end