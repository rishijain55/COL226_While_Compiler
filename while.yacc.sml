functor codeLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : code_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\005\000\000\000\
\\001\000\001\000\019\000\000\000\
\\001\000\001\000\019\000\002\000\042\000\003\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\028\000\037\000\000\000\
\\001\000\001\000\019\000\015\000\027\000\031\000\026\000\035\000\025\000\
\\036\000\024\000\038\000\023\000\000\000\
\\001\000\004\000\065\000\005\000\064\000\006\000\063\000\007\000\062\000\
\\008\000\061\000\009\000\060\000\010\000\059\000\011\000\058\000\
\\012\000\057\000\013\000\056\000\014\000\055\000\016\000\069\000\
\\026\000\054\000\027\000\053\000\000\000\
\\001\000\004\000\065\000\005\000\064\000\006\000\063\000\007\000\062\000\
\\008\000\061\000\009\000\060\000\010\000\059\000\011\000\058\000\
\\012\000\057\000\013\000\056\000\014\000\055\000\026\000\054\000\
\\027\000\053\000\029\000\085\000\000\000\
\\001\000\004\000\065\000\005\000\064\000\006\000\063\000\007\000\062\000\
\\008\000\061\000\009\000\060\000\010\000\059\000\011\000\058\000\
\\012\000\057\000\013\000\056\000\014\000\055\000\026\000\054\000\
\\027\000\053\000\039\000\052\000\000\000\
\\001\000\017\000\088\000\000\000\
\\001\000\018\000\049\000\019\000\048\000\000\000\
\\001\000\020\000\029\000\000\000\
\\001\000\021\000\006\000\000\000\
\\001\000\022\000\030\000\000\000\
\\001\000\030\000\013\000\000\000\
\\001\000\030\000\013\000\034\000\012\000\000\000\
\\001\000\031\000\031\000\000\000\
\\001\000\032\000\032\000\000\000\
\\001\000\032\000\070\000\000\000\
\\001\000\033\000\004\000\000\000\
\\001\000\037\000\090\000\000\000\
\\001\000\040\000\087\000\000\000\
\\001\000\042\000\000\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\034\000\012\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\041\000\028\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\001\000\019\000\015\000\027\000\035\000\025\000\036\000\024\000\
\\038\000\023\000\000\000\
\\109\000\004\000\065\000\005\000\064\000\006\000\063\000\007\000\062\000\
\\008\000\061\000\009\000\060\000\010\000\059\000\011\000\058\000\
\\012\000\057\000\013\000\056\000\014\000\055\000\026\000\054\000\
\\027\000\053\000\000\000\
\\110\000\000\000\
\\111\000\004\000\065\000\005\000\064\000\006\000\063\000\007\000\062\000\
\\008\000\061\000\009\000\060\000\010\000\059\000\011\000\058\000\
\\012\000\057\000\013\000\056\000\014\000\055\000\026\000\054\000\
\\027\000\053\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\012\000\057\000\013\000\056\000\014\000\055\000\000\000\
\\115\000\000\000\
\\116\000\012\000\057\000\013\000\056\000\014\000\055\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\004\000\065\000\005\000\064\000\006\000\063\000\007\000\062\000\
\\008\000\061\000\009\000\060\000\010\000\059\000\011\000\058\000\
\\012\000\057\000\013\000\056\000\014\000\055\000\000\000\
\\120\000\004\000\065\000\005\000\064\000\006\000\063\000\007\000\062\000\
\\008\000\061\000\009\000\060\000\010\000\059\000\011\000\058\000\
\\012\000\057\000\013\000\056\000\014\000\055\000\026\000\054\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\010\000\059\000\011\000\058\000\012\000\057\000\013\000\056\000\
\\014\000\055\000\000\000\
\\131\000\010\000\059\000\011\000\058\000\012\000\057\000\013\000\056\000\
\\014\000\055\000\000\000\
\\132\000\005\000\064\000\006\000\063\000\007\000\062\000\008\000\061\000\
\\010\000\059\000\011\000\058\000\012\000\057\000\013\000\056\000\
\\014\000\055\000\000\000\
\\133\000\005\000\064\000\006\000\063\000\007\000\062\000\008\000\061\000\
\\010\000\059\000\011\000\058\000\012\000\057\000\013\000\056\000\
\\014\000\055\000\000\000\
\\134\000\010\000\059\000\011\000\058\000\012\000\057\000\013\000\056\000\
\\014\000\055\000\000\000\
\\135\000\010\000\059\000\011\000\058\000\012\000\057\000\013\000\056\000\
\\014\000\055\000\000\000\
\\136\000\000\000\
\"
val actionRowNumbers =
"\017\000\021\000\000\000\010\000\
\\013\000\024\000\027\000\012\000\
\\025\000\022\000\001\000\003\000\
\\026\000\023\000\033\000\009\000\
\\031\000\065\000\011\000\014\000\
\\015\000\002\000\002\000\001\000\
\\035\000\002\000\001\000\008\000\
\\002\000\034\000\037\000\053\000\
\\054\000\050\000\006\000\002\000\
\\002\000\052\000\051\000\002\000\
\\057\000\040\000\039\000\004\000\
\\032\000\016\000\030\000\029\000\
\\038\000\036\000\012\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\005\000\056\000\058\000\012\000\
\\028\000\019\000\049\000\048\000\
\\047\000\046\000\044\000\045\000\
\\043\000\062\000\060\000\064\000\
\\063\000\059\000\061\000\055\000\
\\007\000\042\000\012\000\018\000\
\\041\000\020\000"
val gotoT =
"\
\\001\000\089\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\009\000\004\000\008\000\005\000\007\000\006\000\006\000\
\\010\000\005\000\000\000\
\\000\000\
\\004\000\012\000\006\000\006\000\000\000\
\\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\008\000\016\000\009\000\015\000\016\000\014\000\000\000\
\\011\000\020\000\015\000\019\000\016\000\018\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\034\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\041\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\016\000\042\000\000\000\
\\000\000\
\\012\000\043\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\008\000\044\000\016\000\014\000\000\000\
\\007\000\045\000\000\000\
\\012\000\048\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\000\000\
\\011\000\020\000\015\000\049\000\016\000\018\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\064\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\065\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\000\000\
\\000\000\
\\012\000\066\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\069\000\000\000\
\\012\000\070\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\071\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\072\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\073\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\074\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\075\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\076\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\077\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\078\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\079\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\080\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\081\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\012\000\082\000\013\000\033\000\014\000\032\000\016\000\031\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\084\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\087\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 90
val numrules = 45
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | VARIABLE of unit ->  (AST.VARIABLE)
 | COMMANDS of unit ->  (AST.COMMAND list)
 | COMPARISON of unit ->  (AST.EXPRESSION)
 | FACTOR of unit ->  (AST.EXPRESSION)
 | EXPRESSION of unit ->  (AST.EXPRESSION)
 | COMMAND of unit ->  (AST.COMMAND)
 | COMMANDSEQ of unit ->  (AST.CMD) | VARs of unit ->  (AST.VARLIST)
 | VARIABLELIST of unit ->  (AST.VARIABLE list)
 | TY of unit ->  (AST.TY) | DECLARATION of unit ->  (AST.DEC1)
 | DECs of unit ->  (AST.DEC)
 | DECLARATIONSEQ of unit ->  (AST.DEC1 list)
 | BLOCK of unit ->  (AST.BLK) | PROGRAM of unit ->  (AST.Program)
 | START of unit ->  (AST.Program)
end
type svalue = MlyValue.svalue
type result = AST.Program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 41) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "NEGATE"
  | (T 3) => "EQ"
  | (T 4) => "LT"
  | (T 5) => "GT"
  | (T 6) => "GEQ"
  | (T 7) => "LEQ"
  | (T 8) => "NEQ"
  | (T 9) => "PLUS"
  | (T 10) => "MINUS"
  | (T 11) => "TIMES"
  | (T 12) => "DIV"
  | (T 13) => "MOD"
  | (T 14) => "IF"
  | (T 15) => "THEN"
  | (T 16) => "ELSE"
  | (T 17) => "INT"
  | (T 18) => "BOOL"
  | (T 19) => "COLON"
  | (T 20) => "DCOL"
  | (T 21) => "SET"
  | (T 22) => "TT"
  | (T 23) => "FF"
  | (T 24) => "NOT"
  | (T 25) => "AND"
  | (T 26) => "OR"
  | (T 27) => "LPAREN"
  | (T 28) => "RPAREN"
  | (T 29) => "LCBRACE"
  | (T 30) => "RCBRACE"
  | (T 31) => "TERMINAL"
  | (T 32) => "PROG"
  | (T 33) => "VAR"
  | (T 34) => "READ"
  | (T 35) => "WRITE"
  | (T 36) => "ENDIF"
  | (T 37) => "WHILE"
  | (T 38) => "DO"
  | (T 39) => "ENDWH"
  | (T 40) => "COMMA"
  | (T 41) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM1, PROGRAM1left, 
PROGRAM1right)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  (PROGRAM as PROGRAM1) = PROGRAM1 ()
 in (PROGRAM)
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, PROGRAM1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.BLOCK BLOCK1, _, BLOCK1right)) :: _ :: ( _, 
( MlyValue.ID ID1, _, _)) :: ( _, ( _, PROG1left, _)) :: rest671)) =>
 let val  result = MlyValue.PROGRAM (fn _ => let val  ID1 = ID1 ()
 val  (BLOCK as BLOCK1) = BLOCK1 ()
 in (AST.PROG(ID1,BLOCK))
end)
 in ( LrTable.NT 1, ( result, PROG1left, BLOCK1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, _, COMMANDSEQ1right)
) :: ( _, ( MlyValue.DECs DECs1, DECs1left, _)) :: rest671)) => let
 val  result = MlyValue.BLOCK (fn _ => let val  (DECs as DECs1) = 
DECs1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (AST.BLK(DECs,COMMANDSEQ))
end)
 in ( LrTable.NT 2, ( result, DECs1left, COMMANDSEQ1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, COMMANDSEQ1left, 
COMMANDSEQ1right)) :: rest671)) => let val  result = MlyValue.BLOCK
 (fn _ => let val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (AST.BLK(AST.DEC([]),COMMANDSEQ))
end)
 in ( LrTable.NT 2, ( result, COMMANDSEQ1left, COMMANDSEQ1right), 
rest671)
end
|  ( 4, ( ( _, ( MlyValue.DECLARATIONSEQ DECLARATIONSEQ1, 
DECLARATIONSEQ1left, DECLARATIONSEQ1right)) :: rest671)) => let val  
result = MlyValue.DECs (fn _ => let val  (DECLARATIONSEQ as 
DECLARATIONSEQ1) = DECLARATIONSEQ1 ()
 in (symbolTable(DECLARATIONSEQ,DECLARATIONSEQ))
end)
 in ( LrTable.NT 4, ( result, DECLARATIONSEQ1left, 
DECLARATIONSEQ1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.DECLARATIONSEQ DECLARATIONSEQ1, _, 
DECLARATIONSEQ1right)) :: ( _, ( MlyValue.DECLARATION DECLARATION1, 
DECLARATION1left, _)) :: rest671)) => let val  result = 
MlyValue.DECLARATIONSEQ (fn _ => let val  (DECLARATION as DECLARATION1
) = DECLARATION1 ()
 val  (DECLARATIONSEQ as DECLARATIONSEQ1) = DECLARATIONSEQ1 ()
 in (DECLARATION:: DECLARATIONSEQ)
end)
 in ( LrTable.NT 3, ( result, DECLARATION1left, DECLARATIONSEQ1right),
 rest671)
end
|  ( 6, ( ( _, ( MlyValue.DECLARATION DECLARATION1, DECLARATION1left, 
DECLARATION1right)) :: rest671)) => let val  result = 
MlyValue.DECLARATIONSEQ (fn _ => let val  (DECLARATION as DECLARATION1
) = DECLARATION1 ()
 in ([DECLARATION])
end)
 in ( LrTable.NT 3, ( result, DECLARATION1left, DECLARATION1right), 
rest671)
end
|  ( 7, ( ( _, ( _, _, TERMINAL1right)) :: ( _, ( MlyValue.TY TY1, _,
 _)) :: _ :: ( _, ( MlyValue.VARs VARs1, _, _)) :: ( _, ( _, VAR1left,
 _)) :: rest671)) => let val  result = MlyValue.DECLARATION (fn _ =>
 let val  (VARs as VARs1) = VARs1 ()
 val  (TY as TY1) = TY1 ()
 in (AST.Dec(VARs,TY))
end)
 in ( LrTable.NT 5, ( result, VAR1left, TERMINAL1right), rest671)
end
|  ( 8, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.TY (fn _ => (AST.INT))
 in ( LrTable.NT 6, ( result, INT1left, INT1right), rest671)
end
|  ( 9, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.TY (fn _ => (AST.BOOL))
 in ( LrTable.NT 6, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.VARIABLELIST VARIABLELIST1, 
VARIABLELIST1left, VARIABLELIST1right)) :: rest671)) => let val  
result = MlyValue.VARs (fn _ => let val  (VARIABLELIST as 
VARIABLELIST1) = VARIABLELIST1 ()
 in (AST.Vars(VARIABLELIST))
end)
 in ( LrTable.NT 8, ( result, VARIABLELIST1left, VARIABLELIST1right), 
rest671)
end
|  ( 11, ( ( _, ( MlyValue.VARIABLELIST VARIABLELIST1, _, 
VARIABLELIST1right)) :: _ :: ( _, ( MlyValue.VARIABLE VARIABLE1, 
VARIABLE1left, _)) :: rest671)) => let val  result = 
MlyValue.VARIABLELIST (fn _ => let val  (VARIABLE as VARIABLE1) = 
VARIABLE1 ()
 val  (VARIABLELIST as VARIABLELIST1) = VARIABLELIST1 ()
 in (VARIABLE:: VARIABLELIST)
end)
 in ( LrTable.NT 7, ( result, VARIABLE1left, VARIABLELIST1right), 
rest671)
end
|  ( 12, ( ( _, ( MlyValue.VARIABLE VARIABLE1, VARIABLE1left, 
VARIABLE1right)) :: rest671)) => let val  result = 
MlyValue.VARIABLELIST (fn _ => let val  (VARIABLE as VARIABLE1) = 
VARIABLE1 ()
 in ([VARIABLE])
end)
 in ( LrTable.NT 7, ( result, VARIABLE1left, VARIABLE1right), rest671)

end
|  ( 13, ( ( _, ( _, _, RCBRACE1right)) :: ( _, ( MlyValue.COMMANDS 
COMMANDS1, _, _)) :: ( _, ( _, LCBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.COMMANDSEQ (fn _ => let val  (COMMANDS as 
COMMANDS1) = COMMANDS1 ()
 in (iscorrect(AST.CMD(COMMANDS)))
end)
 in ( LrTable.NT 9, ( result, LCBRACE1left, RCBRACE1right), rest671)

end
|  ( 14, ( ( _, ( _, _, RCBRACE1right)) :: ( _, ( _, LCBRACE1left, _))
 :: rest671)) => let val  result = MlyValue.COMMANDSEQ (fn _ => (
AST.CMD([])))
 in ( LrTable.NT 9, ( result, LCBRACE1left, RCBRACE1right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.COMMANDS COMMANDS1, _, COMMANDS1right)) ::
 _ :: ( _, ( MlyValue.COMMAND COMMAND1, COMMAND1left, _)) :: rest671))
 => let val  result = MlyValue.COMMANDS (fn _ => let val  (COMMAND as 
COMMAND1) = COMMAND1 ()
 val  (COMMANDS as COMMANDS1) = COMMANDS1 ()
 in (COMMAND:: COMMANDS)
end)
 in ( LrTable.NT 14, ( result, COMMAND1left, COMMANDS1right), rest671)

end
|  ( 16, ( ( _, ( _, _, TERMINAL1right)) :: ( _, ( MlyValue.COMMAND 
COMMAND1, COMMAND1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMANDS (fn _ => let val  (COMMAND as COMMAND1) = COMMAND1
 ()
 in ([COMMAND])
end)
 in ( LrTable.NT 14, ( result, COMMAND1left, TERMINAL1right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right
)) :: _ :: ( _, ( MlyValue.VARIABLE VARIABLE1, VARIABLE1left, _)) :: 
rest671)) => let val  result = MlyValue.COMMAND (fn _ => let val  (
VARIABLE as VARIABLE1) = VARIABLE1 ()
 val  (EXPRESSION as EXPRESSION1) = EXPRESSION1 ()
 in (AST.SET(VARIABLE,EXPRESSION))
end)
 in ( LrTable.NT 10, ( result, VARIABLE1left, EXPRESSION1right), 
rest671)
end
|  ( 18, ( ( _, ( MlyValue.VARIABLE VARIABLE1, _, VARIABLE1right)) :: 
( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMAND (fn _ => let val  (VARIABLE as VARIABLE1) = VARIABLE1
 ()
 in (AST.read(VARIABLE))
end)
 in ( LrTable.NT 10, ( result, READ1left, VARIABLE1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right
)) :: ( _, ( _, WRITE1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMAND (fn _ => let val  (EXPRESSION as EXPRESSION1) = 
EXPRESSION1 ()
 in (AST.write(EXPRESSION))
end)
 in ( LrTable.NT 10, ( result, WRITE1left, EXPRESSION1right), rest671)

end
|  ( 20, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ2, _, _)) :: _ :: ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, _,
 _)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, _)) :: ( _, (
 _, IF1left, _)) :: rest671)) => let val  result = MlyValue.COMMAND
 (fn _ => let val  (EXPRESSION as EXPRESSION1) = EXPRESSION1 ()
 val  COMMANDSEQ1 = COMMANDSEQ1 ()
 val  COMMANDSEQ2 = COMMANDSEQ2 ()
 in (AST.ITE(EXPRESSION,COMMANDSEQ1,COMMANDSEQ2))
end)
 in ( LrTable.NT 10, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 21, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ1, _, _)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, _,
 _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMAND (fn _ => let val  (EXPRESSION as EXPRESSION1) = 
EXPRESSION1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (AST.WH(EXPRESSION,COMMANDSEQ))
end)
 in ( LrTable.NT 10, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in ( AST.PLUS(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 11, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in ( AST.TIMES(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 11, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in ( AST.MINUS(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 11, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in ( AST.DIV(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 11, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in ( AST.MOD(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 11, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in ( AST.AND(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 11, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in ( AST.OR(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 11, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.FACTOR FACTOR1, FACTOR1left, FACTOR1right))
 :: rest671)) => let val  result = MlyValue.EXPRESSION (fn _ => let
 val  (FACTOR as FACTOR1) = FACTOR1 ()
 in (FACTOR)
end)
 in ( LrTable.NT 11, ( result, FACTOR1left, FACTOR1right), rest671)

end
|  ( 30, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.FACTOR (fn _ => (AST.TT(true)))
 in ( LrTable.NT 12, ( result, TT1left, TT1right), rest671)
end
|  ( 31, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.FACTOR (fn _ => (AST.FF(false)))
 in ( LrTable.NT 12, ( result, FF1left, FF1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.VARIABLE VARIABLE1, VARIABLE1left, 
VARIABLE1right)) :: rest671)) => let val  result = MlyValue.FACTOR (fn
 _ => let val  (VARIABLE as VARIABLE1) = VARIABLE1 ()
 in (AST.VARIABLE(VARIABLE))
end)
 in ( LrTable.NT 12, ( result, VARIABLE1left, VARIABLE1right), rest671
)
end
|  ( 33, ( ( _, ( MlyValue.COMPARISON COMPARISON1, COMPARISON1left, 
COMPARISON1right)) :: rest671)) => let val  result = MlyValue.FACTOR
 (fn _ => let val  (COMPARISON as COMPARISON1) = COMPARISON1 ()
 in (COMPARISON)
end)
 in ( LrTable.NT 12, ( result, COMPARISON1left, COMPARISON1right), 
rest671)
end
|  ( 34, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPRESSION 
EXPRESSION1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.FACTOR (fn _ => let val  (EXPRESSION as 
EXPRESSION1) = EXPRESSION1 ()
 in (EXPRESSION)
end)
 in ( LrTable.NT 12, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 35, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.FACTOR (fn _ => let val  (EXPRESSION as EXPRESSION1) = 
EXPRESSION1 ()
 in (AST.NOT(EXPRESSION))
end)
 in ( LrTable.NT 12, ( result, NOT1left, EXPRESSION1right), rest671)

end
|  ( 36, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.FACTOR (fn _ => let val  (NUM as NUM1)
 = NUM1 ()
 in (AST.NUM(NUM1))
end)
 in ( LrTable.NT 12, ( result, NUM1left, NUM1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.EXPRESSION EXPRESSION1, _, EXPRESSION1right
)) :: ( _, ( _, NEGATE1left, _)) :: rest671)) => let val  result = 
MlyValue.FACTOR (fn _ => let val  (EXPRESSION as EXPRESSION1) = 
EXPRESSION1 ()
 in (AST.NEGATE(EXPRESSION))
end)
 in ( LrTable.NT 12, ( result, NEGATE1left, EXPRESSION1right), rest671
)
end
|  ( 38, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.COMPARISON (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (AST.LT(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 13, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 39, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.COMPARISON (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (AST.LEQ(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 13, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 40, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.COMPARISON (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (AST.EQ(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 13, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 41, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.COMPARISON (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (AST.NEQ(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 13, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 42, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.COMPARISON (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (AST.GT(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 13, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 43, ( ( _, ( MlyValue.EXPRESSION EXPRESSION2, _, EXPRESSION2right
)) :: _ :: ( _, ( MlyValue.EXPRESSION EXPRESSION1, EXPRESSION1left, _)
) :: rest671)) => let val  result = MlyValue.COMPARISON (fn _ => let
 val  EXPRESSION1 = EXPRESSION1 ()
 val  EXPRESSION2 = EXPRESSION2 ()
 in (AST.GEQ(EXPRESSION1,EXPRESSION2))
end)
 in ( LrTable.NT 13, ( result, EXPRESSION1left, EXPRESSION2right), 
rest671)
end
|  ( 44, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.VARIABLE (fn _ => let val  ID1 = ID1 ()
 in (AST.VAR(ID1))
end)
 in ( LrTable.NT 15, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : code_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun DCOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun SET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun LCBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun RCBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun TERMINAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun PROG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
