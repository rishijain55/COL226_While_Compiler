structure AST =
struct
type ID = string
type NUM = int

datatype Program = PROG of ID* BLK 
and BLK = BLK of DEC*CMD

and DEC = DEC of ((DEC1) list)

and DEC1 = Dec of VARLIST*TY

and TY = INT 
| BOOL

and VARLIST = Vars of (VARIABLE list)

and CMD = CMD of COMMAND list

and COMMAND = SET of VARIABLE*EXPRESSION 
| read of VARIABLE 
| write of EXPRESSION 
| ITE of EXPRESSION*CMD*CMD 
| WH of  EXPRESSION*CMD

and EXPRESSION = PLUS of EXPRESSION * EXPRESSION
| MINUS of EXPRESSION * EXPRESSION
| TIMES of EXPRESSION * EXPRESSION
| DIV of EXPRESSION * EXPRESSION
| MOD of EXPRESSION * EXPRESSION
| AND of EXPRESSION* EXPRESSION
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
| EQ of EXPRESSION*EXPRESSION

and VARIABLE = VAR of ID



end

val typeMap : (string, AST.TY) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "not found")

val valueMap : (string, int) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "not found")

fun symbolTable([],Dec) = AST.DEC(Dec)
|  symbolTable(Declaration::DeclarationSeq, Dec) =
    let
        val AST.Dec(variableList,typ) = Declaration
        fun temp(AST.Vars([])) = 0
        
        | temp(AST.Vars(AST.VAR(name)::t)) = 
            let 
                val x = HashTable.insert typeMap (name, typ)
                val y = HashTable.insert valueMap(name, 0)
            in
                temp(AST.Vars(t))
            end
        
        val x = temp(variableList)
    in
      symbolTable(DeclarationSeq, Dec)
    end


