

exception NotBool

fun evalNot(num(0)) = 1 |
    evalNot(num(1)) = 0 |
    evalNot(_) = raise NotBool;

fun evalExp(num(m),num(n),OR) = if (m=1 orelse n=1) then 1 else 0 | 
    evalExp(num(m),num(n),AND) = if (m=1 andalso n=1) then 1 else 0 |
    evalExp(num(m),num(n),PLUS) = m + n |
    evalExp(num(m),num(n),MINUS) = let val a =  print( Int.toString m)  in m - n end |
    evalExp(num(m),num(n),TIMES) = m * n |
    evalExp(num(m),num(n),DIV) = (m div n) |
    evalExp(num(m),num(n),MOD) = (m mod n) |
    evalExp(num(m),num(n),LT) = if m<n then 1 else 0 |
    evalExp(num(m),num(n),GT) = if m>n then 1 else 0 |
    evalExp(num(m),num(n),LEQ) = if m<=n then 1 else 0|
    evalExp(num(m),num(n),GEQ) = if m>=n then 1 else 0|
    evalExp(num(m),num(n),EQ) = if m=n then 1 else 0 |
    evalExp(num(m),num(n),NEQ) = if m<>n then 1 else 0;

signature VMC = 
    sig
        val rules: tokens FunStack.stack * (string,int) HashTable.hash_table * tokens FunStack.stack -> tokens FunStack.stack * (string,int) HashTable.hash_table * tokens FunStack.stack
        val toString: tokens FunStack.stack * (string,int) HashTable.hash_table * tokens FunStack.stack -> string
    end

structure Vmc : VMC =
    struct
    fun toString(V,M,C) = 
     let 
                val a = FunStack.toString printTokens C 
                val c = FunStack.toString printTokens V 
                val b = HashTable.listItems M
                val d = FunStack.list2stack b
                val e = FunStack.toString printInt d
                in "( "^c^", "^e^", "^a^" )" 
    end

        fun

            rules(V,M,C) = 
                let 
                val c1 = if(FunStack.depth C = 0) then WASTE else FunStack.top C;
                val c2 = if(FunStack.depth C < 2) then WASTE else FunStack.nth C 1;
                val c3 = if(FunStack.depth C < 3) then WASTE else FunStack.nth C 2;
                val c4 = if(FunStack.depth C < 4) then WASTE else FunStack.nth C 3;
                val v3 = if(FunStack.depth V < 3) then WASTE else FunStack.nth V 2;
                val v4 = if(FunStack.depth V < 4) then WASTE else FunStack.nth V 3;
                val v1 = if(FunStack.depth V = 0) then WASTE else FunStack.top V;
                val v2 = if(FunStack.depth V < 2) then WASTE else FunStack.nth V 1;
                val Vd1: tokens FunStack.stack= if(FunStack.depth V < 1) then FunStack.create else FunStack.drop V 1;
                val Vd2 : tokens FunStack.stack = if(FunStack.depth V < 2) then FunStack.create else FunStack.drop V 2;
                val Vd3 : tokens FunStack.stack = if(FunStack.depth V < 3) then FunStack.create else FunStack.drop V 3;
                val Cd1 : tokens FunStack.stack = if(FunStack.depth C < 1) then FunStack.create else FunStack.drop C 1;
                val Cd2 : tokens FunStack.stack = if(FunStack.depth C < 2) then FunStack.create else FunStack.drop C 2;
                val Cd3 : tokens FunStack.stack = if(FunStack.depth C < 3) then FunStack.create else FunStack.drop C 3;
                val Cd4 : tokens FunStack.stack = if(FunStack.depth C < 4) then FunStack.create else FunStack.drop C 4;
                val empstack : tokens FunStack.stack = FunStack.create;

                fun isVar(setvar(x)) = true
                    |
                    isVar(_) = false;
                fun    isid(id(x))= true
                    |
                    isid(_)=false;
                fun    evalVar(setvar(x))= x
                    |
                    evalVar(_)= "Noth";
                fun    iscmdseq(CMDSEQ(x))= true
                    |
                    iscmdseq(_)=false;

                fun    evalcmdseq(CMDSEQ(x))= x
                    |
                     evalcmdseq(_)= empstack;

                fun    isite(ITE)= true
                    | isite(_) = false;
                fun    iswh(WH)= true
                    | iswh(_) =false;
                fun    evalid(id(x))= x
                    |
                    evalid(_)= "NOthing";
                fun    evalnum(num(x))= x
                    |
                    evalnum(_)= ~999;
                fun    isnum(num(x))= true
                    | isnum(_) = false;
                fun    isboolexp(BOOLEXP(x))= true
                    | isboolexp(_) = false;
                fun isread(READ) = true
                    | isread(_) = false;
                fun iswrite(WRITE) = true 
                    |iswrite(_) = false;

                fun    evalboolexp(BOOLEXP(x))= x
                    | evalboolexp(_) = FunStack.create;
                fun    isnum0(num(0))= true
                    | isnum0(_) = false;
                fun    isnum1(num(1))= true
                    | isnum1(_) = false;
                fun    isneg(NEG)= true
                    | isneg(_) = false;
                fun    isnot(NOT)= true
                    | isnot(_) = false;
                fun    isset(SET)= true
                    | isset(_) = false;
                fun    isseq(SEQ)= true
                    | isseq(_) = false;
                fun    isexptokens(PLUS)= true
                    | isexptokens(MINUS)= true
                    | isexptokens(DIV)= true
                    | isexptokens(MOD)= true
                    | isexptokens(TIMES)= true
                    | isexptokens(AND)= true
                    | isexptokens(OR)= true
                    | isexptokens(GT)= true
                    | isexptokens(GEQ)= true
                    | isexptokens(LT)= true
                    | isexptokens(LEQ)= true
                    | isexptokens(EQ)= true
                    | isexptokens(NEQ)= true
                    | isexptokens(_)= false;
                fun write(num(x)) = let val a = print((Int.toString x)^"\n") in 0 end
                fun read() = 
                let
                    val a = print("enter the value: ")
                    val str = valOf (TextIO.inputLine TextIO.stdIn)
                    val i : int = valOf (Int.fromString str)
                in
                 i 
                end

                val Vnew = if(isVar(c1)) then let val a = evalVar(c1) val a1 = id(a) val b: tokens FunStack.stack = FunStack.push a1 V in b end
                else if(isseq(c1)) then V
                else if(isid(c1)) then let val s = evalid(c1) val eval = num(HashTable.lookup M s) val b: tokens FunStack.stack = FunStack.push eval  V in b end
                else if(isnum(c1)) then let val b: tokens FunStack.stack = FunStack.push c1 V in b end
                else if(isnum(v1) andalso iswrite(c1) ) then let val b = write(v1) in Vd1 end
                else if(isid(v1) andalso isread(c1) ) then let val a1 = read() val a2 = evalid(v1)  val x = HashTable.insert M (a2,a1) in Vd1 end
                else if(isnum(v1) andalso isneg(c1)) then let val eval = num(~1*evalnum(v1)) val b: tokens FunStack.stack = FunStack.push eval Vd1 in b end
                else if(isnum(v1) andalso isnot(c1)) then let val eval = num(evalNot(v1)) val b: tokens FunStack.stack = FunStack.push eval Vd1  in b end
                else if(isnum(v1) andalso isnum(v2) andalso isexptokens(c1)) then let val eval = num(evalExp(v2,v1,c1)) val b : tokens FunStack.stack = FunStack.push eval  Vd2  in b end
                else if(isnum(v1) andalso isid(v2) andalso isset(c1)) then let val a1 = evalid(v2) val a2 = evalnum(v1)  val x = HashTable.insert M (a1,a2) in Vd2 end
                else if(isboolexp(c1) andalso iscmdseq(c2) andalso iscmdseq(c3) andalso isite(c4)) then V
                else if(isnum0(v1) andalso iscmdseq(c1) andalso iscmdseq(c2) andalso isite(c3)) then  Vd1
                else if(isnum1(v1) andalso iscmdseq(c1) andalso iscmdseq(c2) andalso isite(c3)) then  Vd1
                else if(isboolexp(c1) andalso iscmdseq(c2) andalso iswh(c3)) then let val gote : tokens FunStack.stack = FunStack.push c1  V   val b : tokens FunStack.stack=  FunStack.push c2 gote  in b end
                else if(isnum0(v1) andalso iscmdseq(v2) andalso isboolexp(v3) andalso iswh(c1)) then  Vd3            
                else if(isnum1(v1) andalso iscmdseq(v2) andalso isboolexp(v3) andalso iswh(c1) ) then  Vd3   
                else V;   

                val Cnew = if(isVar(c1)) then Cd1
                else if(isseq(c1)) then Cd1
                else if(isid(c1)) then Cd1
                else if(isnum(c1)) then Cd1 
                else if(isnum(v1) andalso iswrite(c1) ) then Cd1
                else if(isid(v1) andalso isread(c1) ) then Cd1
                else if(isnum(v1) andalso isneg(c1)) then Cd1
                else if(isnum(v1) andalso isnot(c1)) then Cd1
                else if(isnum(v1) andalso isnum(v2) andalso isexptokens(c1)) then Cd1
                else if(isnum(v1) andalso isid(v2) andalso isset(c1)) then Cd1
                else if(isboolexp(c1) andalso iscmdseq(c2) andalso iscmdseq(c3) andalso isite(c4)) then let val c =  evalboolexp(c1) val b =  FunStack.merge c Cd1 in b end 
                else if(isnum0(v1) andalso iscmdseq(c1) andalso iscmdseq(c2) andalso isite(c3)) then  FunStack.merge (evalcmdseq(c2) : tokens FunStack.stack) Cd3
                else if(isnum1(v1) andalso iscmdseq(c1) andalso iscmdseq(c2) andalso isite(c3)) then  FunStack.merge (evalcmdseq(c1): tokens FunStack.stack) Cd3
                else if(isboolexp(c1) andalso iscmdseq(c2) andalso iswh(c3)) then FunStack.merge (evalboolexp(c1): tokens FunStack.stack) Cd2
                else if(isnum0(v1) andalso iscmdseq(v2) andalso isboolexp(v3) andalso iswh(c1)) then  Cd1       
                else if(isnum1(v1) andalso iscmdseq(v2) andalso isboolexp(v3)andalso iswh(c1)) then  FunStack.merge (evalcmdseq(v2): tokens FunStack.stack) (FunStack.push v3 ((FunStack.push v2 C): tokens FunStack.stack): tokens FunStack.stack)  
                else V;        
                val empst = FunStack.create
                val stop = if(c1 = WASTE) then true else false
                val a = toString(Vnew,M,Cnew)
                val b = print(a^"\n\n\n");
                in 
                    if(stop)  then (V,M,C)
                    else rules(Vnew,M,Cnew)
                end ;

    end
