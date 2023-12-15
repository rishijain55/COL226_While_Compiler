fun execute(filename) = 

    let

        val d= HashTable.clear valueMap
        val e= HashTable.clear typeMap
        val a = parseIt(filename);
        val V = FunStack.create;
        val b =  Vmc.rules(V,valueMap,a);
    in
        (Vmc.toString(b),a)
    end;