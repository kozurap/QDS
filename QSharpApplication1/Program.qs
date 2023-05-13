namespace Quantum.QSharpApplication1 {

    open Microsoft.Quantum.Canon;
    open Microsoft.Quantum.Intrinsic;
    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Convert as Convert;
    
    operation SwapTest(q0: Qubit[], q1:Qubit[]) : Bool {
        if(Length(q0) != Length(q1) or Length(q0) == 0){
            return false;
        }
        mutable result = true;
        for i in 0 .. Length(q0) - 1{
            use qubit = Qubit();
            H(qubit);
            (Controlled SWAP)([qubit],(q0[i],q1[i]));
            H(qubit);
            let value = M(qubit);
            if (IsResultOne(value)){
                set result = false;
            }
        }
        return result;
    }

    operation SignMessage(message: Int, system: Int, lengthOfWord: Int, count: Int): Int[][] {
        let keys = MakeAllPossibleArraysOfGivenSystemAndLength(system, lengthOfWord);
        if(count*2>Length(keys) or (message!=0 and message!=1)){
            return [[0]];
        }
        if(message == 0){
            return keys[0 .. count - 1];
        }
        return keys[count .. count * 2 - 1];
    }

    operation MakeAllPossibleArraysOfGivenSystemAndLength(system: Int, length: Int): Int[][]{
        mutable result = [[0, size = length], size = PowI(system, length)];
        for i in 0 .. PowI(system, length)-2{
            for j in 0 .. i{
                mutable addOne = true;
                for n in 0 .. length - 1{
                    if(addOne){
                        if (result[j][length - n - 1] < system -1){
                            mutable tmp = result[j];
                            set tmp w/= length - n - 1 <- tmp[length-n-1]+1;
                            for l in 1 .. n{
                                set tmp w/= length - l <- 0;
                            }
                            set result w/= j <- tmp;
                            set addOne = false;
                        }
                    }
                }
            }
        }
        return result;
    }



    operation RealCheckSignature(privateKeys: Int[][], publicKeys: Qubit[] , system:Int, lgt1 : Int): Int{
        mutable s = 0;
        let c1=0;
        let c2= 0.9;
        use key = Qubit[Length(publicKeys)];
            RealQuantumHashing(privateKeys, system, key, lgt1);
        for i in 0 .. Length(privateKeys)-1{
            if(!SwapTest(key[i*Length(privateKeys[i]) .. (i*Length(privateKeys[i]))+2], publicKeys[i*Length(privateKeys[i]) .. (i*Length(privateKeys[i]))+2])){
                set s += 1;
            }
        }
        ResetAll(key);
        mutable result = 0;
        if(s <= c1 * 2){
            set result = 1;
        }
        if(Convert.IntAsDouble(s) >= c2 * 4.0){
            set result = -1;
        }
        return result;
    }


    operation ConvertBoolArrayToInt(input: Bool[]) : Double{
        mutable sum = 0.0;
        for i in 0 .. Length(input)-1{
            mutable myByte = 0.0;
            if(input[Length(input) - 1 - i]){
                set myByte = 1.0;
            }
            set sum += myByte * PowD(Convert.IntAsDouble(2), Convert.IntAsDouble(i));
        }
        return sum;
    }






    


    operation RealQuantumHashing(originalInput: Int[][], system: Int, qubits:Qubit[], lgt1:Int): Unit{
        for k in 0 .. Length(originalInput)-1{
        let q0 = qubits[k*lgt1];
        let q = qubits[k*lgt1+1 .. k * lgt1 + lgt1 -1];
        for qubit in qubits{
           H(qubit);
        }    
        let input = originalInput[k];
            let m = ConvertFromAnySystemToDeca(input, system);
            for i in 0 .. PowI(2, Length(q))-1{
                let control = ConvertFromDecaToBooleanArray(i, system, Length(q));
                (ControlledOnBitString(control, Ry((4.0*PI()*m * Convert.IntAsDouble(i))/3.0, _)))(q,q0);
            }
        }
        for qubit in qubits{
            H(qubit);
        }
    }

    function ConvertFromAnySystemToDeca(input: Int[], system: Int) : Double{
        mutable sum = 0.0;
        for i in 0 .. Length(input)-1{
            set sum += Convert.IntAsDouble(input[(Length(input) - 1 - i)]) * PowD(Convert.IntAsDouble(system), Convert.IntAsDouble(i));
        }
        return sum;
    }

    function ConvertOneNumberFromAnySystemToDeca(input: Int, system: Int) : Int{
        mutable sum = 0;
        mutable x = input;
        mutable i = 0;
        while(x!=0){
            let tmp = x % system;
            set sum += x * PowI(system, i);
            set x /= 2;
            set i +=1;
        }
        return sum;
    }

    function ConvertFromDecaToBooleanArray(input: Int, initialSystem: Int, initialLength: Int) : Bool[]{
        mutable result = [false, size = Ceiling(Lg(Convert.IntAsDouble(initialSystem))) * initialLength];
        mutable x = input;
        mutable i = 0;
        while(x>0) {
            if(x % 2 == 1){
                set result w/= Length(result) - i - 1 <- true;
            }
            set x = x / 2;
            set i += 1;
        }
        return result;
    }

    operation ConvertIntToBooleanArray(number: Int, system: Int): Bool[]{
        mutable result = [false];
        mutable num = number;
        mutable index = 2;
        repeat{
            let x = num % 2;
            if(x == 1){
                set result w/= index <- true;
            }
            set index -=1;
            set num = num/2;
        }
        until index<0;
        return result;
    }

     operation QuantumRandomNumberGenerator() : Result {
        // Allocate a qubit        
        use q = Qubit();  
        // Put the qubit to superposition
        // It now has a 50% chance of being measured 0 or 1  
        H(q);      
        // Measure the qubit value            
        return M(q); 
    }

    operation BB84(count: Int): Int[]{        
        use qubits = Qubit[count];
        for i in 0 .. count - 1{
            let bit = QuantumRandomNumberGenerator();
            if IsResultOne(bit) {
                X(qubits[i]);
            }
        }

        mutable b = [0, size = count];
        for i in 0 .. count - 1{
            let tmp = QuantumRandomNumberGenerator();
            if(IsResultOne(tmp)){
                set b w/= i <- 1;
            }
        }

        for i in 0 .. count - 1{
            let _base = b[i];
            if _base == 1 {
                H(qubits[i]);
            }
        }        


        // Alice send qubits to Bob
        // ...
        // Bob receives qubits


        mutable c = [0, size = count];
        for i in 0 .. count - 1{
            let tmp = QuantumRandomNumberGenerator();
            if(IsResultOne(tmp)){
                set c w/= i <- 1;
            }
        }

        for i in 0 .. count - 1{
            let _base = c[i];
            if _base == 1 {
                H(qubits[i]);
            }
        }


        mutable shared_secret = [];
        for i in 0 .. count - 1{
            if c[i] == b[i] {
                let misure = M(qubits[i]);
                mutable x = 0;
                if misure == One {
                    set x = 1;
                }
                set shared_secret = shared_secret + [x]; 
            }
        }   


        for i in 0 .. count - 1{
            Reset(qubits[i]);
        }
        
        return shared_secret;
    }


    @EntryPoint()
    operation RealExecute(system: Int, lengthOfWord: Int, countOfWords: Int, countOfQubitsForBB84: Int): Unit{
        let messages = BB84(countOfQubitsForBB84);
        //Asymmetric
        mutable results = [];
        for message in messages{
            let privateKeys = SignMessage(message, system, lengthOfWord, countOfWords);
            let t = Log(Convert.IntAsDouble(2 * PowI(system, lengthOfWord))) * (Convert.IntAsDouble(2)/PowD(0.9, Convert.IntAsDouble(2)));
            let lgt1 = Ceiling(Lg(t)) + 1;
            use publicKeys = Qubit[lgt1 * Length(privateKeys)];
            RealQuantumHashing(privateKeys, system, publicKeys, lgt1);
            let result = RealCheckSignature(privateKeys, publicKeys, system, lgt1);
            set results = results + [result];
            ResetAll(publicKeys);
        }
        mutable isTrasferable = true;
        mutable isValid = true;
        for res in results{
            if(res == -1){
                set isValid = false;
            }
            elif(res == 0){
                set isTrasferable = false;
            }
        }
        if(!isValid){
            Message("Invalid");
        }
        elif(!isTrasferable){
            Message("Valid, not transferable");
        }
        else{
            Message("Valid, transferable");
        }

        //Symmetric
        let tt = Log(Convert.IntAsDouble(2 * PowI(2, Length(messages)))) * (Convert.IntAsDouble(2)/PowD(0.9, Convert.IntAsDouble(2)));
        let lgtt1 = Ceiling(Lg(tt)) + 1;
        use privateKey = Qubit[lgtt1];
        let m = [messages];
        RealQuantumHashing(m, 2, privateKey, lgtt1);
        let result = RealCheckSignature(m, privateKey, 2, lgtt1);
        if(result == -1){
             Message("Invalid");
        }
        elif(result == 0){
            Message("Valid, not transferable");
        }
        else{
            Message("Valid, transferable");
        }
        ResetAll(privateKey);
    }
}
