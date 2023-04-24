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
        (Controlled SWAP)([qubit], (q0[i], q1[i]));
        let value = M(qubit);
        if (IsResultOne(value)){
            set result = false;
        }
        }
         return result;
    }

    operation SignMessage(message: Bool): Bool[][] {
        let k01 = [true, false, false];
        let k02 = [true, false, true];
        let k11 = [true, true, false];
        let k12 = [true, true, true];
        mutable k0 = [false, false, false];
        mutable k1 = [false, false, false];
        if(message == false){
            set k0 = k01;
            set k1 = k02;
        }
        else { 
            set k0 = k11;
            set k1 = k12;
        }
        return [[message], k0, k1];
    }

    operation CheckSignature(message: Bool[][], publicKeys: Qubit[][]) : String {
        mutable s = 0;
        let c1=0;
        let c2= 0.9;
        for i in 1 .. Length(message)-1{
            let key = QuantumHashing(message[i]);
            if(!SwapTest(key, publicKeys[i])){
                set s += 1;
            }
        }
        mutable result = "Valid, not transferable";
        if(s <= c1 * 2){
            set result = "Valid, transferable";
        }
        if(Convert.IntAsDouble(s) >= c2 * 2.0){
            set result = "Invalid";
        }
        return result;
    }

    operation FormPublicKeys(keys: Bool[][]): Qubit[][] {
        mutable result = [QuantumHashing(keys[0])];
        for i in 1 .. Length(keys) - 1{
            let hash = QuantumHashing(keys[i]);
            set result += [QuantumHashing(keys[i])];
        }
        return result;
    }


    operation QuantumHashing(input: Bool[]): Qubit[]{
        use qubit = Qubit();
        mutable m = 0.0;
        if(input[0]){
            set m = 1.0;
        }
        Rx((2.0*PI()*m)/3.0, qubit);
        mutable result = [qubit];
        for i in 1 .. Length(input) - 1{
            use newQubit = Qubit();
            if(input[i]){
                set m = 1.0;
            }
            else{
                set m = 0.0;
            }
            Rx((2.0*PI()*m * Convert.IntAsDouble(i))/3.0, newQubit);
            set result += [newQubit];
        }
        return result;
    }
    @EntryPoint()
    operation Execute(): Unit{
        let message = false;
        let singnedMessage = SignMessage(message);
        let privateKeys = singnedMessage[1 .. Length(singnedMessage)- 1];
        let publicKeys = FormPublicKeys(privateKeys);
        let result = CheckSignature(singnedMessage, publicKeys);
        for i in 0 .. Length(publicKeys){
            ResetAll(publicKeys[i]);
        }
        let message1 = true;
        let singnedMessage1 = SignMessage(message);
        let privateKeys1 = singnedMessage[1 .. Length(singnedMessage)- 1];
        let publicKeys1 = FormPublicKeys(privateKeys);
        let result1 = CheckSignature(singnedMessage, publicKeys);
        for i in 0 .. Length(publicKeys){
            ResetAll(publicKeys[i]);
        }
        Message(result1);
    }
}
