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

    operation SignMessage(message: Bool): Bool[][] {
        let k01 = [false, false, false];
        let k02 = [false, false, true];
        let k03 = [false, true, false];
        let k04 = [false, true, true];
        let k11 = [true, false, false];
        let k12 = [true, false, true];
        let k13 = [true, true, false];
        let k14 = [true, true, true];
        mutable k0 = [false, false, false];
        mutable k1 = [false, false, false];
        mutable k2 = [false, false, false];
        mutable k3 = [false, false, false];
        if(message == false){
            set k0 = k01;
            set k1 = k02;
            set k2 = k03;
            set k3 = k04;
        }
        else { 
            set k0 = k11;
            set k1 = k12;
            set k2 = k13;
            set k3 = k14;
        }
        return [[message], k0, k1, k2, k3];
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


    operation RealCheckSignature(message: Bool[][], publicKeys: Qubit[]): String{
        mutable s = 0;
        let c1=0;
        let c2= 0.9;
        let privateKeys = message[1 .. Length(message)- 1];
        use key = Qubit[12];
            RealQuantumHashing(privateKeys, key);
        for i in 0 .. Length(privateKeys)-1{
            if(!SwapTest(key[i*3 .. (i*3)+2], publicKeys[i*3 .. (i*3)+2])){
                set s += 1;
            }
        }
        ResetAll(key);
        mutable result = "Valid, not transferable";
        if(s <= c1 * 2){
            set result = "Valid, transferable";
        }
        if(Convert.IntAsDouble(s) >= c2 * 4.0){
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



    operation QuantumHashing(input: Bool[]): Qubit[]{
        use qubit = Qubit();
        let m = ConvertBoolArrayToInt(input);
        Ry(((2.0*PI()*m) * 0.0)/3.0, qubit);
        mutable result = [qubit];
        for i in 1 .. Length(input) - 1{
            use newQubit = Qubit();
            Ry((2.0*PI()*m * Convert.IntAsDouble(i))/3.0, newQubit);
            set result += [newQubit];
        }
        return result;
    }


    operation QuantumHashing2(input: Bool[]): Qubit[]{
       use qubits = Qubit[3];
       for qubit in qubits{
           H(qubit);
       }
       let m = ConvertBoolArrayToInt(input);
       for i in 0 .. PowI(2, Length(input)-1){
           let control = ConvertIntToBooleanArray(i);
           for j in 0 .. Length(qubits)-1{
               if(control[j]){
                   Ry((4.0*PI()*m * Convert.IntAsDouble(i))/3.0, qubits[j]);
               }
           }
       }
       for qubit in qubits{
           H(qubit);
       }
       return qubits;
    }

    operation QuantumHashingForForOneInput(input: Bool[], qubits:Qubit[]) : Unit{
         for qubit in qubits{
           H(qubit);
       }
       let m = ConvertBoolArrayToInt(input);
       for i in 0 .. PowI(2, Length(input)-1){
           let control = ConvertIntToBooleanArray(i);
           for j in 0 .. Length(qubits)-1{
               if(control[j]){
                   Ry((4.0*PI()*m * Convert.IntAsDouble(i))/3.0, qubits[j]);
               }
           }
       }
       for qubit in qubits{
           H(qubit);
       }
    }

    operation RealQuantumHashing(input: Bool[][], qubits:Qubit[]): Unit{
         for qubit in qubits{
           H(qubit);
       }
       for l in 0.. Length(input)-1{
           let m = ConvertBoolArrayToInt(input[l]);
           for i in 0 .. PowI(2, Length(input[l]))-1{
               let control = ConvertIntToBooleanArray(i);
               for j in 0 .. (Length(qubits)/Length(input) -1){
                   if(control[j]){
                       Ry((4.0*PI()*m * Convert.IntAsDouble(i))/3.0, qubits[j+(l*3)]);
                   }
               }
           }
       }
       for qubit in qubits{
           H(qubit);
       }
    }

    operation ConvertIntToBooleanArray(number: Int): Bool[]{
        mutable result = [false, false, false];
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

   
    operation Execute(): Unit{
        let message = false;
        let singnedMessage = SignMessage(message);
        let privateKeys = singnedMessage[1 .. Length(singnedMessage)- 1];
        let publicKeys = FormPublicKeys(privateKeys);
        let result = CheckSignature(singnedMessage, publicKeys);
        Message(result);
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

    @EntryPoint()
    operation RealExecute(): Unit{
        let message = false;
        let singnedMessage = SignMessage(message);
        let privateKeys = singnedMessage[1 .. Length(singnedMessage)- 1];
        use publicKeys = Qubit[3*Length(privateKeys)];
        RealQuantumHashing(privateKeys, publicKeys);
        let result = RealCheckSignature(singnedMessage, publicKeys);
        Message(result);
        ResetAll(publicKeys);
        let message2 = false;
        let singnedMessage2 = SignMessage(message2);
        let privateKeys2 = singnedMessage2[1 .. Length(singnedMessage2)- 1];
        use publicKeys2 = Qubit[3*Length(privateKeys2)];
        RealQuantumHashing(privateKeys2, publicKeys2);
        let result2 = RealCheckSignature(singnedMessage2, publicKeys2);
        Message(result2);
        ResetAll(publicKeys2);
    }
}
