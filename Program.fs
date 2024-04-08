open System // Add an open directive for the System namespace

module LogicalOperations =

    let invert x =
        match x with
        | 1 -> 0
        | _ -> 1

    let orFunc x y = 
        match x + y with
        | 0 -> 0 
        | _ -> 1

    let andFunc x y = 
        match x + y with 
        | 2 -> 1
        | _ -> 0

    let xorFunc x y = 
        match x + y with
        | 1 -> 1
        | _ -> 0

    let invertPipeline values =
        values |> List.map invert

    let orPipeline  = List.map2 orFunc

    let andPipeline  = List.map2 andFunc

    let xorPipeline  = List.map2 xorFunc

    let hexStringToInt string = System.Convert.ToInt32(string, 16)

    let toBase2List num =
        let rec loop n acc i =
            match i with 
            | 8 -> acc
            | _ ->  loop (n >>> 1 ) (n % 2 :: acc) (i + 1)
        loop num [] 0

    let base2ListToInt base2list =
        let rec loop list num =
            match list with 
            | [] -> num
            | _ -> loop list.Tail ((num <<< 1) + list.Head)
        loop base2list 0

    let addTwoBinaryLists binList1 binList2 =
        let rec padWithZeros list length =
            if List.length list < length then padWithZeros (0 :: list) length
            else list

        let length = max (List.length binList1) (List.length binList2)
        let paddedBinList1 = padWithZeros binList1 length
        let paddedBinList2 = padWithZeros binList2 length

        let rec loop (binList1: int list) (binList2: int list) acc carry =
            match binList1 with
            | [] -> acc
            |_ ->  
                let sum = (binList1.Head + binList2.Head + carry)
                let digit = sum % 2
                let newCarry = sum / 2
                loop binList1.Tail binList2.Tail (digit :: acc) newCarry

        let a1 = List.rev paddedBinList1
        let b1 = List.rev paddedBinList2
        loop a1 b1 [] 0

    let twosComplement binList =
        let notBinList = invertPipeline binList
        let one = toBase2List 1
        let acc = addTwoBinaryLists notBinList one
        acc    

    let signedToBase2List num =
        match num with
        | num when num < 0 -> 
            let absnum = num * -1
            absnum |> toBase2List |> twosComplement
        | _ -> toBase2List num

    let subtractTwoBinaryLists binList1 binList2 =
        addTwoBinaryLists binList1 (twosComplement binList2)

    let base2ListToSigned (bitlist: int list) =
        match bitlist.Head with 
        | 1 ->
            let num = bitlist |> twosComplement |> base2ListToInt
            num * -1
        | _ ->
            bitlist |> base2ListToInt

    let printResult bitlist1 bitlist2 operator ans vType =
        match vType with
        | "hex" -> 
                printfn "         %A = %X" bitlist1 (base2ListToInt bitlist1)
                printfn "%s%s         %A = %X" operator " " bitlist2 (base2ListToInt bitlist2)
                printfn "--------------------------------------------"
                printfn "         %A = %X" ans (base2ListToInt ans)
        | "dec" -> 
                printfn "         %A = %i" bitlist1 (base2ListToSigned bitlist1)
                printfn "%s%s         %A = %i" operator " " bitlist2 (base2ListToSigned bitlist2)
                printfn "--------------------------------------------"
                printfn "         %A = %i" ans (base2ListToSigned ans)

    let input vType= 
        match vType with
        | "hex" ->
            printf "Enter Hex value between 00 and FF: "
            let byte = System.Console.ReadLine()
            hexStringToInt byte |> toBase2List
        | "dec" -> 
            printf "Enter a number between -128 and 127: "
            let byte = System.Console.ReadLine()
            Int32.Parse(byte) |> signedToBase2List

    let operation () =
        printf "\nEnter the operation you want to perform (NOT, OR, AND, XOR, ADD, SUB or QUIT): "
        let operator = System.Console.ReadLine().ToUpper()
        match operator with
        | "NOT" ->
            let tmp1 = input "hex"
            let tmp2 = invertPipeline tmp1
            printfn "Result of NOT %A = %A = %X" tmp1 tmp2 (base2ListToInt tmp2)
            true
        | "AND" -> 
            let tmp1 = input "hex" 
            let tmp2 = input "hex"
            let ans = andPipeline tmp1 tmp2
            printResult tmp1 tmp2 operator ans "hex"
            true
        | "OR" -> 
            let tmp1 = input "hex"
            let tmp2 = input "hex"
            let ans = orPipeline tmp1 tmp2
            printResult tmp1 tmp2 operator ans "hex"
            true
        | "XOR" -> 
            let tmp1 = input "hex"
            let tmp2 = input "hex"
            let ans = xorPipeline tmp1 tmp2
            printResult tmp1 tmp2 operator ans "hex"
            true
        | "ADD" -> 
            let tmp1 = input "dec" 
            let tmp2 = input "dec"
            let ans = addTwoBinaryLists tmp1 tmp2
            printResult tmp1 tmp2 operator ans "dec"
            true
        | "SUB" -> 
            let tmp = input "dec"
            let tmp2 = input "dec"
            let ans = subtractTwoBinaryLists tmp tmp2
            printResult tmp tmp2 operator ans "dec"
            true
        | "QUIT" |_ -> 
            printfn "Quit Successful!"
            false

    let rec main () =
        match operation () with
        | false -> ()
        | _ -> main ()

LogicalOperations.main()
