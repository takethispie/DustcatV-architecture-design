namespace DustcatV
open System;

module DecodeStageModule =
    let rec strHexToBinary (i: string)=
        let toBinary(c: char) =
            match c with
            | '0' -> "0000"
            | '1' -> "0001"
            | '2' -> "0010"
            | '3' -> "0011"
            | '4' -> "0100"
            | '5' -> "0101"
            | '6' -> "0110"
            | '7' -> "0111"
            | '8' -> "1000"
            | '9' -> "1001"
            | 'a' -> "1010"
            | 'b' -> "1011"
            | 'c' -> "1100"
            | 'd' -> "1101"
            | 'e' -> "1110"
            | 'f' -> "1111"
            | _ -> ""
        Seq.map toBinary (i.ToLower()) |> String.Concat

            
    let InstructionLowDecode (inst: string) =
        let binary = inst |> strHexToBinary 
        let op = binary.Substring(0, 8)
        let target = int(binary.Substring(8, 4))
        let source1 = int(binary.Substring(12, 4))
        let source2 = int(binary.Substring(16, 4))
        let imm = binary.Substring(16, 16)
        match op.ToLower() with
        | "00000001" -> Integer(Add, target, source1, source2)
        | "00010001" -> ImmediateInteger(Add, target, source1, imm)
        | "00000010" -> Integer(Substract, target, source1, source2)
        | "00010010" -> ImmediateInteger(Substract, target, source1, imm)
        | "00000011" -> Integer(Multiply, target, source1, source2)
        | "00010011" -> ImmediateInteger(Multiply, target, source1, imm)
        | "00000100" -> Integer(Divide, target, source1, source2)
        | "00010100" -> ImmediateInteger(Divide, target, source1, imm)
        | _ -> Nope NopeOp


module DispatchStageModule =
    let GetStationId (item: ReservationStationUnit) =
        match item with 
        | Empty id -> id
        | Waiting (id,_,_,_,_,_,_) -> id
        | Ready(id,_,_,_,_) -> id
        | Running(id,_,_,_,_) -> id
        | Done(id,_,_) -> id
        

    let ReplaceStation (itemToUpdate: ReservationStationUnit, items: ReservationStations) = 
        items |> List.map (fun v -> if GetStationId(v) = GetStationId(itemToUpdate) then itemToUpdate else v)
        

    let HasNoInstruction (unit: ReservationStationUnit) =
        match unit with
        | Empty _ -> true
        | _ -> false


    let FreeStations stations =
        stations |> List.where(HasNoInstruction) 


    let TransformInstructionToStationData (inst: Instruction, id: int) =
        match inst with 
        | Integer(op, dest, left, right) -> Waiting(id, op, left, right, "0", "0", dest)
        | ImmediateInteger(op, dest, left, imm) -> Waiting(id, op, left, 0, "0", imm, dest)
        | SetRegister(op, dest, imm) -> Waiting(id, op, 0, 0, "0", imm, dest)
        | Memory(op, dest, source, offset) ->  Waiting(id, op, source, offset, "0", "0", dest)
        | _ -> raise(Exception("not transformable instruction"))


    let BookReservationStation (stations: ReservationStations, inst: Instruction) =
        match FreeStations(stations) with
        | head::_ ->  
            match head with 
            | Empty id -> ReplaceStation(TransformInstructionToStationData(inst, id), stations), true
            | _ -> stations, false
        | [] -> stations, false


    let getReadyStations (stations: ReservationStations ) =
        stations |> List.where(fun it -> 
            match it with
            | Ready _ -> true
            | _ -> false
        ) |> List.sortByDescending(fun (s: ReservationStationUnit )-> 
            match s with
            | Empty id -> id
            | Waiting (id, _, _, _, _, _, _) -> id
            | Ready (id, _, _, _, _) -> id
            | Running (id, _, _, _, _) -> id
            | Done (id, _, _) -> id
        )


module ExecutionStageModule =
    let ResolveSources (rUnit: ReservationStationUnit, message: CommonDataBusMessage)  =
        match rUnit with 
        | Waiting(id, op, qj, qk, vj, vk, dest) ->
            let (qj, vj) = if qj = message.Source then (0, message.Value) else (qj, "0")
            let (qk, vk) = if qk = message.Source then (0, message.Value) else (qk, "0")
            match (qj,qk) with
            | 0,0 -> Ready(id, op, vj, vk, dest) 
            | _ -> rUnit
        | _ -> rUnit


    let ArithmeticAndLogicUnit (op: Operation, left, right) =
        let parsedLeft = left |> int
        let parsedRight = right |> int
        match op with
        | Add -> parsedLeft + parsedRight
        | Substract -> parsedLeft - parsedRight
        | Divide when parsedRight <> 0 -> parsedLeft / parsedRight
        | Divide when parsedRight = 0 -> raise(DivideByZeroException())
        | Multiply -> parsedLeft * parsedRight
        | _ -> raise(Exception("Unit mismatch: instruction cannot be processed by this"))

    let runFunctionalUnit (rUnit: ReservationStationUnit, integerUnit, storeUnit) =
        match rUnit with
        | Running(id, op, vj, vk, dest) -> 
            match op with
            | Set -> storeUnit(rUnit)
            | Load -> storeUnit(rUnit)
            | Store -> storeUnit(rUnit)
            | Read -> storeUnit(rUnit)
            | Write -> storeUnit(rUnit)
            | Add -> integerUnit(rUnit)
            | Substract -> integerUnit(rUnit)
            | Divide -> integerUnit(rUnit)
            | Multiply -> integerUnit(rUnit)
            | _ -> raise(Exception("unknown runnable instruction"))
        | _ -> raise(Exception("station not in correct state"))


    let RunIntegerUnit (rUnit: ReservationStationUnit) =
        
        { Source = 0; Value = ""}


    let RunLoadStoreUnit (rUnit: ReservationStationUnit) =
        
        { Source = 0; Value = ""}


    let UpdateStations (cdbM: CommonDataBusMessage, stations: ReservationStations) =
        stations |> List.map(fun station -> 
            let rec updateState (item) = 
                match item with
                | Empty id -> Empty id
                | Waiting(id, op, qj, qk, vj, vk, dest) when qj <> 0 || qk <> 0 -> updateState(ResolveSources(item, cdbM))
                | Waiting(id, op, qj, qk, vj, vk, dest) when qj = 0 && qk = 0 -> Ready(id, op, vj, vk, dest)
                | Ready(id, op, vj, vk, dest) -> Ready(id, op, vj, vk, dest)
                | Running(id, op, vj, vk, dest) -> Running(id, op, vj, vk, dest) 
                | Done(id, _, _) -> Empty id
                | _ -> raise(Exception("unknown state"))
            updateState(station)
        )

    let setRunning(rUnit: ReservationStationUnit, items: ReservationStations, replace) =
        match rUnit with
        | Ready(id, op, vj, vk, dest) -> replace(Running(id, op, vj, vk, dest), items) 
        | _ -> raise(Exception("unknown state"))

    