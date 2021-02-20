namespace DustcatV

open System;

module FunctionalUnit =

    let IntegerFunctionalUnit (st: ReservationStationUnit)  =
        ignore

        
    let LoadStoreFunctionnalUnit (st: ReservationStationUnit) =
        ignore


module ExecutionStageModule =

    let HasNoInstruction (unit: ReservationStationUnit) =
        match unit with
        | Empty _ -> true
        | _ -> false

    let FreeStations stations =
        stations |> List.where(HasNoInstruction)

    let TransformInstructionToStationData (inst: Instruction, id: int) =
        match inst with 
        | Integer(op, dest, left, right) -> Waiting(id, op, left, right, "0", "0", dest)
        //TODO handle other instruction type 

    let BookReservationStation (station: ReservationStationUnit, inst: Instruction) =
        match station with
        | Empty id -> TransformInstructionToStationData(inst, id)
        | _ -> raise(Exception("reservation station state in unknown / wrong state"))


    let ResolveSources (unit: ReservationStationUnit, message: CommonDataBusMessage)  =
        match unit with 
        | Waiting(id, op, qj, qk, vj, vk, dest) ->
            let (qj, vj) = if qj = message.Source then (0, message.Value) else (qj, "0")
            let (qk, vk) = if qk = message.Source then (0, message.Value) else (qk, "0")
            match (qj,qk) with
            | 0,0 -> Ready(id, op, vj, vk, dest) 
            | _ -> unit
        | _ -> unit


    let UpdateStations (cdbM: CommonDataBusMessage, stations: ReservationStations) =
        stations |> List.map(fun station -> 
            let rec updateState (item) = 
                match item with
                | Empty id -> Empty id
                | Waiting(id, op, qj, qk, vj, vk, dest) when qj <> 0 || qk <> 0 -> updateState(ResolveSources(item, cdbM))
                | Waiting(id, op, qj, qk, vj, vk, dest) when qj = 0 && qk = 0 -> Ready(id, op, vj, vk, dest)
                | _ -> raise(Exception("unknown state"))
            updateState(station)
        )

    let GetStationId (item: ReservationStationUnit) =
        match item with 
        | Empty id -> id
        | Waiting (id,_,_,_,_,_,_) -> id
        | Ready(id,_,_,_,_) -> id
        | Running(id,_,_,_,_) -> id
        | Done(id,_,_,_,_,_) -> id

    let updateElement (itemToUpdate: ReservationStationUnit, items: ReservationStationUnit list) = 
        items |> List.map (fun v -> if GetStationId(v) = GetStationId(itemToUpdate) then itemToUpdate else v)
        

    let getReadyStations stations =
        stations |> List.where(fun it -> 
            match it with
            | Ready _ -> true
            | _ -> false
        )

module DecodeStageUnitsModule =
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

            
    let InstructionDecode (inst: string) =
        let binary = inst |> strHexToBinary 
        let op = binary.Substring(0, 8)
        let target = int(binary.Substring(8, 4))
        let source1 = int(binary.Substring(12, 4))
        let source2 = int(binary.Substring(16, 4))
        let imm = binary.Substring(16, 16)
        match op.ToLower() with
        | "00000001" -> Integer(Arithmetic Add, target, source1, source2)
        | "00010001" -> ImmediateInteger(ImmediateArithmetic Add, target, source1, imm)
        | "00000010" -> Integer(Arithmetic Substract, target, source1, source2)
        | "00010010" -> ImmediateInteger(ImmediateArithmetic Substract, target, source1, imm)
        | "00000011" -> Integer(Arithmetic Multiply, target, source1, source2)
        | "00010011" -> ImmediateInteger(ImmediateArithmetic Multiply, target, source1, imm)
        | "00000100" -> Integer(Arithmetic Divide, target, source1, source2)
        | "00010100" -> ImmediateInteger(ImmediateArithmetic Divide, target, source1, imm)
        | _ -> Nope NopeOp