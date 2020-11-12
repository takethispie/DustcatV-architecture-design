namespace DustcatV

open System;
open System.Linq;

module FunctionalUnit =

    // TODO: fix vj being empty
    let IntegerFunctionalUnit (st: ReservationStationUnit)  =
        let (newState, message) =
            match st.State with
            | Ready state -> 
                let res = 
                    match state.Op.ToLower() with
                    | "add" -> ((state.Vj |> int) + (state.Vk |> int)).ToString() 
                    | "addi" -> ((state.Vj |> int) + (state.Vk |> int)).ToString() 
                    | "sub" -> ((state.Vj |> int) - (state.Vk |> int)).ToString()
                    | "subi" -> ((state.Vj |> int) - (state.Vk |> int)).ToString()
                    | "mul" -> ((state.Vj |> int) * (state.Vk |> int)).ToString()
                    | "muli" -> ((state.Vj |> int) * (state.Vk |> int)).ToString()
                    | "div" when state.Vk <> "0" -> ((state.Vj |> int) / (state.Vk |> int)).ToString()
                    | "div" when state.Vk = "0" -> raise(DivideByZeroException())
                    | "divi" when state.Vk <> "0" -> ((state.Vj |> int) / (state.Vk |> int)).ToString()
                    | "divi" when state.Vk = "0" -> raise(DivideByZeroException())
                    | _ -> ""
                ((Running state), { Source = st.Id; Value = res })
            | _ -> raise(Exception("already running station"))
        ({ Id = st.Id; State = newState; Result = message.Value; Rt = st.Rt}, message)

        
    let LoadStoreFunctionnalUnit (st: ReservationStationUnit) =
        let (newState, message) =
            match st.State with
            | Ready state -> 
                match state.Op with
                | "lw" -> 
                    ((Running state),{ Source = 0; Value = state.Vk })
                | _ -> raise(Exception("unknown operation"))
            | _ -> raise(Exception("already running station"))
        ({ Id = st.Id; State = newState; Result = message.Value; Rt = st.Rt}, message)


module ExecutionStageModule =

    let HasNoInstruction unit =
        match unit.State with
        | Empty(_) -> true
        | _ -> false

    let FreeStations stations =
        stations |> List.where(HasNoInstruction)


    let BookReservationStation (station: ReservationStationUnit, inst: Instruction) =
        let newState = 
            match station.State with
            | Empty _ -> Waiting { Op = inst.Op; Qj = inst.Qj; Qk = (if inst.Imm = "" then inst.Qk else 0); Vj = (if inst.Qj = 0 then "0" else ""); Vk = (if inst.Imm = "" then "" else inst.Imm)}
            | _ -> raise(Exception("reservation station state in unknown / wrong state"))
        { Id = station.Id; State = newState;  Result = ""; Rt = inst.Qt }


    let ResolveSources (state: ReservationState, message: CommonDataBusMessage)  =
        let (qj, vj) = if state.Qj = message.Source then (0, message.Value) else (state.Qj, "0")
        let (qk, vk) = if state.Qk = message.Source then (0, message.Value) else (state.Qk, "0")
        { Op = state.Op; Qj = qj; Qk = qk; Vj = vj; Vk = vk }


    let UpdateStations (cdbM: CommonDataBusMessage, stations: ReservationStations) =
        stations |> List.map(fun station -> 
            let rec updateState (item) = 
                match item.State with
                | Empty state -> Empty state
                | Waiting state when state.Qj <> 0 && state.Qk <> 0 -> 
                    updateState({ Id = station.Id; State = Waiting (ResolveSources(state, cdbM)); Result = ""; Rt = station.Rt})
                | Waiting state when state.Qj = 0 && state.Qk = 0 -> Ready state
                | Ready state -> Ready state
                | Running state -> Running state
                | _ -> raise(Exception("unknown state"))
            let newState = updateState(station)
            { Id = station.Id; State = newState; Result = ""; Rt = station.Rt }
        )



    let updateElement (itemToUpdate: ReservationStationUnit, items: ReservationStationUnit list) = 
        items |> List.map (fun v -> if v.Id = itemToUpdate.Id then itemToUpdate else v)
        

    let getRunnableStation stations =
        stations |> List.where(fun it -> 
            match it.State with
            | Ready(_) -> true
            | _ -> false
        )

    let getInstructionsToCommit (stations: ReservationStations) =
        stations |> List.where(fun st -> 
            match st.State with
            | Running state when state.Qj = 0 && state.Qk = 0 && st.Rt <> 0 -> true
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
        | "00000001" -> { Op = "add"; Qj = source1; Qk = source2; Qt = target; Imm = ""; Type = Integer; }
        | "00010001" -> { Op ="addi"; Qj = source1; Qk = source2; Qt = target; Imm = imm; Type = Integer; }
        | "00000010" -> { Op = "sub"; Qj = source1; Qk = source2; Qt = target; Imm = ""; Type = Integer; }
        | "00010010" -> { Op = "subi"; Qj = source1; Qk = source2; Qt = target; Imm = imm; Type = Integer; }
        | "00000011" -> { Op = "mul"; Qj = source1; Qk = source2; Qt = target; Imm = ""; Type = Integer; }
        | "00010011" -> { Op = "muli"; Qj = source1; Qk = source2; Qt = target; Imm = imm; Type = Integer; }
        | "00000100" -> { Op = "div"; Qj = source1; Qk = source2; Qt = target; Imm = ""; Type = Integer; }
        | "00010100" -> { Op = "divi"; Qj = source1; Qk = source2; Qt = target; Imm = imm; Type = Integer; }
        | _ -> { Op = ""; Qj = 0; Qk = 0; Qt = 0; Imm = ""; Type = None; }