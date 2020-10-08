namespace DustcatV

open System;

module FunctionalUnit =
    let FunctionalUnit (op: string, Vj: string, Vk: string): string = 
        match op.ToLower() with
        | "add" -> ((Vj |> int) + (Vk |> int)).ToString()  
        | "sub" -> ((Vj |> int) - (Vk |> int)).ToString()
        | "mul" -> ((Vj |> int) * (Vk |> int)).ToString()
        | "div" when Vk <> "0" -> ((Vj |> int) / (Vk |> int)).ToString()
        | "div" when Vk = "0" -> raise(DivideByZeroException())
        | _ -> ""


module ExecutionStageUnitsModule =

    let HasNoInstruction unit =
        match unit.State with
        | Empty(_) -> true
        | _ -> false

    let FreeStations stations =
        stations |> List.where(HasNoInstruction)


    let BookReservationStation (station: ReservationStationUnit, inst: string, Qj: int, Qk: int, Vj: string, Vk: string) =
        let newState = 
            match station.State with
            | Empty _ -> Waiting { Op = inst; Qj = Qj; Qk = Qk; Vj = Vj; Vk = Vk}
            | _ -> raise(Exception("reservation station state in unknown / wrong state"))
        { Id = station.Id; State = newState;  Result = "" }


    let ResolveSources (state: ReservationState, message: CommonDataBusMessage)  =
        let (qj, vj) = if state.Qj = message.Source then (0, message.Value) else (state.Qj, "0")
        let (qk, vk) = if state.Qk = message.Source then (0, message.Value) else (state.Qk, "0")
        { Op = state.Op; Qj = qj; Qk = qk; Vj = vj; Vk = vk }


    let UpdateStations (cdbM: CommonDataBusMessage, stations: ReservationStations) =
        if cdbM.Source = 0 
        then stations 
        else stations |> List.map(fun station -> 
            let rec updateState (item) = 
                match item.State with
                | Empty state -> Empty state
                | Waiting state when state.Qj <> 0 && state.Qk <> 0 -> 
                    updateState({ Id = station.Id; State = Waiting (ResolveSources(state, cdbM)); Result = "" })
                | Waiting state when state.Qj = 0 && state.Qk = 0 -> Ready state
                | Ready state -> Ready state
                | Running state -> Running state
                | _ -> raise(Exception("unknown state"))
            let newState = updateState(station)
            { Id = station.Id; State = newState; Result = "" }
        )



    let updateElement (itemToUpdate: ReservationStationUnit, items: ReservationStationUnit list) = 
        items |> List.map (fun v -> if v.Id = itemToUpdate.Id then itemToUpdate else v)


    let IntegerExecutionUnit (st: ReservationStationUnit)  =
        let (newState, message) =
            match st.State with
            | Ready state -> 
                let res = 
                    match state.Op.ToLower() with
                    | "add" -> ((state.Vj |> int) + (state.Vk |> int)).ToString()  
                    | "sub" -> ((state.Vj |> int) - (state.Vk |> int)).ToString()
                    | "mul" -> ((state.Vj |> int) * (state.Vk |> int)).ToString()
                    | "div" when state.Vk <> "0" -> ((state.Vj |> int) / (state.Vk |> int)).ToString()
                    | "div" when state.Vk = "0" -> raise(DivideByZeroException())
                    | _ -> ""
                ((Running state), { Source = st.Id; Value = res })
            | _ -> raise(Exception("already running station"))
        ({ Id = st.Id; State = newState; Result = message.Value}, message)
        
    let LoadStoreUnit (st: ReservationStationUnit) =
        match st.State with
        | Ready state -> 
            0
        | _ -> raise(Exception("already running station"))


    let RunReadyStationOnLoadStoreunit (stations: ReservationStationUnit list)= 
        let runnableStation = stations |> List.where(fun it -> 
            match it.State with
            | Ready(_) -> true
            | _ -> false
        )
        let st = runnableStation.Head
        let mutable result = ""
        let newState = 
            match st.State with
            | Empty state -> Empty state
            | Waiting state-> Waiting state
            | Ready state -> 
                // TODO
                Running state
            | Running _ -> raise(Exception("already running station"))
        let updatedStation = { Id = st.Id; State = newState; Result = result}
        updateElement(updatedStation, stations)
    

module DecodeStageUnitsModule =
    let rec intToBinary i =
        match i with
        | 0 | 1 -> string i
        | _ ->
            let bit = string (i % 2)
            (intToBinary (i / 2)) + bit

            
    let InstructionDecode (inst: string) =
        let binary = (inst |> int) |> intToBinary 
        let op = binary.Substring(0, 8)
        let target = int(binary.Substring(8, 4))
        let source1 = int(binary.Substring(12, 4))
        let source2 = int(binary.Substring(16, 4))
        let imm = binary.Substring(16, 16)
        match op.ToLower() with
        | "add" -> { Op = op.ToLower(); Qj = source1; Qk = source2; Qt = target; Imm = ""; Type = Integer; }
        | "addi" -> { Op = op.ToLower(); Qj = source1; Qk = source2; Qt = target; Imm = imm; Type = Integer;  }
        | "sub" -> { Op = op.ToLower(); Qj = source1; Qk = source2; Qt = target; Imm = ""; Type = Integer;  }
        | "subi" -> { Op = op.ToLower(); Qj = source1; Qk = source2; Qt = target; Imm = imm; Type = Integer;  }
        | "mul" -> { Op = op.ToLower(); Qj = source1; Qk = source2; Qt = target; Imm = ""; Type = Integer;  }
        | "muli" -> { Op = op.ToLower(); Qj = source1; Qk = source2; Qt = target; Imm = imm; Type = Integer;  }
        | "div" -> { Op = op.ToLower(); Qj = source1; Qk = source2; Qt = target; Imm = ""; Type = Integer;  }
        | "divi" -> { Op = op.ToLower(); Qj = source1; Qk = source2; Qt = target; Imm = imm; Type = Integer;  }
        | _ -> { Op = ""; Qj = 0; Qk = 0; Qt = 0; Imm = ""; Type = None;  }

module RegisterRenaming =
    let ReorderBufer =
        0

    let mutable InstructionQueue: Instruction list = []