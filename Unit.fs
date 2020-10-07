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

    let IsEmpty unit =
        match unit.State with
        | Empty(_) -> true
        | _ -> false


    let BookReservationStation (station: ReservationStationUnit, inst: string, Qj: int, Qk: int) =
        let newState = 
            match station.State with
            | Empty _ -> Waiting { Op = inst; Qj = Qj; Qk = Qk; Vj = ""; Vk = ""}
            | _ -> raise(Exception("reservation station state in unknown / wrong state"))
        { Id = station.Id; State = newState;  Result = "" }


    let ResolveSources (state: ReservationState, message: CommonDataBusMessage)  =
        let (qj, vj) = if state.Qj = message.Source then (0, message.Value) else (state.Qj, "0")
        let (qk, vk) = if state.Qk = message.Source then (0, message.Value) else (state.Qk, "0")
        { Op = state.Op; Qj = qj; Qk = qk; Vj = vj; Vk = vk }


    let ProcessCDBMessage (cdbM: CommonDataBusMessage, stations: ReservationStationUnit list) =
        if cdbM.Source = 0 
        then stations 
        else stations |> List.map(fun item -> 
            let (newState, values) = 
                match item.State with
                | Empty state -> (Empty, state)
                | Waiting state when state.Qj <> 0 && state.Qk <> 0 -> (Waiting, ResolveSources(state, cdbM))
                | Ready state -> (Ready, state)
                | Running state -> (Running, state)
                | _ -> raise(Exception("unknown state"))
            { Id = item.Id; State = newState(values); Result = "" }
        )

    let UpdateStationState (stations: ReservationStationUnit list) =
        let updatedStations = stations |> List.map(fun item -> 
            let newState = 
                match item.State with
                | Empty state -> Empty state
                | Waiting state when  state.Qj = 0 && state.Qk = 0 -> Ready state
                | Waiting state-> Waiting state
                | Ready state -> Ready state
                | Running state -> Running state
            { Id = item.Id; State = newState; Result = item.Result}
        )
        updatedStations



    let updateElement (itemToUpdate: ReservationStationUnit, items: ReservationStationUnit list) = 
        items |> List.map (fun v -> if v.Id = itemToUpdate.Id then itemToUpdate else v)


    let RunReadyStationOnIntegerUnit (stations: ReservationStationUnit list) =
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
                result <- FunctionalUnit.FunctionalUnit(state.Op, state.Vj, state.Vk)
                Running state
            | Running _ -> raise(Exception("already running station"))
        let updatedStation = { Id = st.Id; State = newState; Result = result}
        updateElement(updatedStation, stations)

        
    let CleanupAndBuildCDBMessage (stations: ReservationStationUnit list) =
        let mutable mess = { Source = 0; Value = "0";};
        let updatedStations = stations |> List.map(fun item -> 
            let newState = 
                match item.State with
                | Empty state -> Empty state
                | Waiting state-> Waiting state
                | Ready state -> Ready state
                | Running _ -> 
                    if item.Result <> "" then mess <- { Source = item.Id; Value = item.Result }
                    Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }
            let result = { Id = item.Id; State = newState; Result = item.Result}
            result
        )
        (updatedStations, mess)


    let IntegerExecutionUnit (instruction: string, Qj: int, Qk: int, cdbIn: CommonDataBusMessage, exUnit: ExecutionUnit ): ExecutionUnit * CommonDataBusMessage =
        let empty = exUnit.ReservationStations |> List.where(IsEmpty) 
        let updatedStations = 
            if instruction <> "" && not(List.isEmpty(empty))
            then updateElement(BookReservationStation(empty.Head, instruction, Qj, Qk), exUnit.ReservationStations) 
            else exUnit.ReservationStations
        let processedStations = ProcessCDBMessage(cdbIn, updatedStations)
        let uptoDateStations = UpdateStationState(processedStations)
        let afterRunStations = RunReadyStationOnIntegerUnit(uptoDateStations)
        let (resultStations, mess) = CleanupAndBuildCDBMessage(afterRunStations);
        let free = not (List.isEmpty (resultStations |> List.where(IsEmpty)))
        let updatedExUnit = { ReservationStations = resultStations; HasFreeStation = free }
        (updatedExUnit, mess)


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


    let LoadStoreUnit (instruction: string, target: int, address: int, imm: string, cdbIn: CommonDataBusMessage, exUnit: ExecutionUnit ) =
        let empty = exUnit.ReservationStations |> List.where(IsEmpty)
        let updatedStations = 
            if instruction <> "" && not(List.isEmpty(empty))
            then 
                let st = empty.Head
                if target <> 0 
                then updateElement({ Id = st.Id; State = Waiting { Op = instruction;  Qj = address; Qk = 0; Vj = ""; Vk = imm; }; Result = "" }, exUnit.ReservationStations) 
                else updateElement({ Id = st.Id; State = Waiting { Op = instruction;  Qj = address; Qk = 0; Vj = ""; Vk = target.ToString(); }; Result = "" }, exUnit.ReservationStations) 
            else exUnit.ReservationStations
        let processedStations = ProcessCDBMessage(cdbIn, updatedStations)
        let uptoDateStations = UpdateStationState(processedStations)
        let afterRunStations = RunReadyStationOnLoadStoreunit(uptoDateStations)
        let (resultStations, mess) = CleanupAndBuildCDBMessage(afterRunStations);
        let free = not (List.isEmpty (resultStations |> List.where(IsEmpty)))
        let updatedExUnit = { ReservationStations = resultStations; HasFreeStation = free }
        (updatedExUnit, mess)
        0
    

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