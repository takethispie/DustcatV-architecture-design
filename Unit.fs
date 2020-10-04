namespace DustcatV

open System;

module FunctionalUnit =
    let FunctionalUnit (op: string, Vj: string, Vk: string): string = 
        ""

module ExecutionUnitModule =

    let IsEmpty unit =
        match unit.State with
        | Empty(_) -> true
        | _ -> false

    let BookReservationStation (station: ReservationStationUnit, inst: string, Qj: int, Qk: int) =
        let newState = 
            match station.State with
            | Empty state -> Waiting state
            | _ -> raise(Exception("reservation station state in unknown / wrong state"))
        { Id = station.Id; State = newState; Qj = Qj; Qk = Qk; Vk = "0"; Vj = "0"; Op = inst; Result = "" }

    let ResolveSources (state: ReservationState, message: CommonDataBusMessage)  =
        let (qj, vj) = if state.Qj = message.Source then (0, message.Value) else (state.Qj, "0")
        let (qk, vk) = if state.Qk = message.Source then (0, message.Value) else (state.Qk, "0")
        {Id = state.Id; Op = state.Op; Qj = qj; Qk = qk; Vj = vj; Vk = vk }

    let ProcessCDBMessage (cdbM: CommonDataBusMessage, stations: ReservationStationUnit list) =
        if cdbM.Source = 0 
        then stations 
        else stations |> List.map(fun item -> 
            let (newState, values) = 
                match item.State with
                | Empty state -> (Empty, state)
                | Waiting state when state.Qj <> 0 && state.Qk <> 0 -> (Waiting, ResolveSources(state, cdbM))
                | Waiting state when  state.Qj = 0 && state.Qk = 0 -> (Ready, state)
                | Ready state -> (Ready, state)
                | Running state -> (Running, state)
                | Done state -> (Done, state) 
                | _ -> raise(Exception("unknown state"))
            { Id = values.Id; Op = values.Op; Qj = values.Qj; Qk = values.Qk; Vj = values.Vj; Vk = values.Vk; State = newState(values); Result = "" }
        )


    let updateElement (itemToUpdate: ReservationStationUnit, items: ReservationStationUnit list) = 
        items |> List.map (fun v -> if v.Id = itemToUpdate.Id then itemToUpdate else v)

    let RunReadyStation (cdbM: CommonDataBusMessage, stations: ReservationStationUnit list) =
        let runnableStation = stations |> List.where(fun it -> 
            match it.State with
            | Ready(_) -> true
            | _ -> false
        )
        let st = runnableStation.Head
        let result = FunctionalUnit.FunctionalUnit(st.Op, st.Vj, st.Vk)
        let newState = 
            match st.State with
            | Empty state -> Empty state
            | Waiting state-> Waiting state
            | Ready state -> Running state
            | Running state -> Running state
            | Done state -> Done state
        let updatedStation = { Id = st.Id; Op = st.Op; Qj = st.Qj; Qk = st.Qk; Vj = st.Vj; Vk = st.Vk; State = newState; Result = result}
        updateElement(updatedStation, stations)
        
    let CleanupAndBuildCDBMessage (stations: ReservationStationUnit list) =
        let mutable mess = { Source = 0; Value = "0";};
        let updatedStations = stations |> List.map(fun item -> 
            let newState = 
                match item.State with
                | Empty state -> Empty state
                | Waiting state-> Waiting state
                | Ready state -> Ready state
                | Running state -> 
                    if item.Result <> "" then mess <- { Source = item.Id; Value = item.Result }
                    Done state
                | Done state -> Empty state
            let result = { Id = item.Id; Op = item.Op; Qj = item.Qj; Qk = item.Qk; Vj = item.Vj; Vk = item.Vk; State = newState; Result = item.Result}
            result
        )
        (updatedStations, mess)

    let ExecutionUnit (instruction: string, Qj: int, Qk: int, cdbIn: CommonDataBusMessage, exUnit: ExecutionUnit ): ExecutionUnit * CommonDataBusMessage =
        let empty = exUnit.ReservationStations |> List.where(IsEmpty) 
        let updatedStations = 
            if instruction <> "" && not(List.isEmpty(empty))
            then updateElement(BookReservationStation(empty.Head, instruction, Qj, Qk), exUnit.ReservationStations) 
            else exUnit.ReservationStations
        let processedStations = ProcessCDBMessage(cdbIn, updatedStations)
        let (resultStations, mess) = CleanupAndBuildCDBMessage(processedStations);
        let free = not (List.isEmpty (resultStations |> List.where(IsEmpty)))
        let updatedExUnit = { ReservationStations = resultStations; HasFreeStation = free }
        (updatedExUnit, mess)