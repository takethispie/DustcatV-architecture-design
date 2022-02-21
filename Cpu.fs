namespace DustcatV

open System
open DecodeStageModule
open DispatchStageModule
open ExecutionStageModule
open System.Collections.Generic

module Cpu =

    let Cpu (instructions: string list) =
        let mutable ram = new Dictionary<int, string>()
        let mutable stations: ReservationStations = [ for i in 1 .. 4 -> Empty(i) ]

        let mutable registers: Register list =
            [ for i in 1 .. 31 -> { Id = i; Value = ""; Dirty = false } ]


        let dispatch (inst: Instruction) =
            let updatedStations, success = BookReservationStation(stations, inst)

            match success with
            | true -> stations <- updatedStations
            | false -> ()

            success


        let execute (dataBus: CommonDataBusMessage) =
            stations <- UpdateStations(dataBus, stations)
            match getReadyStations (stations) with
            | head :: _ ->
                stations <- setRunning (head, stations, ReplaceStation)
                let runningStations = getRunningStations(stations)
                match runningStations.Head with
                | Running (id, op, vj, vk, dest) ->
                    match op with
                    | NopeOp -> dataBus
                    | _ -> runFunctionalUnit (runningStations.Head, RunIntegerUnit, RunLoadStoreUnit)
                | _ -> raise (Exception("station is not in running state"))
            | [] -> dataBus


        let cpuHalted () =
            FreeStations(stations).Length = stations.Length


        let writeRegister (msg: CommonDataBusMessage) =
            match msg.Valid with 
            | false ->
                registers <- registers |> List.map (fun register -> if(register.Id = msg.Source) then { register with Value = msg.Value; Dirty = false } else register )
                { msg with Valid = true }
            | _ -> msg


        let rec executionLoop (inst: Instruction list, bus: CommonDataBusMessage) =
            let res =
                match inst with
                | [] ->
                    match cpuHalted () with
                    | true -> [], bus
                    | false ->
                        match execute (bus) with
                        | newBus -> 
                            let writtenRegister = writeRegister(newBus)
                            stations <- UpdateStations(newBus, stations)
                            ([], writtenRegister)
                | head :: tail ->
                    let remaining =
                        match dispatch (head) with
                        | true -> tail
                        | false -> head :: tail
                    let newbus = execute (bus)
                    (remaining,  writeRegister(newbus))
            if cpuHalted() then ([], bus)
            else executionLoop (res)


        let decodedInsts =
            List.map InstructionLowDecode instructions


        executionLoop (decodedInsts, { Source = 0; Value = ""; Valid = true })
        |> ignore


        ram
