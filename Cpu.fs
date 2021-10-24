namespace DustcatV
open System
open DecodeStageModule
open DispatchStageModule
open ExecutionStageModule
open System.Collections.Generic

module Cpu =

    let Cpu(instructions: string list) =
        let mutable ram = new Dictionary<int, string>()
        let mutable stations: ReservationStations = [for i in 1 .. 4 -> Empty(i)]
        let mutable registers: Register list = [for i in 1 .. 31 -> { Id = i; Value = ""; Dirty = false }]

        let dispatch (inst: Instruction) =
            let updatedStations, success = BookReservationStation(stations, inst)
            match success with 
            | true -> stations <- updatedStations
            | false -> ()
            success

        let execute (dataBus: CommonDataBusMessage) =
            stations <- UpdateStations(dataBus, stations)
            match getReadyStations(stations) with 
            | head::_ -> 
                stations <- setRunning(head, stations, ReplaceStation)
                match head with
                | Running(id, op, vj, vk, dest) -> 
                    match op with
                    | NopeOp -> dataBus 
                    | _ -> runFunctionalUnit(head, RunIntegerUnit, RunLoadStoreUnit)
                | _ -> raise(Exception("station is not in running state"))
            | [] -> dataBus

        let cpuHalted() = FreeStations(stations).Length = stations.Length 

        let rec executionLoop (inst: Instruction list, bus: CommonDataBusMessage) = 
            match inst with
            | [] -> 
                match cpuHalted() with
                | true -> [], bus 
                | false -> 
                    match execute(bus) with
                    | newBus -> executionLoop([], newBus)
            | head::tail -> 
                let remaining =
                    match dispatch(head) with
                    | true -> tail
                    | false -> head::tail
                let newbus = execute(bus)
                executionLoop(remaining, newbus)
                
        let decodedInsts = List.map InstructionLowDecode instructions
        executionLoop(decodedInsts, { Source = 0; Value = ""; } ) |> ignore
        ram