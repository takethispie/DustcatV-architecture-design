namespace DustcatV
open ExecutionStageModule
open DecodeStageUnitsModule
open System.Collections.Generic

module Cpu =

    let Cpu(instructions: string list) =
        let mutable ram = new Dictionary<int, string>()

        let mutable stations: ReservationStations = [
            { Id = 1; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
            { Id = 2; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
            { Id = 3; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
            { Id = 4; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
        ]

        let mutable loadStoreStations: ReservationStations = [
            { Id = 10; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
            { Id = 20; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
            { Id = 30; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
            { Id = 40; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
        ]

        let rec iter (inst, message) = 
            match inst with
            | [] -> ()
            | head::tail -> 
                let next, message = 
                    let freeStations = FreeStations(stations)
                    if freeStations.IsEmpty
                    then 
                        stations <- UpdateStations(message, stations)
                        match getRunnableStation(stations) with
                        | [] -> tail, { Source = 0; Value = ""}
                        | firstReady::others -> 
                            let runStation, message = IntegerExecutionUnit(firstReady)
                            stations <- updateElement(runStation, stations)
                            (head::tail, message)
                    else 
                        match InstructionDecode(head) with
                        | decoded when decoded.Imm = "" -> 
                            let newStation =
                                match decoded.Type with
                                | Integer -> BookReservationStation(freeStations.Head, decoded.Op, decoded.Qj, decoded.Qk, "", "")
                            stations <- updateElement(newStation, stations)
                        | decoded when decoded.Imm <> "" -> 
                            let newStation = BookReservationStation(freeStations.Head, decoded.Op, decoded.Qj, 0, "", decoded.Imm)
                            stations <- updateElement(newStation, stations)
                        stations <- UpdateStations(message, stations)
                        match getRunnableStation(stations) with
                        | [] -> tail, { Source = 0; Value = ""}
                        | firstReady::others -> 
                            let runStation, message = IntegerExecutionUnit(firstReady)
                            stations <- updateElement(runStation, stations)
                            (tail, message)
                iter (next, message)
        iter (instructions, { Source = 0; Value = ""})
        0