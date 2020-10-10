namespace DustcatV
open System
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
                let next, newMessage = 
                    let freeStations = FreeStations(stations)
                    if freeStations.IsEmpty
                    then 
                        stations <- UpdateStations(message, stations)
                    else 
                        match InstructionDecode(head) with
                        | decoded -> 
                            match decoded.Type with
                            | Integer -> 
                                let newStation = 
                                    BookReservationStation(freeStations.Head, decoded.Op, decoded.Qj, (if decoded.Imm = "" then decoded.Qk else 0), "", (if decoded.Imm = "" then "" else decoded.Imm))
                                stations <- updateElement(newStation, stations)
                            | LoadStore -> 
                                match FreeStations(loadStoreStations) with
                                | [] -> 
                                    loadStoreStations <- UpdateStations(message, loadStoreStations)
                                | freeLsHead::freeLsTail ->
                                    let newLsStation = 
                                        BookReservationStation(freeLsHead, decoded.Op, decoded.Qj, (if decoded.Imm = "" then decoded.Qk else 0), "", (if decoded.Imm = "" then "" else decoded.Imm))
                                    loadStoreStations <- updateElement(newLsStation, loadStoreStations)
                            | None -> raise(Exception("unknown instruction type"))
                        stations <- UpdateStations(message, stations)
                    match getRunnableStation(stations) with
                    | [] -> tail, { Source = 0; Value = ""}
                    | firstReady::others -> 
                        let runStation, message = IntegerExecutionUnit(firstReady)
                        stations <- updateElement(runStation, stations)
                        (tail, message)
                iter (next, newMessage)
        iter (instructions, { Source = 0; Value = ""})
        0