namespace DustcatV
open System
open ExecutionStageModule
open DecodeStageUnitsModule
open System.Collections.Generic
open FunctionalUnit

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

        let mutable loadQueue: Object list = [] 

        let mutable storeQueue: Object list = []
        

        let exStage (inst: Instruction, message: CommonDataBusMessage) =
            let freeStations = FreeStations(stations)
            let instProcessed =
                if freeStations.IsEmpty
                then 
                    stations <- UpdateStations(message, stations)
                    loadStoreStations <- UpdateStations(message, loadStoreStations)
                    false
                else 
                    match inst with
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
                            | freeLsHead::_ ->
                                let newLsStation = 
                                    BookReservationStation(freeLsHead, decoded.Op, decoded.Qj, (if decoded.Imm = "" then decoded.Qk else 0), "", (if decoded.Imm = "" then "" else decoded.Imm))
                                loadStoreStations <- updateElement(newLsStation, loadStoreStations)
                        | None -> raise(Exception("unknown instruction type"))
                    stations <- UpdateStations(message, stations)
                    loadStoreStations <- UpdateStations(message, loadStoreStations)
                    true
            match getRunnableStation(stations) with
            | [] -> instProcessed, { Source = 0; Value = ""}
            | firstReady::_ -> 
                let runStation, message = IntegerFunctionalUnit(firstReady)
                stations <- updateElement(runStation, stations)
                instProcessed, message


        let rec executionUnitLoop (inst: Instruction list, message) = 
            match inst with
            | [] -> ()
            | head::tail -> 
                let remaining, nextMessage =
                    match exStage(head, message) with
                    | (true, m) -> tail, m
                    | (false, m) -> head::tail, m
                executionUnitLoop (remaining, nextMessage)


        let decodedInstructions = (instructions |> List.map(InstructionDecode))
        executionUnitLoop (decodedInstructions, { Source = 0; Value = ""})
        ram