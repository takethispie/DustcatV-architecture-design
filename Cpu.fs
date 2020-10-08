namespace DustcatV
open ExecutionStageUnitsModule
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
        let rec iter (inst, message) = 
            match inst with
            | [] -> ()
            | head::tail -> 
                let next = 
                    let freeStations = FreeStations(stations)
                    if freeStations.IsEmpty
                    then 
                        head::tail
                    else 
                        let decoded = InstructionDecode(head)
                        let bookedStation = 
                            if decoded.Imm = "" 
                            then BookReservationStation(freeStations.Head, decoded.Op, decoded.Qj, decoded.Qk, "", "")
                            else BookReservationStation(freeStations.Head, decoded.Op, decoded.Qj, 0, "", decoded.Imm)
                        stations <- updateElement(bookedStation, stations)
                        stations <- UpdateStations(message, stations)
                        tail
                iter next 
        iter instructions
        0