namespace DustcatV
open ExecutionStageUnitsModule
open DecodeStageUnitsModule
open System.Collections.Generic

module Cpu =

    let Cpu(instructions: string list) =
        let ram = new Dictionary<int, string>()
        let IEU: ExecutionUnit = 
            { 
                ReservationStations = [
                    { Id = 1; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
                    { Id = 2; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
                    { Id = 3; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
                    { Id = 4; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""}
                ]; 
                HasFreeStation = true; 
            }
        
        let rec iter (inst, message, exUnit) = 
            match inst with
            | [] -> ()
            | head::tail -> 
                let res =
                    match IEU.HasFreeStation with
                    | true -> 
                        let decoded = InstructionDecode(head);
                        let source = if decoded.Imm <> "" then IEU.ReservationStations.Length else 0
                        let value = if decoded.Imm <> "" then decoded.Imm else ""
                        let cdbMessage = { Source = source;  Value = value; }
                        let (newExUnit, newMessage) = 
                            match decoded.Type with
                            | Integer -> IntegerExecutionUnit(decoded.Op, decoded.Qj, decoded.Qk, cdbMessage, IEU)
                            | _ -> (IEU, { Source = 0; Value = ""})
                        (tail, newMessage, newExUnit)
                    | false -> 
                        let (newExUnit, newMessage) = 
                            IntegerExecutionUnit("", 0, 0, message, IEU)
                        (tail, newMessage, newExUnit)
                iter res
        iter (instructions, { Source = 0;  Value = ""; }, IEU)
        0