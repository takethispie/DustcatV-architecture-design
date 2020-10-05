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
        for inst in instructions do
            let decoded = InstructionDecode(inst);
            let source = if decoded.Imm <> "" then IEU.ReservationStations.Length else 0
            let value = if decoded.Imm <> "" then decoded.Imm else ""
            let cdbMessage = { Source = source;  Value = value; }
            let mutable (exunit, message) = 
                match decoded.Type with
                | Integer -> IntegerExecutionUnit(decoded.Op, decoded.Qj, decoded.Qk, cdbMessage, IEU)
                | _ -> (IEU, { Source = 0; Value = ""})
            ()
        0