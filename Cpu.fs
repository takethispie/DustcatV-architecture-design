namespace DustcatV
open ExecutionStageUnitsModule
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
        0