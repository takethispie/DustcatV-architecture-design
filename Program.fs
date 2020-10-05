// Learn more about F# at http://fsharp.org
open System
open DustcatV
open ExecutionStageUnitsModule

[<EntryPoint>]
let main argv =
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
    let (exunit, message) = IntegerExecutionUnit("mul", 1, 1, { Source = 1; Value = "5"}, IEU)
    0 // return an integer exit code
