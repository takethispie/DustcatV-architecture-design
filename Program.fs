// Learn more about F# at http://fsharp.org
open System
open DustcatV
open ExecutionStageModule
open Cpu

[<EntryPoint>]
let main argv =
    let inst = [
        "11100004"
    ]
    let _res = Cpu(inst)
    0 // return an integer exit code
