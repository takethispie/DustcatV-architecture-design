open DustcatV
open Cpu

[<EntryPoint>]
let main argv =
    let inst = [
        "11100004"
    ]
    let _res = Cpu(inst)
    0 // return an integer exit code
