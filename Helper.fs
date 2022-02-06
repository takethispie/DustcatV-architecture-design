namespace DustcatV

open System

module Helper =

    let updateElement st key n =
        st
        |> List.map (fun (k, v) -> if k = key then k, n else k, v)
