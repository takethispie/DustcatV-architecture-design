namespace DustcatV
open System
open ExecutionStageModule
open DecodeStageUnitsModule
open System.Collections.Generic
open FunctionalUnit

module Cpu =

    let Cpu(instructions: string list) =
        let mutable ram = new Dictionary<int, string>()
        let mutable intStations: ReservationStations = [for i in 1 .. 4 -> Empty(i)]
        let mutable loadStoreStations: ReservationStations = [for i in 1 .. 4 -> Empty(i*10)]
        let mutable registers: Register list = [for i in 1 .. 31 -> { Value = ""; Dirty = false }]
        let mutable InstructionQueue: Instruction List = new List<Instruction>()

        let execute (inst: Instruction, dataBus: CommonDataBus) =
            intStations <- UpdateStations(dataBus.Int, intStations)
            loadStoreStations <- UpdateStations(dataBus.LoadStore, loadStoreStations)
            ignore

        ram