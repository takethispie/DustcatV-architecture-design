namespace DustcatV
open System
open ExecutionStageModule
open DecodeStageUnitsModule
open System.Collections.Generic
open FunctionalUnit

module Cpu =

    let Cpu(instructions: string list) =
        let mutable ram = new Dictionary<int, string>()

        let mutable stations: ReservationStations = 
            [for i in 1 .. 4 -> { Id = i; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""; Rt = 0}]

        let mutable loadStoreStations: ReservationStations = 
            [for i in 1 .. 4 -> { Id = i*10; State = Empty { Op = ""; Qj = 0; Qk = 0; Vj = ""; Vk = ""; }; Result = ""; Rt = 0}]

        let mutable registers: Register list = [for i in 1 .. 31 -> { Value = ""; Dirty = false }]

        let mutable loadBuffer: LoadInstruction list = [] 

        let mutable storeBuffer: StoreInstruction list = []
        

        let exStage (inst: Instruction, bus: CommonDataBus) =
            let newMessage =
                match getRunnableStation(loadStoreStations) with
                | [] -> { Source = 0; Value = ""}
                | firstReady::_ ->
                    let runStation, newMessage = LoadStoreFunctionnalUnit(firstReady)
                    loadStoreStations <- updateElement(runStation, stations)
                    newMessage
            let newBus =
                match getRunnableStation(stations) with
                | [] -> { Int = newMessage; LoadStore = { Source = 0; Value = "" }}
                | firstReady::_ -> 
                    let runStation, newLoadStoreMessage = IntegerFunctionalUnit(firstReady)
                    stations <- updateElement(runStation, stations)
                    { Int = newMessage; LoadStore = newLoadStoreMessage }

            let freeStations = FreeStations(stations)
            let instProcessed =
                if freeStations.IsEmpty
                then 
                    stations <- UpdateStations(bus.Int, stations)
                    loadStoreStations <- UpdateStations(bus.Int, loadStoreStations)
                    false
                else 
                    match inst with
                    | decoded -> 
                        match decoded.Type with
                        | Integer -> 
                            let newStation = 
                                BookReservationStation(freeStations.Head, decoded )
                            stations <- updateElement(newStation, stations)
                        | LoadStore -> 
                            match FreeStations(loadStoreStations) with
                            | [] -> 
                                loadStoreStations <- UpdateStations(bus.LoadStore, loadStoreStations)
                            | freeLsHead::_ ->
                                let newLsStation = 
                                    BookReservationStation(freeLsHead, decoded)
                                loadStoreStations <- updateElement(newLsStation, loadStoreStations)
                        | None when decoded.Op = "none" -> 
                            stations <- UpdateStations(bus.Int, stations)
                            loadStoreStations <- UpdateStations(bus.Int, loadStoreStations)
                        | None -> raise(Exception("unknown instruction type"))
                    stations <- UpdateStations(bus.Int, stations)
                    loadStoreStations <- UpdateStations(bus.LoadStore, loadStoreStations)
                    true
            (instProcessed, newBus)
                    
            
        let cpuHalted() = 
             FreeStations(stations).Length = stations.Length 
             && FreeStations(loadStoreStations).Length = loadStoreStations.Length   


        let rec executionUnitLoop (inst: Instruction list, bus: CommonDataBus) = 
            
            match inst with
            | [] -> 
                match cpuHalted() with
                | true -> [], bus 
                | false -> 
                     match exStage({ Op = "none"; Qj = 0; Qk = 0; Qt = 0; Imm = ""; Type = None; }, bus) with
                     | (_, m) -> [], m
            | head::tail -> 
                let remaining, newbus =
                    match exStage(head, bus) with
                    | (true, m) -> tail, m
                    | (false, m) -> head::tail, m
                executionUnitLoop (remaining, newbus)


        let decodedInstructions = (instructions |> List.map(InstructionDecode))
        executionUnitLoop (decodedInstructions, { Int = { Source = 0; Value = ""}; LoadStore = { Source = 0; Value = ""}})
        ram