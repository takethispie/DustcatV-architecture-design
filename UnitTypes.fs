namespace DustcatV

[<AutoOpen>]
module UnitTypes =

    type CommonDataBusMessage = { Source: int; Value: string; } 
    
    type CommonDataBus = { Int: CommonDataBusMessage; LoadStore: CommonDataBusMessage }

    type ReservationState = { Op: string; Qj: int; Qk: int; Vj: string; Vk: string}

    type LoadInstruction = { TargetRegister: int; Address: string; Value: string;}
    type StoreInstruction = { Address: string; Value: string;}

    type Register = { Value: string; Dirty: bool}

    type ReservationStationUnitState =
        | Empty of ReservationState
        | Waiting of ReservationState
        | Ready of ReservationState
        | Running of ReservationState

    type ReservationStationUnit = { Id: int; State: ReservationStationUnitState; Result: string; Rt: int; }

    type ReservationStations = ReservationStationUnit list

    type InstructionType =
    | Integer 
    | LoadStore
    | None

    type Instruction = { Op: string; Qj: int; Qk: int; Qt: int; Imm: string; Type: InstructionType}