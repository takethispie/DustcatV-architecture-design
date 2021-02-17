namespace DustcatV

[<AutoOpen>]
module UnitTypes =

    type CommonDataBusMessage = { Source: int; Value: string; } 
    
    type CommonDataBus = { Int: CommonDataBusMessage; LoadStore: CommonDataBusMessage }

    type Register = { Value: string; Dirty: bool}

    type StationState =
        | Empty
        | Waiting
        | Ready
        | Running

    type ReservationStationUnit = { Id: int; Op: string; Qj: int; Qk: int; Vj: string; Vk: string; Result: string; Rt: int; State: StationState }
    type ReservationStations = ReservationStationUnit list

    type MathOperation = 
    | Add
    | Substract
    | Divide
    | Multiply

    type Metadata = { Source1Valid: int; Source2Valid: int; Destination: int }

    type InstructionType =
    | Integer of Metadata * Left: int * MathOperation * Right: int
    | Load of Metadata * Destination: int * Offset: int * Source: int 
    | Store of Metadata * Destination: int * Offset: int * source: int 
    | Read of Metadata * Source: int * Offset: int * Dest: int
    | Write
    | None

    type Instruction = { Op: string; Qj: int; Qk: int; Qt: int; Imm: string; Type: InstructionType}