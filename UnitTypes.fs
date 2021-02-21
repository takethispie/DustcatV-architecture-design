namespace DustcatV

[<AutoOpen>]
module UnitTypes =

    type CommonDataBusMessage = { Source: int; Value: string; } 
    
    type CommonDataBus = { Int: CommonDataBusMessage; LoadStore: CommonDataBusMessage }

    type Register = { Value: string; Dirty: bool}

    type MathOperation = 
    | Add
    | Substract
    | Divide
    | Multiply

    type Operation = 
    | Arithmetic of MathOperation
    | ImmediateArithmetic of MathOperation
    | Set
    | Load
    | Store 
    | Read
    | Write
    | NopeOp

    type Instruction =
    | Integer of Op: Operation * Dest: int * Left: int * Right: int
    | ImmediateInteger of  Op: Operation * Dest: int * Left: int * Immediate: string
    | Set of Op: Operation * Dest: int * Immediate: string
    | Load of Op: Operation * Dest: int * Source: int * Offset: int
    | Store of Op: Operation * Dest: int * Source: int * Offset: int
    | Nope of Op: Operation

    type ReservationStationUnit =
        | Empty of Id: int
        | Waiting of Id: int * Op: Operation * Qj: int * Qk: int * Vj: string * Vk: string * dest: int 
        | Ready of Id: int * Op: Operation * Vj: string * Vk: string * Rt: int
        | Running of Id: int * Op: Operation * Vj: string * Vk: string * Rt: int
        | Done of Id: int * Op: Operation * Vj: string * Vk: string * Rt: int * result: string

    type ReservationStations = ReservationStationUnit list
