namespace DustcatV

[<AutoOpen>]
module UnitTypes =

    type CommonDataBusMessage = { Source: int; Value: string; } 
    
    type Register = { Id: int; Value: string; Dirty: bool}
    
    type Operation = 
    | Set
    | Load
    | Store 
    | Read
    | Write
    | NopeOp
    | Add
    | Substract
    | Divide
    | Multiply
    
    type Instruction =
    | Integer of Op: Operation * Dest: int * Left: int * Right: int
    | ImmediateInteger of  Op: Operation * Dest: int * Left: int * Immediate: string
    | SetRegister of Op: Operation * Dest: int * Immediate: string
    | Memory of Op: Operation * Dest: int * Source: int * Offset: int
    | Nope of Op: Operation

    type ReservationStationUnit =
    | Empty of Id: int
    | Waiting of Id: int * Op: Operation * Qj: int * Qk: int * Vj: string * Vk: string * dest: int 
    | Ready of Id: int * Op: Operation * Vj: string * Vk: string * Rt: int
    | Running of Id: int * Op: Operation * Vj: string * Vk: string * Rt: int
    | Done of Id: int * Rt: int * result: string

    type ReservationStations = ReservationStationUnit list