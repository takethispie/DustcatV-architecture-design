namespace DustcatV

[<AutoOpen>]
module UnitTypes =

    type CommonDataBusMessage = { Source: int; Value: int }

    type ReservationStationUnitState =
        | Empty of id: int
        | Busy of id: int * op: int * Qj: int * Qk: int
        | Running of id: int * Vj: int * Vk: int
        | HandlingMessage of id: int * cdb: CommonDataBusMessage
        | Done of id: int * result: int

    type ReservationStationUnit = { Id: int; Qj: int; Qk: int; Vj: int; Vk: int; State: ReservationStationUnitState }

    type ExecutionUnit = { ReservationStations: ReservationStationUnit list;  }