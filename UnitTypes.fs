namespace DustcatV

[<AutoOpen>]
module UnitTypes =

    type CommonDataBusMessage = { Source: int; Value: string }

    type ReservationState = { Op: string; Qj: int; Qk: int; Vj: string; Vk: string}

    type ReservationStationUnitState =
        | Empty of ReservationState
        | Waiting of ReservationState
        | Ready of ReservationState
        | Running of ReservationState
        | Done of ReservationState

    type ReservationStationUnit = { Id: int; State: ReservationStationUnitState; Result: string }

    type ExecutionUnit = { ReservationStations: ReservationStationUnit list; HasFreeStation: bool  }