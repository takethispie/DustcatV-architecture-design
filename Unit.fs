namespace DustcatV

open System;

module ExecutionUnitModule =

    let IsEmpty unit =
        match unit.State with
        | Empty(_) -> true
        | _ -> false

    let BookReservationStation (station: ReservationStationUnit, inst: int, Qj: int, Qk: int): ReservationStationUnit =
        let newState = 
            match station.State with
            | Empty id -> Busy(id, inst, Qj, Qk)
            | _ -> raise(Exception("reservation station in unknown / wrong state"))
        { Id = station.Id; State = newState; Qj = Qj; Qk = Qk; Vk = 0; Vj = 0 }
        

    let updateElement (id: int, items: ReservationStationUnit list, itemToUpdate: ReservationStationUnit) = 
        items |> List.map (fun v -> if v.Id = id then itemToUpdate else v)

    let ExecutionUnit (instruction: int, Qj: int, Qk: int, cdbIn: CommonDataBusMessage, exUnit: ExecutionUnit ): ExecutionUnit * CommonDataBusMessage =
        let empty = exUnit.ReservationStations |> List.where(IsEmpty) 
        let updated = if instruction > 0 then BookReservationStation(empty.Head, instruction, Qj, Qk) else empty.Head
        let updatedExUnit: ExecutionUnit = { ReservationStations = updateElement(updated.Id, exUnit.ReservationStations, updated) } 
        ( updatedExUnit, cdbIn)