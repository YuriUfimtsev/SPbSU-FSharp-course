namespace LocalNetwork

open System
open System.IO
open Computer
open Microsoft.FSharp.Collections
open FSharp.Core

module LocalNetwork =
    type Network
        (
            supportedOperatingSystems : Map<string, float>,
            computers : Computer list,
            adjacencyMatrix : int[][],
            infectedComputerAddress : int Option
        ) =
        let localComputers : Computer list = computers

        let supportedOperatingSystems : Map<string, float> = supportedOperatingSystems

        let mutable isStateFinal = false

        do
            match infectedComputerAddress with
            | Some number ->
                let infectedComputer =
                    List.find (fun (computer : Computer) -> computer.MACAddress = number) localComputers

                infectedComputer.IsInfected <- true
            | None -> ()

            let addConnectedComputers (computers : Computer list) (adjacencyMatrix : int[][]) =
                let computersArray = List.toArray computers

                for i = 0 to computersArray.Length - 1 do
                    for j = 0 to computersArray.Length - 1 do
                        if adjacencyMatrix[i][j] = 1 then
                            computersArray[i].AddConnectedComputer computersArray[j]

            addConnectedComputers localComputers adjacencyMatrix

        member val GetOperatingSystems = supportedOperatingSystems

        /// returns infected computers.
        member _.InfectedComputers () =
            List.filter (fun (computer : Computer) -> computer.IsInfected) localComputers

        member _.IsStateFinal = isStateFinal

        /// returns true if the number of infected computers has reached the maximum possible. Otherwise false
        member this.SpreadVirus () =
            if isStateFinal then
                isStateFinal
            else if List.length (this.InfectedComputers()) = List.length localComputers then
                isStateFinal <- true
                isStateFinal
            else
                let mutable result = true

                List.iter
                    (fun (computer : Computer) ->
                        if not (computer.SpreadVirus()) then
                            result <- false)
                    (this.InfectedComputers())

                if result then
                    result <-
                        List.forall
                            (fun (computer : Computer) -> computer.IsAllConnectedComputersInfected())
                            (this.InfectedComputers())

                result
