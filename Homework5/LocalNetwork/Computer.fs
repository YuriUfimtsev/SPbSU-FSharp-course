namespace LocalNetwork

open System

module Computer =
    type Computer(macAddress : int, operatingSystem : string, infectionProbability : float) =

        let random = Random()
        let mutable connectedComputers : Computer list = []

        member val MACAddress = macAddress with get
        member val OperatingSystem = operatingSystem with get
        member val InfectionProbability = infectionProbability with get
        member this.ConnectedComputers = connectedComputers
        member val IsInfected = false with get, set

        member this.AddConnectedComputer (computer : Computer) =
            connectedComputers <- computer :: connectedComputers

        member this.ResistVirus () =
            if not this.IsInfected then
                let number = random.Next(0, 100)

                let isInfected =
                    this.InfectionProbability |> (*) 100.0 |> (-) (float number) |> (>) 0

                if isInfected then
                    this.IsInfected <- true

        /// returns true if all connected computers are infected (except those that have a probability of infection = 0 or unreachable).
        /// Otherwise false
        member _.SpreadVirus () =
            if List.length connectedComputers = 0 then
                true
            else
                List.iter
                    (fun (computer : Computer) ->
                        (if not computer.IsInfected && not (computer.InfectionProbability.Equals(0)) then
                             computer.ResistVirus()))
                    connectedComputers

                List.forall
                    (fun (computer : Computer) -> (computer.IsInfected || computer.InfectionProbability.Equals(0)))
                    connectedComputers

        member this.IsAllConnectedComputersInfected () =
            List.forall
                (fun (connectedComputer : Computer) ->
                    connectedComputer.IsInfected || connectedComputer.InfectionProbability.Equals(0))
                this.ConnectedComputers
