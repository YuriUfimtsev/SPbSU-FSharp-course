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

        /// returns true if all connected computers are infected (except those that have a probability of infection = 0).
        /// Otherwise false
        member this.SpreadVirus () =
            if List.length connectedComputers = 0 then
                true
            else
                let mutable result = true

                List.iter
                    (fun (computer : Computer) ->
                        (computer.ResistVirus()
                         if not (computer.IsInfected) && not (computer.InfectionProbability.Equals(0)) then
                             result <- false))
                    connectedComputers

                result
