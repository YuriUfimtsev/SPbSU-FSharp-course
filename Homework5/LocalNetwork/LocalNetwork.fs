namespace LocalNetwork

open System
open System.IO
open Computer

module LocalNetwork =
    type Network(pathToFile : string, supportedOperatingSystems : Map<string, float>) =
        let mutable computers : Computer list = []

        let supportedOperatingSystems : Map<string, float> = supportedOperatingSystems

        do
            let initialize (path : string) =
                use streamReader = new StreamReader(path)

                let initializeComputers (source : StreamReader) =
                    let addNewComputer computer computersList = computer :: computersList
                    let mutable newLine = source.ReadLine()

                    while newLine <> String.Empty do
                        let array = newLine.Split ' '

                        if Array.length array <> 2 then
                            raise (
                                invalidArg
                                    "Computer macAddress and operating system info"
                                    "Two parameters weren't found"
                            )

                        let infectionProbability = Map.find array[1] supportedOperatingSystems
                        let computer = Computer((array[0] |> int), array[1], infectionProbability)
                        computers <- addNewComputer computer computers
                        newLine <- source.ReadLine()

                    computers <- List.rev computers
                    newLine <- source.ReadLine()

                initializeComputers streamReader
                let computersCount = List.length computers
                let mutable adjacencyMatrix = Array2D.zeroCreate computersCount computersCount

                let initializeAdjacencyMatrix (source : StreamReader) =
                    let mutable newLine = source.ReadLine()
                    let mutable i = 0

                    while newLine <> String.Empty do
                        let array = newLine.Split ' '

                        if array.Length <> computersCount then
                            raise (invalidArg "Adjacency matrix" "Incorrect adjacency matrix")

                        for j = 0 to computersCount - 1 do
                            Array2D.set adjacencyMatrix (array[j] |> int) i j

                        i <- i + 1
                        newLine <- source.ReadLine()

                initializeAdjacencyMatrix streamReader

                let addConnectedComputers (computers : Computer list) (adjacencyMatrix : int[,]) =
                    let computersArray = List.toArray computers

                    for i = 0 to computersArray.Length - 1 do
                        for j = 0 to computersArray.Length - 1 do
                            if adjacencyMatrix[i, j] = 1 then
                                computersArray[i].AddConnectedComputer computersArray[j]

                addConnectedComputers computers adjacencyMatrix

            initialize pathToFile

        member val GetOperatingSystems = supportedOperatingSystems

        member _.SpreadVirus =
            let infectedComputers =
                List.filter (fun (computer : Computer) -> computer.IsInfected) computers

            List.iter (fun (computer : Computer) -> computer.SpreadVirus()) infectedComputers
