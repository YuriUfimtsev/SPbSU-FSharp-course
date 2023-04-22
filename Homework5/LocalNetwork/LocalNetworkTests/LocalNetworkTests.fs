module LocalNetworkTests

open LocalNetwork.LocalNetwork
open NUnit.Framework
open FsUnit
open Foq
open LocalNetwork
open Computer

[<Test>]
let ``Infection with a probability of 1 should work as a bfs`` () =
    let supportedOS = Map [ "Windows", 1.0; "Linux", 1.0 ]
    let pathToFile = "TestNetwork.txt"
    let network = Network(pathToFile, supportedOS)

    let mutable infectionResult = network.SpreadVirus()
    infectionResult |> should be False

    network.State()
    |> List.map (fun (computer : Computer) -> computer.MACAddress)
    |> should equal [ 1; 2; 3; 5 ]

    infectionResult <- network.SpreadVirus()
    infectionResult |> should be False

    network.State()
    |> List.map (fun (computer : Computer) -> computer.MACAddress)
    |> should equal [ 1; 2; 3; 4; 5; 6 ]

    infectionResult <- network.SpreadVirus()
    infectionResult |> should be True

[<Test>]
let ``Infection with zero probability shouldn't spread`` () =
    let supportedOS = Map [ "Windows", 0.0; "Linux", 0.0 ]
    let pathToFile = "TestNetwork.txt"
    let network = Network(pathToFile, supportedOS)
    let mutable infectionResult = network.SpreadVirus()
    infectionResult |> should be True

    network.State()
    |> List.map (fun (computer : Computer) -> computer.MACAddress)
    |> should equal [ 1 ]
