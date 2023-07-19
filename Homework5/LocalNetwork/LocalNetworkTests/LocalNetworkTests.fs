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

    let computers =
        [ Computer(1, "Windows", 1.0)
          Computer(2, "Linux", 1.0)
          Computer(3, "Windows", 1.0)
          Computer(4, "Linux", 1.0)
          Computer(5, "Windows", 1.0)
          Computer(6, "Linux", 1.0) ]

    let adjacencyMatrix =
        [| [| 0; 1; 1; 0; 1; 0 |]
           [| 1; 0; 0; 0; 0; 0 |]
           [| 1; 0; 0; 1; 0; 0 |]
           [| 0; 0; 1; 0; 0; 0 |]
           [| 1; 0; 0; 0; 0; 1 |]
           [| 1; 0; 0; 0; 0; 0 |] |]

    let network = Network(supportedOS, computers, adjacencyMatrix, Some 1)
    network.SpreadVirus() |> should be False

    network.InfectedComputers()
    |> List.map (fun (computer : Computer) -> computer.MACAddress)
    |> should equal [ 1; 2; 3; 5 ]

    network.SpreadVirus() |> should be True

    network.InfectedComputers()
    |> List.map (fun (computer : Computer) -> computer.MACAddress)
    |> should equal [ 1; 2; 3; 4; 5; 6 ]

[<Test>]
let ``Infection with zero probability shouldn't spread`` () =
    let supportedOS = Map [ "Windows", 0.0; "Linux", 0.0 ]

    let computers =
        [ Computer(1, "Windows", 0.0)
          Computer(2, "Linux", 0.0)
          Computer(3, "Windows", 0.0)
          Computer(4, "Linux", 0.0)
          Computer(5, "Windows", 0.0)
          Computer(6, "Linux", 0.0) ]

    let adjacencyMatrix =
        [| [| 0; 1; 1; 0; 1; 0 |]
           [| 1; 0; 0; 0; 0; 0 |]
           [| 1; 0; 0; 1; 0; 0 |]
           [| 0; 0; 1; 0; 0; 0 |]
           [| 1; 0; 0; 0; 0; 1 |]
           [| 1; 0; 0; 0; 0; 0 |] |]

    let network = Network(supportedOS, computers, adjacencyMatrix, Some 1)
    network.SpreadVirus() |> should be True

    network.InfectedComputers()
    |> List.map (fun (computer : Computer) -> computer.MACAddress)
    |> should equal [ 1 ]

[<Test>]
let ``Empty local network test`` () =
    let supportedOS = Map [ "Windows", 1.0; "Linux", 0.0 ]
    let computers = []
    let adjacencyMatrix = [||]
    let network = Network(supportedOS, computers, adjacencyMatrix, None)
    network.SpreadVirus() |> should be True
    network.InfectedComputers() |> should be Empty

[<Test>]
let ``Network with unreachable computers (3, 4) test`` () =
    let supportedOS = Map [ "Windows", 1.0; "Linux", 1.0 ]

    let computers =
        [ Computer(1, "Windows", 1.0)
          Computer(2, "Linux", 1.0)
          Computer(3, "Windows", 1.0)
          Computer(4, "Linux", 1.0)
          Computer(5, "Windows", 1.0)
          Computer(6, "Linux", 1.0) ]

    let adjacencyMatrix =
        [| [| 0; 1; 0; 0; 1; 0 |]
           [| 1; 0; 0; 0; 0; 0 |]
           [| 0; 0; 0; 1; 0; 0 |]
           [| 0; 0; 1; 0; 0; 0 |]
           [| 1; 0; 0; 0; 0; 1 |]
           [| 1; 0; 0; 0; 0; 0 |] |]

    let network = Network(supportedOS, computers, adjacencyMatrix, Some 1)
    network.SpreadVirus() |> should be False

    network.InfectedComputers()
    |> List.map (fun (computer : Computer) -> computer.MACAddress)
    |> should equal [ 1; 2; 5; ]

    network.SpreadVirus() |> should be True

    network.InfectedComputers()
    |> List.map (fun (computer : Computer) -> computer.MACAddress)
    |> should equal [ 1; 2; 5; 6 ]

