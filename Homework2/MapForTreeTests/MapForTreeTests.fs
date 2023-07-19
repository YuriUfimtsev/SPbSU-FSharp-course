module MapForTreeTests

open NUnit.Framework
open FsUnit
open MapForTree.MapForTree

let testNumberFunc = fun element -> element * element
let testNumberTree () = Tree (5, Tree(4, Tip 6, Tip 8), Tip 9)
let testSquaredNumberTree () = Tree (25, Tree(16, Tip 36, Tip 64), Tip 81)

let testSeqFunc = fun sequence -> Seq.length sequence
let testSeqTree () = Tree (seq {'a'; 'b'; 'c'; 'd'}, Tree (seq {'c'; 'd'}, Tip (seq {'a'}), Tip (seq {'b'})), Tip (seq {'c'}))
let testSeqLengthTree () = Tree (4, Tree (2, Tip 1, Tip 1), Tip 1)

[<Test>]
let ``Square tree and compare test`` () =
    mapForTree (testNumberTree ()) testNumberFunc |> should equal (testSquaredNumberTree ())

[<Test>]
let ``Calculate tree sequences length and compare test`` () =
    mapForTree (testSeqTree ()) testSeqFunc |> should equal (testSeqLengthTree ())
