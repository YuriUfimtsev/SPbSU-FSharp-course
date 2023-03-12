namespace ParseTree

module ParseTree =
    type Operator =
    | Addition
    | Multiplication
    | Subtraction
    | Division

    type Tree =
    | Tree of Operator * Tree * Tree
    | Tip of int
    
    let rec calculateExpression tree =
        match tree with
        | Tree (Addition, leftTree, rightTree) -> calculateExpression leftTree + calculateExpression rightTree
        | Tree (Multiplication, leftTree, rightTree) -> calculateExpression leftTree * calculateExpression rightTree
        | Tree (Subtraction, leftTree, rightTree) -> calculateExpression leftTree - calculateExpression rightTree
        | Tree (Division, leftTree, rightTree) ->
            let leftTreeResult = calculateExpression leftTree
            let rightTreeResult = calculateExpression rightTree
            if rightTreeResult <> 0 then calculateExpression leftTree / calculateExpression rightTree
            else raise (nullArg("DivisionByZero"))
        | Tip number -> number