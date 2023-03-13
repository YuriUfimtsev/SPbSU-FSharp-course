namespace MapForTree

module MapForTree =

    type Tree<'a> =
    | Tree of 'a * Tree<'a> * Tree<'a>
    | Tip of 'a

    let rec mapForTree tree func =
        match tree with
        | Tree (element, firstTree, secondTree) -> Tree (func element, mapForTree firstTree func, mapForTree secondTree func)
        | Tip element -> Tip (func element)
