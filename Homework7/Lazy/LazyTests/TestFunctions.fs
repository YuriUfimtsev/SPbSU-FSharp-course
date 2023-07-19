module LazyTests.TestFunctions

open System

type TestFunctions() =
    static member GuessNumber () =
        let random = Random()
        let prediction = random.Next(2)
        let number = random.Next(2)
        number = prediction

    static member CalculateRandomValue () =
        let random = Random()
        let firstArgument = random.Next(1000)
        let secondArgument = random.Next(1000)
        firstArgument + secondArgument

    static member ConcatenateString () =
        let random = Random()
        let stringLength = random.Next(5)
        let mutable newString = String.Empty

        for i = 0 to stringLength do
            newString <- String.concat newString [ "f" ]

        newString

    static member CreateNewObject () = obj ()
