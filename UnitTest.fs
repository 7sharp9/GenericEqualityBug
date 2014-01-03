namespace LanguageUnitTests
open System
open NUnit.Framework


[<TestFixture>]
type ``Generic equality tests``() =
    [<Test>]
    member x.``Using duplicated GenericEquality code from Fsharp.Core I expect identical integer arrays to be equal``() = 
        let areEq = GenericEquality.GenericEqualityIntrinsic [|0|] [|0|]
        Assert.IsTrue(areEq)

    [<Test>]
    member x.``Using GenericEquality code from Fsharp.Core I expect identical integer arrays to be equal``() = 
        let areEq = Microsoft.FSharp.Core.LanguagePrimitives.GenericEquality [|0|] [|0|] //same as [|0|] = [|0|]
        Assert.IsTrue areEq

    [<Test>]
    member x.``Using GenericEquality code from Fsharp.Core I expect identical integer lists to be equal``() = 
        let areEq = Microsoft.FSharp.Core.LanguagePrimitives.GenericEquality [0] [0] //same as [0] = [0]
        Assert.IsTrue areEq

