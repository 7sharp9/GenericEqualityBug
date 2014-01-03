Generic Equality Bug
====================
These are unit tests for iOS that show a failure in generic equality in Arrays  
e.g [|0|] = [|0|] should equate to true, however, this throws a null reference exception while running on iOS hardware:

```
iec: Microsoft.FSharp.Core.LanguagePrimitives+HashCompare+fsEqualityComparer@1632
 	[FAIL] Using GenericEquality code from Fsharp.Core I expect identical integer arrays to be equal : System.NullReferenceException : Object reference not set to an instance of an object
 		  at Microsoft.FSharp.Core.LanguagePrimitives+HashCompare.GenericEqualityObj$cont@1471 (Boolean er, IEqualityComparer iec, System.Object yobj, System.Object xobj, System.Array arr1, Microsoft.FSharp.Core.Unit unitVar) [0x00178] in /Users/dave/code/fsharp/src/fsharp/FSharp.Core/prim-types.fs:1485 
 		  at Microsoft.FSharp.Core.LanguagePrimitives+HashCompare.GenericEqualityObj (Boolean er, IEqualityComparer iec, System.Object xobj, System.Object yobj) [0x00040] in /Users/dave/code/fsharp/src/fsharp/FSharp.Core/prim-types.fs:1471 
 		  at Microsoft.FSharp.Core.LanguagePrimitives+HashCompare+fsEqualityComparer@1632.System-Collections-IEqualityComparer-Equals (System.Object x, System.Object y) [0x00000] in /Users/dave/code/fsharp/src/fsharp/FSharp.Core/prim-types.fs:1633 
 		  at Microsoft.FSharp.Core.LanguagePrimitives+HashCompare.GenericEqualityIntrinsic[Int32[]] (System.Int32[] x, System.Int32[] y) [0x00000] in <filename unknown>:0 
 		  at LanguageUnitTests.Generic equality tests.Using GenericEquality code from Fsharp.Core I expect identical integer arrays to be equal () [0x00000] in /Users/dave/Projects/FsharpUnitTestProject/GenericEqualityBug/UnitTest.fs:15 
 		  at (wrapper managed-to-native) System.Reflection.MonoMethod:InternalInvoke (System.Reflection.MonoMethod,object,object[],System.Exception&)
 		  at System.Reflection.MonoMethod.Invoke (System.Object obj, BindingFlags invokeAttr, System.Reflection.Binder binder, System.Object[] parameters, System.Globalization.CultureInfo culture) [0x00044] in /Developer/MonoTouch/Source/mono/mcs/class/corlib/System.Reflection/MonoMethod.cs:230 
```
