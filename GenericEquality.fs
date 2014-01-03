module GenericEquality

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.Text
open Microsoft.FSharp.Core
open NUnit.Framework

module BasicInlinedOperations =  
    //let inline unboxPrim<'T>(x:obj) = (# "unbox.any !0" type ('T) x : 'T #)
    //let inline box     (x:'T) = (# "box !0" type ('T) x : obj #)
    //let inline not     (b:bool) = (# "ceq" b false : bool #)
    //let inline (=)     (x:int)   (y:int)    = (# "ceq" x y : bool #) 
    //let inline (<>)    (x:int)   (y:int)    = not(# "ceq" x y : bool #) 
    //let inline (>=)    (x:int)   (y:int)    = not(# "clt" x y : bool #)
    let inline (>=.)   (x:int64) (y:int64)  = not(# "clt" x y : bool #)
    //let inline (>=...) (x:char)  (y:char)   = not(# "clt" x y : bool #)
    //let inline (<=...) (x:char)  (y:char)   = not(# "cgt" x y : bool #)

    //let inline (/)     (x:int)    (y:int)    = (# "div" x y : int #)
    //let inline (+)     (x:int)    (y:int)    = (# "add" x y : int #)
    let inline (+.)    (x:int64)  (y:int64)  = (# "add" x y : int64 #)
    //let inline (+..)   (x:uint64) (y:uint64) = (# "add" x y : uint64 #)
    //let inline ( *. )  (x:int64)  (y:int64)  = (# "mul" x y : int64 #)
    //let inline ( *.. ) (x:uint64) (y:uint64) = (# "mul" x y : uint64 #)
    //let inline (^)     (x:string) (y:string) = System.String.Concat(x,y)
    //let inline (<<<)   (x:int)    (y:int)    = (# "shl" x y : int #)
    //let inline ( * )   (x:int)    (y:int)    = (# "mul" x y : int #)
    //let inline (-)     (x:int)    (y:int)    = (# "sub" x y : int #)
    //let inline (-.)    (x:int64)  (y:int64)  = (# "sub" x y : int64 #)
    //let inline (-..)   (x:uint64) (y:uint64) = (# "sub" x y : uint64 #)
    //let inline (>)     (x:int)    (y:int)    = (# "cgt" x y : bool #)
    //let inline (<)     (x:int)    (y:int)    = (# "clt" x y : bool #)

    let inline get (arr: 'T[]) (n:int) = arr.GetValue(n) :?> 'T //  (# "ldelem.any !0" type ('T) arr n : 'T #)
    let set (arr: 'T[]) (n:int) (x:'T) = arr.SetValue(x, n ) //(# "stelem.any !0" type ('T) arr n x #)

    let inline objEq (xobj:obj) (yobj:obj) = (# "ceq" xobj yobj : bool #)
    let inline int64Eq (x:int64) (y:int64) = (# "ceq" x y : bool #) 
    let inline int32Eq (x:int32) (y:int32) = (# "ceq" x y : bool #) 
    let inline floatEq (x:float) (y:float) = (# "ceq" x y : bool #) 
    let inline float32Eq (x:float32) (y:float32) = (# "ceq" x y : bool #) 
    let inline charEq (x:char) (y:char) = (# "ceq" x y : bool #) 
    //let inline intOrder (x:int) (y:int) = if (# "clt" x y : bool #) then (0-1) else (# "cgt" x y : int #)
    //let inline int64Order (x:int64) (y:int64) = if (# "clt" x y : bool #) then (0-1) else (# "cgt" x y : int #)
    //let inline byteOrder (x:byte) (y:byte) = if (# "clt" x y : bool #) then (0-1) else (# "cgt" x y : int #)
    let inline byteEq (x:byte) (y:byte) = (# "ceq" x y : bool #) 
    //let inline int64 (x:int) = (# "conv.i8" x  : int64 #)
    //let inline int32 (x:int64) = (# "conv.i4" x  : int32 #)

    let inline zeroCreate (n:int) = Array.zeroCreate n // (# "newarr !0" type ('T) n : 'T[] #)

//-------------------------------------------------------------------------
// EQUALITY
//------------------------------------------------------------------------- 
open BasicInlinedOperations

/// optimized case: Core implementation of structural equality on arrays.
let GenericEqualityByteArray (x:byte[]) (y:byte[]) : bool=
    let lenx = x.Length 
    let leny = y.Length 
    let c = (lenx = leny)
    if not c then c 
    else
        let mutable i = 0 
        let mutable res = true
        while i < lenx do 
            let c = byteEq (get x i) (get y i) 
            if not c then (res <- false; i <- lenx) 
            else i <- i + 1
        res

/// optimized case: Core implementation of structural equality on arrays.
let GenericEqualityInt32Array (x:int[]) (y:int[]) : bool=
    let lenx = x.Length 
    let leny = y.Length 
    let c = (lenx = leny)
    if not c then c 
    else
        let mutable i = 0 
        let mutable res = true
        while i < lenx do 
            let c = int32Eq (get x i) (get y i) 
            if not c then (res <- false; i <- lenx) 
            else i <- i + 1
        res

/// optimized case: Core implementation of structural equality on arrays
let GenericEqualitySingleArray er (x:float32[]) (y:float32[]) : bool=
    let lenx = x.Length 
    let leny = y.Length 
    let f32eq x y = if er && not(float32Eq x x) && not(float32Eq y y) then true else (float32Eq x y)
    let c = (lenx = leny)
    if not c then c 
    else
        let mutable i = 0 
        let mutable res = true
        while i < lenx do 
            let c = f32eq (get x i) (get y i) 
            if not c then (res <- false; i <- lenx) 
            else i <- i + 1
        res

/// optimized case: Core implementation of structural equality on arrays.
let GenericEqualityDoubleArray er (x:float[]) (y:float[]) : bool=
    let lenx = x.Length 
    let leny = y.Length 
    let c = (lenx = leny)
    let feq x y = if er && not(floatEq x x) && not(floatEq y y) then true else (floatEq x y)
    if not c then c 
    else
        let mutable i = 0 
        let mutable res = true
        while i < lenx do 
            let c = feq (get x i) (get y i) 
            if not c then (res <- false; i <- lenx) 
            else i <- i + 1
        res

/// optimized case: Core implementation of structural equality on arrays.
let GenericEqualityCharArray (x:char[]) (y:char[]) : bool=
    let lenx = x.Length 
    let leny = y.Length 
    let c = (lenx = leny)
    if not c then c 
    else
        let mutable i = 0 
        let mutable res = true
        while i < lenx do 
            let c = charEq (get x i) (get y i) 
            if not c then (res <- false; i <- lenx) 
            else i <- i + 1
        res

/// optimized case: Core implementation of structural equality on arrays.
let GenericEqualityInt64Array (x:int64[]) (y:int64[]) : bool=
    let lenx = x.Length 
    let leny = y.Length 
    let c = (lenx = leny)
    if not c then c 
    else
        let mutable i = 0 
        let mutable res = true
        while i < lenx do 
            let c = int64Eq (get x i) (get y i) 
            if not c then (res <- false; i <- lenx) 
            else i <- i + 1
        res



let rec GenericEqualityObj (er:bool) (iec:System.Collections.IEqualityComparer) ((xobj:obj),(yobj:obj)) : bool = 
    (*if objEq xobj yobj then true else  *)
    match xobj,yobj with 
     | null,null -> true
     | null,_ -> false
     | _,null -> false
     | (:? string as xs),(:? string as ys) -> System.String.Equals(xs,ys)
     // Permit structural equality on arrays
     | (:? System.Array as arr1),_ -> 
         match arr1,yobj with 
         // Fast path
         | (:? (obj[]) as arr1),    (:? (obj[]) as arr2)      -> GenericEqualityObjArray er iec arr1 arr2
         // Fast path
         | (:? (byte[]) as arr1),    (:? (byte[]) as arr2)     -> GenericEqualityByteArray arr1 arr2
         | (:? (int32[]) as arr1),   (:? (int32[]) as arr2)   -> GenericEqualityInt32Array arr1 arr2
         | (:? (int64[]) as arr1),   (:? (int64[]) as arr2)   -> GenericEqualityInt64Array arr1 arr2
         | (:? (char[]) as arr1),    (:? (char[]) as arr2)     -> GenericEqualityCharArray arr1 arr2
         | (:? (float32[]) as arr1), (:? (float32[]) as arr2) -> GenericEqualitySingleArray er arr1 arr2
         | (:? (float[]) as arr1),   (:? (float[]) as arr2)     -> GenericEqualityDoubleArray er arr1 arr2
         | _                   ,    (:? System.Array as arr2) -> GenericEqualityArbArray er iec arr1 arr2
         | _ -> xobj.Equals(yobj)
     | (:? IStructuralEquatable as x1),_ -> x1.Equals(yobj,iec)
     // Ensure ER NaN semantics on recursive calls
     | (:? float as f1), (:? float as f2) ->
        if er && (not (# "ceq" f1 f1 : bool #)) && (not (# "ceq" f2 f2 : bool #)) then true // NAN with ER semantics
        else (# "ceq" f1 f2 : bool #) // PER semantics
     | (:? float32 as f1), (:? float32 as f2) ->
        if er && (not (# "ceq" f1 f1 : bool #)) && (not (# "ceq" f2 f2 : bool #)) then true // NAN with ER semantics
        else (# "ceq" f1 f2 : bool #)  // PER semantics
     | _ -> xobj.Equals(yobj)

/// specialcase: Core implementation of structural equality on arbitrary arrays.
and GenericEqualityArbArray er (iec:System.Collections.IEqualityComparer) (x:System.Array) (y:System.Array) : bool =
#if FX_NO_ARRAY_LONG_LENGTH
    if x.Rank = 1 && y.Rank = 1 then 
        // check lengths 
        let lenx = x.Length
        let leny = y.Length 
        (int32Eq lenx leny) &&
        // check contents
        let basex = x.GetLowerBound(0)
        let basey = y.GetLowerBound(0)
        (int32Eq basex basey) &&
        let rec check i = (i >= lenx) || (GenericEqualityObj er iec ((x.GetValue(basex + i)),(y.GetValue(basey + i))) && check (i + 1))
        check 0                    
    elif x.Rank = 2 && y.Rank = 2 then 
        // check lengths 
        let lenx0 = x.GetLength(0)
        let leny0 = y.GetLength(0)
        (int32Eq lenx0 leny0) && 
        let lenx1 = x.GetLength(1)
        let leny1 = y.GetLength(1)
        (int32Eq lenx1 leny1) && 
        let basex0 = x.GetLowerBound(0)
        let basex1 = x.GetLowerBound(1)
        let basey0 = y.GetLowerBound(0)
        let basey1 = y.GetLowerBound(1)
        (int32Eq basex0 basey0) && 
        (int32Eq basex1 basey1) && 
        // check contents
        let rec check0 i =
           let rec check1 j = (j >= lenx1) || (GenericEqualityObj er iec ((x.GetValue(basex0 + i,basex1 + j)), (y.GetValue(basey0 + i,basey1 + j))) && check1 (j + 1))
           (i >= lenx0) || (check1 0 && check0 (i + 1))
        check0 0
    else 
        (x.Rank = y.Rank) && 
        let ndims = x.Rank
        // check lengths 
        let rec precheck k = 
            (k >= ndims) || 
            (int32Eq (x.GetLength(k)) (y.GetLength(k)) && 
             int32Eq (x.GetLowerBound(k)) (y.GetLowerBound(k)) && 
             precheck (k+1))
        precheck 0 &&
        let idxs : int32[] = zeroCreate ndims 
        // check contents
        let rec checkN k baseIdx i lim =
           (i >= lim) ||
           (set idxs k (baseIdx + i);
            (if k = ndims - 1 
             then GenericEqualityObj er iec ((x.GetValue(idxs)),(y.GetValue(idxs)))
             else check (k+1)) && 
            checkN k baseIdx (i + 1) lim)
        and check k = 
           (k >= ndims) || 
           (let baseIdx = x.GetLowerBound(k)
            checkN k baseIdx 0 (x.GetLength(k)))
               
        check 0
#else
    if x.Rank = 1 && y.Rank = 1 then 
        // check lengths 
        let lenx = x.LongLength
        let leny = y.LongLength 
        (int64Eq lenx leny) &&
        // check contents
        let basex = int64 (x.GetLowerBound(0))
        let basey = int64 (y.GetLowerBound(0))
        (int64Eq basex basey) &&                    
        let rec check i = (i >=. lenx) || (GenericEqualityObj er iec ((x.GetValue(basex +. i)),(y.GetValue(basey +. i))) && check (i +. 1L))
        check 0L                    
    elif x.Rank = 2 && y.Rank = 2 then 
        // check lengths 
        let lenx0 = x.GetLongLength(0)
        let leny0 = y.GetLongLength(0)
        (int64Eq lenx0 leny0) && 
        let lenx1 = x.GetLongLength(1)
        let leny1 = y.GetLongLength(1)
        (int64Eq lenx1 leny1) && 
        let basex0 = int64 (x.GetLowerBound(0))
        let basex1 = int64 (x.GetLowerBound(1))
        let basey0 = int64 (y.GetLowerBound(0))
        let basey1 = int64 (y.GetLowerBound(1))
        (int64Eq basex0 basey0) && 
        (int64Eq basex1 basey1) && 
        // check contents
        let rec check0 i =
           let rec check1 j = (j >=. lenx1) || (GenericEqualityObj er iec ((x.GetValue(basex0 +. i,basex1 +. j)),(y.GetValue(basey0 +. i,basey1 +. j))) && check1 (j +. 1L))
           (i >=. lenx0) || (check1 0L && check0 (i +. 1L))
        check0 0L
    else 
        (x.Rank = y.Rank) && 
        let ndims = x.Rank
        // check lengths 
        let rec precheck k = 
            (k >= ndims) || 
            (int64Eq (x.GetLongLength(k)) (y.GetLongLength(k)) && 
             int32Eq (x.GetLowerBound(k)) (y.GetLowerBound(k)) && 
             precheck (k+1))
        precheck 0 &&
        let idxs : int64[] = zeroCreate ndims 
        // check contents
        let rec checkN k baseIdx i lim =
           (i >=. lim) ||
           (set idxs k (baseIdx +. i);
            (if k = ndims - 1
             then GenericEqualityObj er iec ((x.GetValue(idxs)),(y.GetValue(idxs)))
             else check (k+1)) && 
            checkN k baseIdx (i +. 1L) lim)
        and check k = 
           (k >= ndims) || 
           (let baseIdx = x.GetLowerBound(k)
            checkN k (int64 baseIdx) 0L (x.GetLongLength(k)))
               
        check 0
#endif                    
  
/// optimized case: Core implementation of structural equality on object arrays.
and GenericEqualityObjArray er iec (x:obj[]) (y:obj[]) : bool =
    let lenx = x.Length 
    let leny = y.Length 
    let c = (lenx = leny )
    if not c then c 
    else
        let mutable i = 0
        let mutable res = true
        while i < lenx do 
            let c = GenericEqualityObj er iec ((get x i),(get y i))
            if not c then (res <- false; i <- lenx) 
            else i <- i + 1
        res


let fsEqualityComparer = 
    { new System.Collections.IEqualityComparer with
        override iec.Equals(x:obj,y:obj) = GenericEqualityObj false iec (x,y)  // PER Semantics
        override iec.GetHashCode(x:obj) = raise (InvalidOperationException "Not used for hashing") }
        
let fsEqualityComparerER = 
    { new System.Collections.IEqualityComparer with
        override iec.Equals(x:obj,y:obj) = GenericEqualityObj true iec (x,y)  // ER Semantics
        override iec.GetHashCode(x:obj) = raise (InvalidOperationException "Not used for hashing") }

let rec GenericEqualityIntrinsic (x : 'T) (y : 'T) : bool = 
    fsEqualityComparer.Equals((box x), (box y))
    
let rec GenericEqualityERIntrinsic (x : 'T) (y : 'T) : bool =
    fsEqualityComparerER.Equals((box x), (box y))
    
let rec GenericEqualityWithComparerIntrinsic (comp : System.Collections.IEqualityComparer) (x : 'T) (y : 'T) : bool =
    comp.Equals((box x),(box y))
    

//let inline GenericEqualityERFast (x : 'T) (y : 'T) : bool = 
//      GenericEqualityERIntrinsic x y
//      when 'T : bool    = (# "ceq" x y : bool #)
//      when 'T : int     = (# "ceq" x y : bool #)
//      when 'T : sbyte   = (# "ceq" x y : bool #)
//      when 'T : int16   = (# "ceq" x y : bool #)
//      when 'T : int32   = (# "ceq" x y : bool #)
//      when 'T : int64   = (# "ceq" x y : bool #)
//      when 'T : byte    = (# "ceq" x y : bool #)
//      when 'T : uint16  = (# "ceq" x y : bool #)
//      when 'T : uint32  = (# "ceq" x y : bool #)
//      when 'T : uint64  = (# "ceq" x y : bool #)
//      when 'T : nativeint  = (# "ceq" x y : bool #)
//      when 'T : unativeint  = (# "ceq" x y : bool #)
//      when 'T : float = 
//        if not (# "ceq" x x : bool #) && not (# "ceq" y y : bool #) then
//            true
//        else
//            (# "ceq" x y : bool #)
//      when 'T : float32 =
//        if not (# "ceq" x x : bool #) && not (# "ceq" y y : bool #) then
//            true
//        else
//            (# "ceq" x y : bool #)
//      when 'T : char    = (# "ceq" x y : bool #)
//      when 'T : string  = System.String.Equals((# "" x : string #),(# "" y : string #))
//      when ^T : decimal     = System.Decimal.op_Equality((# "" x:decimal #), (# "" y:decimal #))
//                   
//let inline GenericEqualityFast (x : 'T) (y : 'T) : bool = 
//      GenericEqualityIntrinsic x y
//      when 'T : bool    = (# "ceq" x y : bool #)
//      when 'T : int     = (# "ceq" x y : bool #)
//      when 'T : sbyte   = (# "ceq" x y : bool #)
//      when 'T : int16   = (# "ceq" x y : bool #)
//      when 'T : int32   = (# "ceq" x y : bool #)
//      when 'T : int64   = (# "ceq" x y : bool #)
//      when 'T : byte    = (# "ceq" x y : bool #)
//      when 'T : uint16  = (# "ceq" x y : bool #)
//      when 'T : uint32  = (# "ceq" x y : bool #)
//      when 'T : uint64  = (# "ceq" x y : bool #)
//      when 'T : float   = (# "ceq" x y : bool #)
//      when 'T : float32 = (# "ceq" x y : bool #)
//      when 'T : char    = (# "ceq" x y : bool #)
//      when 'T : nativeint  = (# "ceq" x y : bool #)
//      when 'T : unativeint  = (# "ceq" x y : bool #)
//      when 'T : string  = System.String.Equals((# "" x : string #),(# "" y : string #))
//      when ^T : decimal     = System.Decimal.op_Equality((# "" x:decimal #), (# "" y:decimal #))
//      
//// Note, because of the static optimization conditionals for float and float32, this operation has PER semantics
//let inline GenericEqualityWithComparerFast (comp : System.Collections.IEqualityComparer) (x : 'T) (y : 'T) : bool = 
//      GenericEqualityWithComparerIntrinsic comp x y
//      when 'T : bool    = (# "ceq" x y : bool #)
//      when 'T : int     = (# "ceq" x y : bool #)
//      when 'T : sbyte   = (# "ceq" x y : bool #)
//      when 'T : int16   = (# "ceq" x y : bool #)
//      when 'T : int32   = (# "ceq" x y : bool #)
//      when 'T : int64   = (# "ceq" x y : bool #)
//      when 'T : byte    = (# "ceq" x y : bool #)
//      when 'T : uint16  = (# "ceq" x y : bool #)
//      when 'T : uint32  = (# "ceq" x y : bool #)
//      when 'T : uint64  = (# "ceq" x y : bool #)
//      when 'T : float   = (# "ceq" x y : bool #)        
//      when 'T : float32 = (# "ceq" x y : bool #)          
//      when 'T : char    = (# "ceq" x y : bool #)
//      when 'T : nativeint  = (# "ceq" x y : bool #)
//      when 'T : unativeint  = (# "ceq" x y : bool #)
//      when 'T : string  = System.String.Equals((# "" x : string #),(# "" y : string #))                  
//      when ^T : decimal     = System.Decimal.op_Equality((# "" x:decimal #), (# "" y:decimal #))
//      
//// Note, because of the static optimization conditionals for float and float32, this operation has ER semantics
//let inline GenericEqualityWithComparerERFast (comp : System.Collections.IEqualityComparer) (x : 'T) (y : 'T) : bool = 
//      GenericEqualityWithComparerIntrinsic comp x y
//      when 'T : bool    = (# "ceq" x y : bool #)
//      when 'T : int     = (# "ceq" x y : bool #)
//      when 'T : sbyte   = (# "ceq" x y : bool #)
//      when 'T : int16   = (# "ceq" x y : bool #)
//      when 'T : int32   = (# "ceq" x y : bool #)
//      when 'T : int64   = (# "ceq" x y : bool #)
//      when 'T : byte    = (# "ceq" x y : bool #)
//      when 'T : uint16  = (# "ceq" x y : bool #)
//      when 'T : uint32  = (# "ceq" x y : bool #)
//      when 'T : uint64  = (# "ceq" x y : bool #)    
//      when 'T : nativeint  = (# "ceq" x y : bool #)
//      when 'T : unativeint  = (# "ceq" x y : bool #)
//      when 'T : float = 
//        if not (# "ceq" x x : bool #) && not (# "ceq" y y : bool #) then
//            true
//        else
//            (# "ceq" x y : bool #)
//      when 'T : float32 =
//        if not (# "ceq" x x : bool #) && not (# "ceq" y y : bool #) then
//            true
//        else
//            (# "ceq" x y : bool #)                  
//      when 'T : char    = (# "ceq" x y : bool #)
//      when 'T : string  = System.String.Equals((# "" x : string #),(# "" y : string #))
//      when ^T : decimal     = System.Decimal.op_Equality((# "" x:decimal #), (# "" y:decimal #))
//
//
//let inline GenericInequalityFast (x:'T) (y:'T) = (not(GenericEqualityFast x y) : bool)
//let inline GenericInequalityERFast (x:'T) (y:'T) = (not(GenericEqualityERFast x y) : bool)

//let inline GenericEquality               x y = GenericEqualityFast               x y
