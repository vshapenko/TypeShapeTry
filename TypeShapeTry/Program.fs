

open TypeShape.Core
open TypeShape.Core.Utils
open LiteDB

 type Convert<'t> = {To:'t->BsonValue;From:BsonValue->'t}
 [<AutoOpen>]
 module Impl=
   let inline delay (f : 'T) : BsonValue->'T = fun  _-> f

 let rec genPickler<'T> () : Convert<'T> =
    let ctx = new TypeGenerationContext()
    genPicklerCached<'T> ctx
    
 and private genPicklerCached<'T> (ctx : TypeGenerationContext) : Convert<'T> =
    let delay (c : Cell<Convert<'T>>) : Convert<'T> =
        { To = fun sb -> c.Value.To sb
          From= fun x->c.Value.From x }

    match ctx.InitOrGetCachedValue<Convert<'T>> delay with
    | Cached(value = f) -> f
    | NotCached t ->
        let p = genPicklerAux<'T> ctx
        ctx.Commit t p
 
 and private genPicklerAux<'T> (ctx : TypeGenerationContext) : Convert<'T> =
  let mkParser (parser:'T->BsonValue) (writer:BsonValue->'T)=
      {
        To=unbox parser
        From=unbox writer
      }
  let mkMemberPickler (v:'Class) (shape : IShapeMember<'Class>) =
        shape.Accept { new IMemberVisitor<'Class, ('Class->BsonValue)*(BsonValue->'Class)> with
            member __.Visit (shape : ShapeMember<'Class, 'Field>) =
                let fP = genPicklerCached<'Field> ctx
                
                let printer=fun x->
                    shape.Get x |>fP.To
                let parser=
                    fun bson->
                        shape.Set v (fP.From bson)
                  
                printer,parser
        }

  let combineMemberPicklers (v:'Class) (members : IShapeMember<'Class> []) =
        let (printers,parsers)=  members |>Array.map  (mkMemberPickler v)|>Array.unzip
        let names=members|>Array.map (fun x->x.Label)|>Array.map(fun x->if (x.ToLower()="id") then "_id" else x)
        let printer  =
           fun x->
            let doc=new BsonDocument()
            let v=unbox<'Class> x
            let arr=printers|>Array.zip  names
            arr|>Array.iter (fun (name,x)-> doc.[name]<-x v)
            doc:>BsonValue
        let parser=
            fun (value:BsonValue) ->
              if (value.IsDocument) then
                 let doc=value.AsDocument
                 let arr=parsers|>Array.zip names
                 arr |>Array.fold( fun _ (n,x)-> x doc.[n] ) v
              else v
                
        mkParser printer parser
        
  match shapeof<'T> with
    | Shape.Unit -> mkParser (fun _->BsonValue.Null) (fun v->unbox v.RawValue)
    | Shape.Bool -> mkParser (fun x->unbox<bool> x|>BsonValue) (fun v->unbox v.RawValue)
    | Shape.Byte -> mkParser (fun x->unbox<byte> x|>BsonValue) (fun v->unbox v.RawValue)
    | Shape.Int32 ->mkParser (fun x->unbox<int> x|>BsonValue) (fun v->unbox v.RawValue)
    | Shape.Int64 ->mkParser (fun x->unbox<int64> x|>BsonValue) (fun v->unbox v.RawValue)
    | Shape.String -> mkParser (fun x->unbox<string> x|>BsonValue) (fun v->unbox v.RawValue)
//    | Shape.FSharpOption s ->
//                 s.Element.Accept {
//                     new ITypeVisitor<Convert<'T>>
//                         with
//                          member __.Visit<'t> () =
//                             let tP = genPicklerCached<'t> ctx
//                             let printer  =
//                                 fun x->
//                                 match unbox<'t option> x with
//                                 | None ->BsonValue.Null
//                                 | Some t ->tP.To t
//                                 
//                             let parser=
//                                 fun (v:BsonValue) ->
//                                     if(not v.IsNull) then tP.From (v)|>Some
//                                     else None
//                             mkParser (printer) (parser)
//                 }
//                 
    | Shape.FSharpList s ->
        s.Element.Accept {
            new ITypeVisitor<Convert<'T>> with
                member __.Visit<'t> () =
                    let eP = genPicklerCached<'t> ctx
                    let printer (x:'t list)=
                        let ts=x
                        ts|>List.map eP.To|>BsonArray:>BsonValue
                        
                    let parser=fun(v:BsonValue)->
                        if(v.IsArray) then v.AsArray|>Seq.map eP.From|>List.ofSeq
                        else []
                        
                    mkParser printer parser
        }
//
//    | Shape.Array s when s.Rank = 1 ->
//        s.Element.Accept {
//                       new ITypeShapeVisitor<Convert<'T>> with
//                member __.Visit<'t> () =
//                    let eP = genPicklerCached<'t> ctx
//                    let printer=
//                      fun x->
//                        let ts=unbox<'t array> x
//                        ts|>Array.map eP.To|>BsonArray:>BsonValue
//                    mkParser printer
//        }
//
//    | Shape.FSharpMap s ->
//        s.Accept {
//            new IFSharpMapVisitor<Convert<'T>> with
//                member __.Visit<'k,'v when 'k : comparison> () =
//                    if typeof<'k> <> typeof<string> then failwithf "Type '%O' is not supported" typeof<'T>
//                    let vp = genPicklerCached<'v> ctx
//                    let printer =
//                       fun x->
//                        let m= unbox<Map<'k,'v>> x
//                        let mutable doc=new BsonDocument()
//                        m|>Map.map (fun k v->vp.To v)|>Map.iter(fun k v-> doc.[k.ToString()]<-v)
//                        doc:>BsonValue
//                    mkParser printer 
//        }
//
//    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
//        combineMemberPicklers  shape.Elements
//
//    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
//        combineMemberPicklers shape.Fields
//
//    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
//        let mkUnionCaseInfo (case : ShapeFSharpUnionCase<'T>) =
//            let printer=
//                fun x->
//                  let v=unbox<'T> x
//                  let mutable doc=BsonDocument()
//                  doc.["Case"]<-case.CaseInfo.Name|>BsonValue
//                  doc.["Value"]<-(combineMemberPicklers  case.Fields).To v
//                  doc:>BsonValue
//            mkParser printer
//            
//            
//        let caseInfo = shape.UnionCases |> Array.map mkUnionCaseInfo
//
//        {
//            To = 
//                fun x ->
//                    let t=unbox<'T> x
//                    
//                    let tag = shape.GetTag t
//                    let printer= caseInfo.[tag]
//                    printer.To t
//
//
//        } 
    | _ -> failwithf "unsupported type '%O'" typeof<'T>


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let v=genPickler<string> ()
    let vv=v.From (BsonValue "Hello")
    0 // return an integer exit code
