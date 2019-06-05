

namespace LiteDB.FSharp

module Shaper=
 open LiteDB
 open System
 open TypeShape.Core
 open TypeShape.Core.Utils
 type Convert<'t> = {To:'t->BsonValue;From:BsonValue->'t}
 [<AutoOpen>]
 module Impl=
   let inline delay (f : unit->'T) : BsonValue->'T = fun  _-> f()
 
           
  
   let toKey (x:string)=
       if(x.ToLower()="id") then "_id"
       else x.Trim('@')
       
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
 
  let mkParser (parser:'t->BsonValue) (writer:BsonValue->'t):Convert<'T> =
      {
        To= fun x->(unbox parser) x 
        From=fun x->(unbox writer) x
      }
      
  let mkMemberPickler (shape : IShapeMember<'Class>) =
        shape.Accept { new IMemberVisitor<'Class, ('Class->BsonValue)*(BsonValue->'Class->'Class)> with
            member __.Visit (shape : ShapeMember<'Class, 'Field>) =
                let fP = genPicklerCached<'Field> ctx
                
                let printer=fun x->
                    shape.Get x |>fP.To
                    
                let parser=
                    fun (bson:BsonValue)->
                       if(bson.IsDocument) then
                         let doc=bson.AsDocument 
                         fun x->let res=shape.Set x (fP.From doc.[toKey shape.Label])
                                res
                       else fun x->x
                  
                printer,parser
        }

  let combineMemberPicklers (v:BsonValue->'Class) (members : IShapeMember<'Class> [])  =
        let (printers,parsers)=  members |>Array.map  mkMemberPickler|>Array.unzip
        let names=members|>Array.map (fun x->x.Label)|>Array.map toKey
        let printer  =
           fun x->
            let doc=new BsonDocument()
            let arr=printers|>Array.zip  names
            arr|>Array.iter (fun (name,printer)-> doc.[name]<-printer x)
            doc:>BsonValue
            
        let parser=
             fun bson->
               match Array.toList parsers with
               |[]->v bson
               |hd::tl->tl|>List.fold (fun acc p->p bson acc) (bson|>v|>hd bson)
                  

                
        mkParser printer parser
  if(typeof<'T>.Name =typeof<BsonDocument>.Name)
    then mkParser (fun x->x:>BsonValue) (fun x->x.AsDocument)
  else                            
  match shapeof<'T> with
    | Shape.Unit -> mkParser (fun _-> BsonValue.Null) (fun _->())
    | Shape.Bool -> mkParser (fun x-> unbox<bool>x|>BsonValue) (fun v->
                                                                     if(v.IsNull) then false
                                                                     else
                                                                     unbox<bool> v.RawValue) 
    | Shape.Byte -> mkParser (fun x-> unbox<byte>x|>BsonValue) (fun v->unbox<byte> v.RawValue)
    | Shape.Int32 ->mkParser (fun x-> unbox<int>x|>BsonValue) (fun v->unbox<int> v.RawValue)
    | Shape.Int64 ->mkParser (fun x-> unbox<int64>x|>BsonValue) (fun v->unbox<int64> v.RawValue)
    | Shape.String -> mkParser (fun x-> unbox<string>x|>BsonValue) (fun v->unbox<string> v.RawValue)
    | Shape.Guid-> mkParser (fun x-> unbox<Guid>x|>BsonValue) (fun v->unbox<Guid> v.RawValue)
    | Shape.Decimal-> mkParser (fun x-> unbox<Decimal>x|>BsonValue) (fun v->unbox<Decimal> v.RawValue)
    | Shape.Double-> mkParser (fun x-> unbox<Double>x|>BsonValue) (fun v->unbox<Double> v.RawValue)
    | Shape.DateTime->mkParser (fun x->unbox<DateTime>x|>BsonValue) (fun v->unbox<DateTime> v.RawValue)
    | Shape.FSharpOption s ->
                 s.Element.Accept {
                     new ITypeVisitor<Convert<'T>>
                         with
                          member __.Visit<'t>() =
                             let tP = genPicklerCached<'t> ctx
                             let printer =function
                                 | None ->BsonValue.Null
                                 | Some t ->tP.To t
                                 
                             let parser  =
                                 fun (v:BsonValue) ->
                                     let vv=
                                          if(not v.IsNull) then tP.From v|>Some
                                          else None
                                     vv
                             mkParser printer parser
                 }
                 
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

    | Shape.Array s when s.Rank = 1 ->
        s.Element.Accept {
               new ITypeVisitor<Convert<'T>> with
                member __.Visit<'t> () =
                    let eP = genPicklerCached<'t> ctx
                    let printer=
                      fun x->
                        let ts=unbox<'t array> x
                        ts|>Array.map eP.To|>BsonArray:>BsonValue
                      
                    let parser=fun(v:BsonValue)->
                        if(v.IsArray) then v.AsArray|>Seq.map eP.From|>Array.ofSeq
                        else [||]
                        
                    mkParser printer parser
        }

    | Shape.FSharpMap s ->
        s.Accept {
            new IFSharpMapVisitor<Convert<'T>> with
                member __.Visit<'k,'v when 'k : comparison> () =
                    if typeof<'k> <> typeof<string> then failwithf "Type '%O' is not supported" typeof<'T>
                    let vp = genPicklerCached<'v> ctx
                    let printer =
                       fun x->
                        let m= unbox<Map<'k,'v>> x
                        let mutable doc=new BsonDocument()
                        m|>Map.map (fun k v->vp.To v)|>Map.iter(fun k v-> doc.[k.ToString()]<-v)
                        doc:>BsonValue
                        
                    let parser=
                        fun (v:BsonValue)->
                         
                         if(v.IsDocument) then
                             v.AsDocument.RawValue|>Seq.fold (fun (acc:Map<string,'v>) pair->acc.Add(pair.Key,vp.From pair.Value)) Map.empty
                         else Map.empty
                    mkParser printer parser
        }
        

        
    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
        combineMemberPicklers (delay shape.CreateUninitialized) shape.Elements 
        
    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
        combineMemberPicklers (delay shape.CreateUninitialized) shape.Fields 

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
      
        let mkUnionCaseInfo (case : ShapeFSharpUnionCase<'T>) =
            let hasFields = case.Fields.Length > 0
            let init=delay case.CreateUninitialized
            let pickler = combineMemberPicklers (init) case.Fields 
            let printer=
                 fun x->
                  if(hasFields) then
                    let doc=new BsonDocument()
                    doc.["__case"]<-case.CaseInfo.Name|>BsonValue
                    doc.["Items"]<- pickler.To x
                    doc|>BsonValue
                   else (case.CaseInfo.Name|>BsonValue)
            let parser=
                 fun v->
                    if(hasFields) then
                       pickler.From v
                    else
                     init v
                     
                      
            mkParser printer parser
 
        let caseInfo = shape.UnionCases |> Array.map mkUnionCaseInfo

        {
                To = 
                    fun x ->
                        let tag = shape.GetTag x
                        let printer= caseInfo.[tag]
                        printer.To x

                From=
                    fun v->
                       if(v.IsDocument) then
                           let doc=v.AsDocument
                           let case=doc.["__case"].AsString
                           let index=shape.UnionCases|>Array.findIndex(fun x->x.CaseInfo.Name=case)
                           let v=doc.[case]
                           let printer=caseInfo.[index]
                           printer.From doc.["Items"]
                           
                       else if (v.IsString) then
                            let str=v.AsString
                            let index=shape.UnionCases|>Array.findIndex(fun x->x.CaseInfo.Name=str)
                            let printer=caseInfo.[index]
                            printer.From v 
                                
                       else raise (ArgumentException("Invalid type!!!"))
        }
        
    | Shape.Poco ((:? ShapePoco<'T> as shape))->
        combineMemberPicklers (delay shape.CreateUninitialized) (shape.Fields|>Array.filter(fun s->s.IsPublic)) 
    | _ -> failwithf "unsupported type '%O'" typeof<'T>

 
 type A={Name:string option;Id:int}
 type DU=
     |One
     |Two of A list 
     |Three of int*string option
     
 type C1={A:int;B:DU}
 [<EntryPoint>]
 let main argv =
    printfn "Hello World from F#!"
    let v=genPickler<C1> ()
    let value={A=1;B=Two [{Id=1;Name=Some "Name"};{Id=2;Name=None}]}
    
    let bson=v.To (value)
    let newValue=v.From bson
    0 // return an integer exit code
