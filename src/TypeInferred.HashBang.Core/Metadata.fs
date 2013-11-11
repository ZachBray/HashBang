namespace TypeInferred.HashBang

open System
open System.Net
open System.Reflection

open TypeInferred.HashBang.Runtime

type HttpMethod = 
    
    Get | Post | Put | Delete

    member meth.HttpName =
        match meth with
        | Get -> "GET"
        | Post -> "POST"
        | Put -> "PUT"
        | Delete -> "DELETE"

type TypeMetadata =
    {
        AssemblyQualifiedName : string
    }
    static member From(t : Type) =
        {
            AssemblyQualifiedName = t.AssemblyQualifiedName
        }

    member meta.RuntimeType =
        let resolveAssembly(name : AssemblyName) = 
            Assembly.Load name
        let resolveType (ass : Assembly) name ignoreCase =
            ass.GetType(name, true, ignoreCase)
        let t =
            Type.GetType(
                meta.AssemblyQualifiedName, 
                (fun name -> resolveAssembly name), 
                (fun ass name ignoreCase -> resolveType ass name ignoreCase),
                true, true)
        match t with
        | null -> failwithf "Could not locate: %s" meta.AssemblyQualifiedName
        | v -> v

type ParameterMetadata =
    {
        Key : string
        Type : PrimitiveType
        IsRequired : bool
    }

type HandlerMetadata =
    {
        Resource : string
        Action : string
        Description : string
        UrlParts : UrlPart[]
        Method : HttpMethod
        Headers : ParameterMetadata[]
        QueryParameters : ParameterMetadata[]
        RequestType : (ContentType * TypeMetadata) option
        ResponseType : (ContentType * TypeMetadata) option
    }

    static member Empty =
        {
            Resource = ""
            Action = ""
            Description = ""
            UrlParts = [||]
            Method = Get
            Headers = [||]
            QueryParameters = [||]
            RequestType = None
            ResponseType = None
        }

    member meta.Parameters =
        meta.UrlParts |> Array.choose (function
            | FixedPart _ -> None
            | VariablePart(name, t) -> Some { Key = name; Type = t; IsRequired = true }) 