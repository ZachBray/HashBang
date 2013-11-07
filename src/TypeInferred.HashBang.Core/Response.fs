namespace TypeInferred.HashBang

open System

type 'a Response =
    /// The request has succeeded.
    | OK of 'a
    /// The server successfully processed the request, but is not returning any content.
    | NoContent
    /// The requested resource has been assigned a new permanent URI.
    | MovedPermanently of Uri
    /// The response to the request can be found under a different URI. ('method -> GET)
    | SeeOther of Uri
    /// The requested resource resides temporarily under a different URI. ('method -> 'method)
    | TemporaryRedirect of Uri
    /// The request could not be understood by the server due to malformed syntax.
    | BadRequest
    /// The request requires user authentication.
    | Unauthorized
    /// The server has not found anything matching the Request-URI.
    | NotFound
    /// The method specified in the Request-Line is not allowed for the resource identified by the Request-URI. 
    | MethodNotAllowed
    /// The server encountered an unexpected condition which prevented it from fulfilling the request.
    | InternalServerError of string

    /// The HTTP code for the response
    member response.Code =
        match response with
        | OK _ -> 200
        | NoContent -> 204
        | MovedPermanently _ -> 301
        | SeeOther _ -> 303
        | TemporaryRedirect _ -> 307
        | BadRequest -> 400
        | Unauthorized -> 401
        | NotFound -> 404
        | MethodNotAllowed -> 405
        | InternalServerError _ -> 500

module Response =

    /// Maps the content of an OK response. Otherwise, nothing.
    let map f = function
        | OK x -> OK(f x)
        | NoContent -> NoContent
        | MovedPermanently uri -> MovedPermanently uri
        | SeeOther uri -> SeeOther uri
        | TemporaryRedirect uri -> TemporaryRedirect uri
        | BadRequest -> BadRequest
        | Unauthorized -> Unauthorized
        | NotFound -> NotFound
        | MethodNotAllowed -> MethodNotAllowed
        | InternalServerError message -> InternalServerError message

    /// Maps the content of an OK response to another response. Otherwise, nothing.
    let bind f = function
        | OK x -> f x
        | NoContent -> NoContent
        | MovedPermanently uri -> MovedPermanently uri
        | SeeOther uri -> SeeOther uri
        | TemporaryRedirect uri -> TemporaryRedirect uri
        | BadRequest -> BadRequest
        | Unauthorized -> Unauthorized
        | NotFound -> NotFound
        | MethodNotAllowed -> MethodNotAllowed
        | InternalServerError message -> InternalServerError message

    /// Maps the content of an OK response to another response. Otherwise, nothing.
    let bindAsync f resp =
        async {
            match resp with
            | OK x -> return! f x
            | NoContent -> return NoContent
            | MovedPermanently uri -> return MovedPermanently uri
            | SeeOther uri -> return SeeOther uri
            | TemporaryRedirect uri -> return TemporaryRedirect uri
            | BadRequest -> return BadRequest
            | Unauthorized -> return Unauthorized
            | NotFound -> return NotFound
            | MethodNotAllowed -> return MethodNotAllowed
            | InternalServerError message -> return InternalServerError message
        }

    /// Returns an OK response with the option's payload if there is one. 
    /// Otherwise, returns the default argument.
    let fromOptionElse defaultArg = function
        | Some x -> OK x
        | None -> defaultArg