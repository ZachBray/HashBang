/// This module contains helpers for salting and hashing passwords.
[<RequireQualifiedAccess>]
module Chat.Server.Utilities.Cryptography

open System
open System.Text
open System.Security.Cryptography

let private sha256 = new SHA256Managed()

let private rand = new RNGCryptoServiceProvider()
let private randBytes n =
    let xs = Array.zeroCreate n
    rand.GetBytes xs
    xs

let generateSalt() =
    randBytes 32 |> Convert.ToBase64String

let computeHash (password : string) salt =
    let passBytes = password |> Encoding.UTF8.GetBytes
    let saltBytes = salt |> Convert.FromBase64String
    let allBytes = Array.append passBytes saltBytes
    sha256.ComputeHash allBytes |> Convert.ToBase64String