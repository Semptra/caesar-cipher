open System

let caesarEncryptCharacter character offset = 
    if Char.IsLetter character 
    then (char)((((int)character - 97 + offset) % 26) + 97)
    else character

let mapDecryptCharacter character (offset:int) = 
    let charInt = (int)character - 97 - (Math.Abs(offset) % 26)
    if charInt < 0
    then (char)(charInt + 26 + 97)
    else (char)(charInt % 26 + 97)

let caesarDecryptCharacter character offset = 
    if Char.IsLetter character
    then mapDecryptCharacter character offset
    else character

let rec caesarEncryptList text offset = 
    match Seq.toList text with
        | character :: tail -> caesarEncryptCharacter character offset :: caesarEncryptList tail offset
        | _ -> []

let rec caesarDecryptList text offset = 
    match Seq.toList text with
        | character :: tail -> caesarDecryptCharacter character offset :: caesarDecryptList tail offset
        | _ -> []

let caesarEncrypt (text:string) offset = 
    caesarEncryptList (text.ToCharArray() |> List.ofArray) offset |> List.toArray |> System.String

let caesarDecrypt (text:string) offset = 
    caesarDecryptList (text.ToCharArray() |> List.ofArray) offset |> List.toArray |> System.String

[<EntryPoint>]
let main argv =
    printf "Enter text: "
    let text = Console.ReadLine()

    printf "Enter offset: "
    let offset = Int32.Parse(Console.ReadLine())
    
    let encrypted = caesarEncrypt (text.ToLower()) offset
    let decrypted = caesarDecrypt encrypted offset

    printfn "Encrypted: %s" encrypted
    printfn "Decrypted: %s" decrypted

    0 