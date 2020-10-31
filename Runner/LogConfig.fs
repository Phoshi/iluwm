module Runner.LogConfig

open Microsoft.FSharpLu.Json

    
open Newtonsoft.Json

type SerializerSettings =
    static member formatting = Formatting.None
    static member settings =
        JsonSerializerSettings(
            NullValueHandling = NullValueHandling.Ignore,
            MissingMemberHandling = MissingMemberHandling.Error,
            Converters = [| CompactUnionJsonConverter(true, true) |]
        )

let serialize obj =
    obj
    |> Compact.serialize
