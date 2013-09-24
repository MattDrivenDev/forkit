namespace forkit

module Program = 

    [<EntryPoint>]
    let main argv =     
        let forkit = new forkit()
        forkit.Run()
        0
