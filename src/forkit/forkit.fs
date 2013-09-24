﻿namespace forkit

open Xna
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type forkit() as this = 
    inherit FsGame(
        "Content",
        (fun gfx ->
            gfx.PreferredBackBufferWidth <- 1280
            gfx.PreferredBackBufferHeight <- 768
            gfx.GraphicsProfile <- GraphicsProfile.HiDef
            gfx.IsFullScreen <- false
        )
    )

    let mutable tile = Unchecked.defaultof<_>

    let mutable gamestate = {
        Repo = Repo.init 100 100
    }

    let textures _ =
        tile <- this.Content.Load<Texture2D>("particles")

    let blackBackground _ _ =
        this.Graphics.GraphicsDevice.Clear(Color.Black)

    let beginSpriteBatch spritebatch _ =
        SpriteBatch.start spritebatch

    let endSpriteBatch spritebatch _ =
        SpriteBatch.stop spritebatch

    let drawRepo spritebatch gametime = 
        Repo.draw spritebatch tile gamestate.Repo

    override this.LoadContentSteps = 
        [ textures ]

    override this.DrawSteps = 
        [ blackBackground
          beginSpriteBatch
          drawRepo
          endSpriteBatch ]