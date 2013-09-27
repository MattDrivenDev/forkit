namespace forkit

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

module Xna = 

    let private randomizer = new Random()
    /// Picks a new random number between a clamped min/max.
    let random min max = randomizer.Next(min, max)

    /// Functional wrapper over the Microsoft.Xna.Framework.Rectangle constructor.
    let rect x y width height = new Rectangle(x, y, width, height)

    /// Functional wrapper over the Microsoft.Xna.Framework.Point constructor.
    let point x y = new Point(x, y)
    
    [<RequireQualifiedAccess>]
    module SpriteBatch = 

        /// Functional wrapper over the Microsoft.Xna.Framework.Graphics.SpriteBatch.Begin() method.
        let start (spritebatch:SpriteBatch) = spritebatch.Begin()

        /// Functional wrapper over the Microsoft.Xna.Framework.Graphics.SpriteBatch.End() method.
        let stop (spritebatch:SpriteBatch) = spritebatch.End()

        /// Functional wrapper over one of the Microsoft.Xna.Framework.Graphics.SpriteBatch.Draw() overloads.
        let draw
            (spritebatch:SpriteBatch)
            (texture:Texture2D)
            (destination:Rectangle)
            (source:Rectangle)
            (color:Color) = 

            spritebatch.Draw(
                texture,
                destination,
                System.Nullable(source),
                color
            )