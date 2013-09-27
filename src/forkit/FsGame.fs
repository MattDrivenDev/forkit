namespace forkit

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

[<AbstractClass>]
type FsGame(contentRoot, configureGraphics) as this = 
    inherit Game()

    let random = new Random()
    let graphics = new GraphicsDeviceManager(this)
    let mutable spritebatch = Unchecked.defaultof<_>

    do 
        this.Content.RootDirectory <- contentRoot
        configureGraphics graphics
        
    member this.Graphics = graphics
    
    member this.Random = random

    /// Ordered list of functions to run during the LoadContent phase of the game lifecycle.
    abstract LoadContentSteps : (unit -> unit) list

    /// Creates the spritebatch and then iterates through and invokes each of the 'LoadContentSteps'.
    override this.LoadContent() = 
        spritebatch <- new SpriteBatch(graphics.GraphicsDevice)
        Seq.iter(fun f -> f()) this.LoadContentSteps

    abstract UpdateSteps : (GameTime -> unit) list

    override this.Update(gametime) = 
        this.UpdateSteps
        |> Seq.iter(fun f ->
            f gametime
        )

    /// Ordered list of functions to run during each Draw phase of the game loop.
    abstract DrawSteps : (SpriteBatch -> GameTime -> unit) list

    /// Iterates through and invokes each of the 'DrawSteps'.
    override this.Draw(gametime) =
        this.DrawSteps 
        |> Seq.iter(fun f ->
            f spritebatch gametime
        )