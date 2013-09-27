module Movement

open Microsoft.Xna.Framework.Input

type UpDownMovement = | Up | Down

let playerMovement() = 
    let state = Keyboard.GetState()
    if state.IsKeyDown(Keys.Up) then Some Up
    else if state.IsKeyDown(Keys.Down) then Some Down
    else None