namespace forkit

open Xna
open Microsoft.Xna.Framework

/// Each 'tile' in the repository represents a commit.
type Commit =     
    | GrowingCommit of Point
    | BigCommit of Point
    | ShrinkingCommit of Point
    | LittleCommit of Point

/// Commits exist within a branch.
type Branch = {
    Commits: (Commit * float)list
}

/// The player takes the role of a git repository (so this is essentially the 
/// 'hero' of the game, that the player is helping.
type Repository = {
    Branches: Branch list
}

[<RequireQualifiedAccess>]
module Repo =     

    let growingCommit x y = GrowingCommit(point x y)

    let bigCommit x y = BigCommit(point x y)

    let shrinkingCommit x y = ShrinkingCommit(point x y)

    let littleCommit x y = LittleCommit(point x y)

    /// Creates a new Branch with 56 commits trailing from a specified head.
    let branch seed position =         
        let commits (p:Point) = 
            seq { 
                for i in seed .. seed + 55 do 
                    if i % 4 = 1 then 
                        yield growingCommit (p.X - (11 * (i - seed))) p.Y, 0.0
                    else if i % 4 = 2 then 
                        yield bigCommit (p.X - (11 * (i - seed))) p.Y, 0.0
                    else if i % 4 = 3 then 
                        yield shrinkingCommit (p.X - (11 * (i - seed))) p.Y, 0.0
                    else if i % 4 = 0 then 
                        yield littleCommit (p.X - (11 * (i - seed))) p.Y, 0.0
            } |> List.ofSeq
        { Commits = commits position }

    /// Initializes a new Repository at a specified set of coordinates.
    let init x y = {
        Branches = 
            [ branch 5 (point x y)
              branch 4 (point x (y + 11))
              branch 4 (point x (y + 22))
              branch 3 (point x (y + 33))
              branch 3 (point x (y + 44))
              branch 2 (point x (y + 55))
              branch 2 (point x (y + 66))
              branch 1 (point x (y + 77))
              branch 1 (point x (y + 88))
              branch 0 (point x (y + 99)) ]
    }

    let pulseCommit (gametime:GameTime) (commit, time) = 
        
        let timesofar = time + (gametime.ElapsedGameTime.TotalMilliseconds)
        
        match commit with
        | GrowingCommit p ->
            if timesofar > 75.0 
                then bigCommit p.X p.Y, 0.0
                else commit, timesofar
        | BigCommit p ->
            if timesofar > 75.0
                then shrinkingCommit p.X p.Y, 0.0
                else commit, timesofar
        | ShrinkingCommit p ->
            if timesofar > 75.0
                then littleCommit p.X p.Y, 0.0
                else commit, timesofar                
        | LittleCommit p ->
            if timesofar > 75.0
                then growingCommit p.X p.Y, 0.0
                else commit, timesofar

    let pulseBranch gametime branch = 
        { branch with Commits = List.map (pulseCommit gametime) branch.Commits }

    let pulseRepo repo gametime = 
        { repo with Branches = List.map (pulseBranch gametime) repo.Branches }

    let moveCommit (gametime:GameTime) (commit, time) =
        let timesofar = time + (gametime.ElapsedGameTime.TotalMilliseconds)
        match commit with
        | GrowingCommit p -> 
            if timesofar > 75.0
                then growingCommit (p.X - 11) p.Y, 0.0
                else commit, timesofar
        | BigCommit p -> 
            if timesofar > 75.0
                then bigCommit (p.X - 11) p.Y, 0.0
                else commit, timesofar
        | ShrinkingCommit p ->
            if timesofar > 75.0
                then shrinkingCommit (p.X - 11) p.Y, 0.0
                else commit, timesofar
        | LittleCommit p ->
            if timesofar > 75.0
                then littleCommit (p.X - 11) p.Y, 0.0
                else commit, timesofar 
                
    let push (gametime:GameTime) commits =  
        let timesofar time = time + (gametime.ElapsedGameTime.TotalMilliseconds)       
        match List.head commits with
        | GrowingCommit p, t ->
            if timesofar t > 75.0
                then (bigCommit (p.X + 11) p.Y, 0.0) :: commits
                else List.map(fun (c, _) -> c, timesofar t) commits
        | BigCommit p, t ->
            if timesofar t > 75.0
                then (shrinkingCommit (p.X + 9) p.Y, 0.0) :: commits
                else List.map(fun (c, _) -> c, timesofar t) commits
        | ShrinkingCommit p, t ->
            if timesofar t > 75.0
                then (littleCommit (p.X + 7) p.Y, 0.0) :: commits
                else List.map(fun (c, _) -> c, timesofar t) commits
        | LittleCommit p, t ->
            if timesofar t > 75.0
                then (growingCommit (p.X + 9) p.Y, 0.0) :: commits
                else List.map(fun (c, _) -> c, timesofar t) commits

    let moveBranch gametime branch = 
        let commits = 
            List.map (moveCommit gametime) branch.Commits
            |> (push gametime)
        { branch with Commits = commits }
        
    let moveRepo repo gametime = 
        { repo with Branches = List.map (moveBranch gametime) repo.Branches }

    /// Draw a single commit to a spritebatch.
    let drawCommit spritebatch (gametime:GameTime) texture (commit, time) = 
        
        let drawone destination color = 
            SpriteBatch.draw spritebatch texture destination (rect 0 0 8 8) color        
        
        match commit with
        | GrowingCommit p ->            
            drawone (rect p.X p.Y 8 8) (Color.GreenYellow * float32 1.05)
        | BigCommit p ->            
            drawone (rect (p.X - 1) (p.Y - 1) 10 10) (Color.GreenYellow * float32 1.25)
        | ShrinkingCommit p ->            
            drawone (rect (p.X) (p.Y) 8 8) (Color.GreenYellow * float32 1.00)
        | LittleCommit p ->            
            drawone (rect (p.X + 1) (p.Y + 1) 6 6) (Color.GreenYellow * float32 0.75)

    /// Draw a single branch to a spritebatch.
    let drawBranch spritebatch gametime texture branch =         
        branch.Commits |> Seq.iter (drawCommit spritebatch gametime texture) 

    /// Draw the entire repo to a spritebatch.
    let draw spritebatch gametime texture repo =         
        repo.Branches|> Seq.iter (drawBranch spritebatch gametime texture)