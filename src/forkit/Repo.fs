namespace forkit

open Xna
open Microsoft.Xna.Framework

/// Each 'tile' in the repository represents a commit.
type Commit = Commit of Rectangle

/// Commits exist within a branch.
type Branch = {
    Commits: Commit list
}

/// The player takes the role of a git repository (so this is essentially the 
/// 'hero' of the game, that the player is helping.
type Repository = {
    Branches: Branch list
}

[<RequireQualifiedAccess>]
module Repo =     

    /// Creates a new Commit.
    let commit x y = Commit(rect x y 8 8)

    /// Creates a new Branch with a specified Commit at the head.
    let branch head = { Commits = [head] }

    /// Initializes a new Repository at a specified set of coordinates.
    let init x y = {
        Branches = 
            [ branch (commit x (y +  0))
              branch (commit x (y + 10))
              branch (commit x (y + 20))
              branch (commit x (y + 30))
              branch (commit x (y + 40))
              branch (commit x (y + 50))
              branch (commit x (y + 60))
              branch (commit x (y + 70))
              branch (commit x (y + 80))
              branch (commit x (y + 90)) ]
    }

    /// Draw a single commit to a spritebatch.
    let drawCommit spritebatch texture (Commit destination) = 
        SpriteBatch.draw spritebatch texture destination (new Rectangle(0, 0, 8, 8)) Color.GreenYellow

    /// Draw a single branch to a spritebatch.
    let drawBranch spritebatch texture branch = 
        let f = drawCommit spritebatch texture
        List.iter f branch.Commits 

    /// Draw the entire repo to a spritebatch.
    let draw spritebatch texture repo = 
        let f = drawBranch spritebatch texture
        List.iter f repo.Branches