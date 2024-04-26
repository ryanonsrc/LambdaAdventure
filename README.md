# LambdaAdventure!

## Lambda what?

LambdaAdventure is a rather crude "game" intended more as a working-example Haskell implementation than for action-packed entertainment.

## So ... this is for learning Haskell?

Not really, though this could be a useful supplement for more comprehensive resources (e.g. the truly excellent ![Haskell Book](https://haskellbook.com/)).

## So ... what's the point?

This is intended simply as a reference implementation that can be used as a summary of core language features and various library functions.  As such, this is particularly useful for those who are relatively new to Haskell and want to jog their memory and/or would like a working example putting various aspects of the Haskell language into use.

## Whatever ... just tell me about LambdaAdventure

The playfield is an M x N 2D-grid.  This grid can contain monsters, bombs, treasure and you.  The goal is to collect the treasure and kill as many monsters as possible, all while avoiding being eaten. You can kill a monster by shooting left, right, up, or down.  Both you and monsters can move through the grid.  You can scan for a monster using a scanner. Everyone can only move once per "turn". You can leave bombs for monsters (or yourself, if you aren't careful) to step on. Anyone that is at the grid location of a bomb, will blow up.  If you step on a grid location where a monster is, or if a monster steps on a grid location where you are, you will get eaten.  The Scanner is only marginally useful as it will only tell you if something is there, just not what.  The only way to tell if the scanner has detected a monster is if it seems to be moving.  But, the scanner cannot distinguish a bomb from treasure, so you have to keep track of where you leave your bombs, shooting a bomb can detonate from a distance (creating a "blast radius" that extends outside the grid location of the bomb), shooting monsters is also effective but don't shoot treasure as it will destroy it just as easily.

## Wow, this game really sucks

Once again, making a game that is actually good, is an anti-goal.  That would be too complex for this to serve its purpose and would have to span multiple modules (whereas the goal for brevity requires us to keep it self-contained in one module) as a relatively succinct sample implementation.  That being said if this game is bad enough that it actually is "good", then consider that a bonus.