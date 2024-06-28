# LambdaAdventure!

## Lambda what?

LambdaAdventure is a rather crude "game" intended more as a working-example Haskell implementation than for action-packed entertainment.

## So ... this is for learning Haskell?

Not really, though this could be a useful supplement for more comprehensive resources (e.g. the truly excellent ![Haskell Book](https://haskellbook.com/)).

## So ... what's the point?

This is intended simply as a proof-of-concept of a very simple text-based console game implemented in Haskell.  As such, this is particularly useful for those who are relatively new to Haskell and want to see various aspects of the Haskell core language and libraries in use.

## Whatever ... just tell me about LambdaAdventure

The playfield is an 2D-grid.  This grid can contain monsters, bombs, treasure and you.  The goal is to collect the treasure and kill as many monsters as possible, all while avoiding being eaten. You can kill a monster by shooting left, right, up, or down.  Both you and monsters can move through the grid.  You can see around you via a rudimentary scanner. Everyone can only move once per "turn". You can leave bombs for monsters (or yourself, if you aren't careful) to step on. Anyone that is at the grid location of a bomb, will blow up.  If you step on a grid location where a monster is, or if a monster steps on a grid location where you are, you will get eaten.  The Scanner is only marginally useful as it will only tell you if something is there, just not what (except that it will specifically indicate grid slots that are outside of the 2D-grid).  The only way to tell if the scanner has detected a monster is if it seems to be moving.  But, the scanner cannot distinguish a bomb from treasure, so you have to keep track of where you leave your bombs, shooting a bomb can detonate from a distance (creating a "blast radius" that extends outside the grid location of the bomb), shooting monsters is also effective.  Note: if you shoot treasure, it will not effect it, as it is shielded against gun fire and monsters.  That being said, a bomb is strong enough to destroy it.

## The Game-play breakdown (TODO: Refine and merge with section above)

Cursor can be moved around you.  For a given position, these are turn-ending actions:

* Move to here
* Shoot this direction
* Place bomb here
* Place brick here

Game loop cursor positions:

         UL UM UR        
         ML    MR        
         LL LM LR        




## Wow, this game really sucks

Once again, developing a game that is actually good, is an anti-goal.  That would be too complex for this to serve its purpose as a concise sample working implementation.  That being said if this game is bad enough that it actually is "good", then consider that a bonus.