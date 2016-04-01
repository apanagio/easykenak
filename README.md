# easykenak
Version 2 of easykenak

Initially it is only a rewrite of the "cutting" application in Haskell. The initial was in Octave, but that is not the only change.
The data structure has changed and 3D vectors are used everywhere.
Input is a json structure describing the uncut building and output is json again describing the segments. Backwards compatibility will probably :-) be kept

to test:
From within the Haskell directory:
`runhaskell app.hs < ../building.json`
