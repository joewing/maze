"Maze generator in Smalltalk"
"Joe Wingbermuehle"
"2010-10-06"

Object subclass: #Maze
   instanceVariableNames: 'width height maze rand'
   classVariableNames: ''
   poolDictionaries: ''
   category: nil
!

!Maze methodsFor: 'Initialization'!
init
   width    := 39.
   height   := 23.
   rand     := Random new.
   maze     := ByteArray new: ((height + 1) * width + 1).
   1 to: height do: [ :y |
      1 to: width do: [ :x |
         maze at: (y * width + x) put: 1.
      ].
   ].
   ^self.
!

!Maze class methodsFor: 'Creation'!
new
   | r |
   r := super new.
   r init.
   ^r
!!

!Maze methodsFor: 'Display'!
print
   1 to: height do: [ :y |
      1 to: width do: [ :x |
         ((maze at: (y * width + x)) = 1) ifTrue:  [ '[]' display ]
                                          ifFalse: [ '  ' display ].
      ].
      '' displayNl.
   ]
!!

!Maze methodsFor: 'Generation'!

"Get a random direction."
get_direction
   ^rand between: 0 and: 3
!

"Start carving at x, y"
carveat: x and: y
   | dir count x1 y1 x2 y2 dx dy |
   dir := self get_direction.
   count := 0.
   [ count < 4 ] whileTrue: [
      dx := 0. dy := 0.
      dir = 0 ifTrue: [ dx :=  1 ].
      dir = 1 ifTrue: [ dy :=  1 ].
      dir = 2 ifTrue: [ dx := -1 ].
      dir = 3 ifTrue: [ dy := -1 ].
      x1 :=  x + dx. y1 :=  y + dy.
      x2 := x1 + dx. y2 := y1 + dy.
      (x2 > 1) & (x2 < width) & (y2 > 1) & (y2 < height) ifTrue: [
         ((maze at: (y1 * width + x1)) = 1) ifTrue: [
            ((maze at: (y2 * width + x2)) = 1) ifTrue: [
               maze at: (y1 * width + x1) put: 0.
               maze at: (y2 * width + x2) put: 0.
               self carveat: x2 and: y2.
            ].
         ].
      ].
      count := count + 1.
      dir := (dir + 1) \\ 4.
   ].
!

"Generate the maze."
generate
   maze at: (2 * width + 2) put: 0.
   self carveat: 2 and: 2.
   maze at: (1 * width + 2) put: 0.
   maze at: (height * width + width - 1) put: 0.
!!

"Generate and display a random maze."
| m |
m := Maze new.
m generate.
m print.

