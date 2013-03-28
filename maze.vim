" Maze generator in VIM.
" Joe Wingbermuehle 20130328
"
" To use this, load the generator into vim (run "vim -S maze.vim") then
" call the GenerateMaze function with the maze dimensions, for example:
" :call GenerateMaze(15, 10)
"

" Generate a random direction.
fu! RandDir()
   let r=system("echo $RANDOM")
   return eval(r % 4)
endfu

" Initialize the maze.
fu! InitMaze(width, height)
   for i in range(0, eval(a:width * a:height - 1))
      let g:maze_{i} = 1
   endfor
   for i in range(0, eval(a:width - 1))
      let g:maze_{i} = 0
      let g:maze_{eval(i + (a:height - 1) * a:width)} = 0
   endfor
   for i in range(0, eval(a:height - 1))
      let g:maze_{eval(i * a:width)} = 0
      let g:maze_{eval(i * a:width + a:width - 1)} = 0
   endfor
endfu

" Display the maze.
fu! ShowMaze(width, height)
   put ='Maze generator in VIM'
   put ='Joe Wingbermuehle 20130328'
   for y in range(0, eval(a:height - 1))
      let line = ''
      for x in range(0, eval(a:width - 1))
         let i = eval(y * a:width + x)
         if g:maze_{i} == 1
            let line .= '[]'
         else
            let line .= '  '
         endif
      endfor
      put =line
   endfor
endfu

" Carve the maze.
fu! CarveMaze(width, height, x, y)
   let d = RandDir()
   for i in range(0, 3)
      let dx = 0
      let dy = 0
      if d == 0
         let dx = 1
      elseif d == 1
         let dx = -1
      elseif d == 2
         let dy = 1
      else
         let dy = -1
      endif
      let a = eval((a:y + dy) * a:width + a:x + dx)
      let nx = eval(a:x + 2 * dx)
      let ny = eval(a:y + 2 * dy)
      let b = eval(ny * a:width + nx)
      if g:maze_{a} == 1 && g:maze_{b} == 1
         let g:maze_{a} = 0
         let g:maze_{b} = 0
         call CarveMaze(a:width, a:height, nx, ny)
      endif
      let d = eval((d + 1) % 4)
   endfor
endfu

" Generate and display a random maze.
fu! GenerateMaze(width, height)
   let w = eval(a:width * 2 + 1)
   let h = eval(a:height * 2 + 1)
   call InitMaze(w, h)
   call CarveMaze(w, h, 2, 2)
   let g:maze_{eval(1 * w + 2)} = 0
   let g:maze_{eval((h - 2) * w + (w - 3))} = 0
   call ShowMaze(w, h)
endfu

