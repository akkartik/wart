" Highlighting wart's literate directives in C++ sources.
" Here's how it looks for me: http://i.imgur.com/Q7Gv008.png
function! HighlightTangledFile()
  set ft=cpp
  syntax region wartTangle start=+:(+ end=+)+ contains=cString
  " show them in red
  highlight link wartTangle WarningMsg
  " less salient string escapes to compensate
  highlight cSpecial ctermfg=darkgreen
endfunction
call HighlightTangledFile()
autocmd BufReadPost,BufNewFile 0* call HighlightTangledFile()
