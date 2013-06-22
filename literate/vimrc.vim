" Highlighting wart's literate directives in C++ sources.
function! HighlightTangledFile()
  set ft=cpp
  syntax region wartTangle start=+:(+ end=+)+
  highlight link wartTangle Delimiter
endfunction
call HighlightTangledFile()
autocmd BufReadPost,BufNewFile 0* call HighlightTangledFile()
