" Highlighting wart's literate directives in C++ sources.
function! HighlightTangledFile()
  set ft=cpp
  syntax region wartTangle start=+:(+ end=+)+
  highlight link wartTangle Delimiter
  syntax region wartTrace start="^+" end="$"
  highlight wartTrace ctermfg=darkgreen
  syntax region wartTraceAbsent start="^-" end="$"
  highlight wartTraceAbsent ctermfg=darkred
  syntax region wartTraceResult start="^=>" end="$"
  highlight wartTraceResult ctermfg=yellow
endfunction
call HighlightTangledFile()
autocmd BufReadPost,BufNewFile 0* call HighlightTangledFile()
