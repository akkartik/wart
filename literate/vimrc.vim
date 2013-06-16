autocmd BufReadPost,BufNewFile 0* call HighlightTangledFile()
function! HighlightTangledFile()
  set ft=cpp
  syntax region wartTangle start=+:(+ end=+)+ contains=cString
  " show them in red
  highlight link wartTangle WarningMsg
  " less salient string escapes to compensate
  highlight cSpecial ctermfg=darkgreen
endfunction
