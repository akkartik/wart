" Copy this into your vimrc.

" parens/ssyntax very subtle
highlight Delimiter ctermfg=DarkMagenta
" quasiquote/unquote slightly less subtle
highlight Preproc ctermfg=Blue

" quoted syms are like literal numbers or strings
highlight link lispAtom Constant

function! WartSettings()
  set filetype=lisp
  set nolisp " since we lack parens so often

  "" ssyntax
  set iskeyword-=:
  " !a !~a.b:c!d&:e.f!.g!!h?.i j. k!( l!'
  " ^  ^^ ^ ^ ^ ^  ^  ^  ^  ^   ^   ^   ^   highlight these like parens
  syntax match SSyntax /[^ ]\zs\./  " period after sym
  syntax match SSyntax /!\([^ \t'.!(),@`]\)\@=/   " bang before sym
  syntax match SSyntax /[^ ]\zs[:&]\([^ ]\)\@=/   " infix colon or ampersand
  syntax match SSyntax /\~\([^ ]\)\@=/  " tilde before sym
  syntax cluster lispListCluster add=SSyntax
  highlight link SSyntax Delimiter
  " hack: symbols are interfering with SSyntax
  syntax clear lispSymbol
  syntax clear lispFunc

  "" unquote and splice
  syntax match Unquote /,@\|,/
  syntax cluster lispListCluster add=Unquote
  highlight link Unquote Preproc
  syntax match Splice /@/
  syntax cluster lispListCluster add=Delimiter
  highlight link Splice Delimiter

  " unquote trumps quote
  syntax clear lispAtom
  syntax match lispAtom "'[^ ,@\t()]\+" contains=lispAtomMark
endfunction

autocmd BufReadPost,BufNewFile *.wart,*.test,*.wtst call WartSettings()
if (expand("%:e") =~ 'wart\|test\|wtst') " in case we loaded this too late on startup
  call WartSettings()
endif
