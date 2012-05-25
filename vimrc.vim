" Copy this into your vimrc.

highlight Comment ctermfg=Cyan cterm=bold
highlight Constant ctermfg=Cyan cterm=none
" parens/ssyntax very subtle
highlight Delimiter ctermfg=DarkMagenta cterm=none
" quasiquote/unquote slightly less subtle
highlight Preproc ctermfg=Blue cterm=bold

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
  " ,@a (,@b)
  syntax match Unquote /,@\|,/
  syntax cluster lispListCluster add=Unquote
  highlight link Unquote Preproc
  " @a (@b)
  syntax match Splice /@/
  syntax cluster lispListCluster add=Splice
  highlight link Splice Delimiter

  " quoted and keyword literals
  " :a (:b -12 -5.6 :c -7e-8 "d" 'e :f) ',g :h   these are all literals except g
  syntax clear lispAtom
  syntax match lispAtom "'[^ \t'.!(),@`]\+" contains=lispAtomMark
  syntax match lispAtom "\(\w\)\@<!:\w\+"
  syntax cluster lispListCluster add=lispAtom
endfunction

autocmd BufReadPost,BufNewFile *.wart,*.test,*.wtst call WartSettings()
if (expand("%:e") =~ 'wart\|test\|wtst') " in case we loaded this too late on startup
  call WartSettings()
endif
