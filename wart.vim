" Vim syntax file
" Language:    wart
" Maintainer:  Kartik Agaram <wart@akkartik.com>
" URL:         http://github.com/akkartik/wart
" License:     public domain
"
" Copy this into your ftplugin directory, and add the following to your vimrc:
"   autocmd BufReadPost,BufNewFile *.wart,*.test set filetype=wart
" Screenshot: http://i.imgur.com/H1h7M.png (colors not provided)

let s:save_cpo = &cpo
set cpo&vim

if exists("b:syntax")
  finish
endif
let b:syntax = "wart"

setlocal iskeyword=@,48-57,?,!,_,$

syntax match wartComment /#.*$/
highlight link wartComment Comment
syntax match wartSalientComment /##.*$/
highlight link wartSalientComment SalientComment
" I have these in my dark colorscheme across all languages.
highlight Comment ctermfg=DarkBlue
highlight SalientComment ctermfg=Cyan

syntax keyword wartKeyword nil false object caller_scope fn
highlight link wartKeyword Constant

syntax region wartString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell
highlight link wartString String

" 3 4 3.4 -3.4 -.5 2.9e-3.4 f23
" ^ ^ ^   ^      ^ ^      ^     numbers
syntax match wartNumber "\(\W\)\@<=-\=\(\.\d\+\|\d\+\(\.\d*\)\=\)\([dDeEfFlL][-+]\=\d\+\)\="
syntax match wartNumber "^-\=\(\.\d\+\|\d\+\(\.\d*\)\=\)\([dDeEfFlL][-+]\=\d\+\)\="
highlight link wartNumber Constant

syntax match wartQuote /'/
highlight link wartQuote Delimiter

syntax match wartCons /\.\.\./
highlight link wartCons Delimiter

" deemphasize period operator
syntax match wartCall /\./
highlight link wartCall Delimiter

" highlight assignment
syntax match wartAssign /<-/
syntax match wartAssign /\<\k*!\>/
highlight link wartAssign Special

syntax match wartSplice /@/
highlight link wartSplice Delimiter

syntax match wartUnquote /,@\|,/
highlight link wartUnquote Preproc

syntax region wartList      matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=@wartListCluster
syntax region wartBackquote matchgroup=Preproc start="`(" matchgroup=Preproc end=")" contains=@wartListCluster
syntax cluster wartListCluster contains=wartComment,wartString,wartNumber,wartKeyword,wartQuote,wartCons,wartCall,wartAssign,wartSplice,wartUnquote,wartList,wartBackquote

syntax match wartParenError /)/
highlight link wartParenError Error

let &cpo = s:save_cpo
