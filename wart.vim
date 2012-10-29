" Vim syntax file
" Language:    wart
" Maintainer:  Kartik Agaram <wart@akkartik.com>
" URL:         http://github.com/akkartik/wart
" License:     public domain
"
" Copy this into your ftplugin directory, and add the following to your vimrc:
"   autocmd BufReadPost,BufNewFile *.wart,*.test set filetype=wart

let s:save_cpo = &cpo
set cpo&vim

if exists("b:syntax")
  finish
endif
let b:syntax = "wart"

setlocal iskeyword=@,48-57,?,!,_,$

syntax match wartComment /#.*$/
highlight link wartComment Comment

syntax region	wartString start=+"+ skip=+\\\\\|\\"+ end=+"+	contains=@Spell
highlight link wartString String

syntax match wartAtom /[^\s#"()',@]/

syntax match wartNumber "-\=\(\.\d\+\|\d\+\(\.\d*\)\=\)\([dDeEfFlL][-+]\=\d\+\)\="
highlight link wartNumber Constant

syntax match wartQuote /'/
highlight link wartQuote Delimiter

syntax match wartCons /\.\.\./
highlight link wartCons Delimiter

syntax region wartList matchgroup=Delimiter start="("   skip="|.\{-}|"			matchgroup=Delimiter end=")"	contains=@wartListCluster
syntax match wartSplice /@/
highlight link wartSplice Delimiter

syntax region wartBackquote matchgroup=Preproc start="`(" matchgroup=Preproc end=")" contains=@wartListCluster
syntax match wartUnquote /,@\|,/
highlight link wartUnquote Preproc

" deemphasize period operator
syntax match wartCall /\./
highlight link wartCall Delimiter

" highlight assignment
syntax match wartAssign /<-/
highlight link wartAssign Special

syntax match wartParenError /)/
highlight link wartParenError Error

syntax cluster wartListCluster contains=wartComment,wartString,wartAtom,wartNumber,wartQuote,wartCons,wartList,wartBackquote,wartUnquote,wartSplice,wartCall,wartAssign

let &cpo = s:save_cpo
