" set packpath=~/.vim_back,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after
" set runtimepath=~/.vim_back,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after

" let &runtimepath='~/.vim_back'
" let $VIMHOME = '~/.vim_back'

" set default 'runtimepath' (without ~/.vim folders)
let &runtimepath = printf('%s/vimfiles,%s,%s/vimfiles/after', $VIM, $VIMRUNTIME, $VIM)

" what is the name of the directory containing this file?
let s:portable = expand('<sfile>:p:h')

" add the directory to 'runtimepath'
let &runtimepath = printf('%s,%s,%s/after', s:portable, &runtimepath, s:portable)

syntax on
set clipboard=unnamedplus

colorscheme monokai

set backspace=indent,eol,start

set number

set softtabstop=4
set tabstop=4
set expandtab

hi Normal guibg=NONE ctermbg=NONE
set list listchars=tab:\ ,eol:\ 


autocmd VimLeave * call system("xsel -ib", getreg('+'))
