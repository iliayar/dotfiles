
" set packpath=~/.vim_back,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after
" set runtimepath=~/.vim_back,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after

syntax on
set clipboard=unnamedplus

colorscheme monokai

set number

set softtabstop=4
set tabstop=1
set expandtab

hi Normal guibg=NONE ctermbg=NONE

autocmd VimLeave * call system("xsel -ib", getreg('+'))
