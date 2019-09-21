call plug#begin('~/.local/share/nvim/plugged')



" Plugins {
Plug 'davidhalter/jedi-vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" }
" Deoplete {
let g:deoplete#enable_at_startup = 1
" }

" General {

set number

set clipboard=unnamedplus

" }

call plug#end()
