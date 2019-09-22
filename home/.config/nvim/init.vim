call plug#begin('~/.local/share/nvim/plugged')



" Plugins {
Plug 'davidhalter/jedi-vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-jedi'
Plug 'vim-airline/vim-airline'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdcommenter'
Plug 'sbdchd/neoformat'
Plug 'scrooloose/nerdtree'
Plug 'neomake/neomake'
Plug 'machakann/vim-highlightedyank'
Plug 'tmhedberg/SimpylFold'
Plug 'majutsushi/tagbar'
Plug 'iamcco/mathjax-support-for-mkdp'
Plug 'iamcco/markdown-preview.vim'
" }
" Deoplete {
let g:deoplete#enable_at_startup = 1
" }
" Vim-Airline {
let g:airline_powerline_fonts = 1
" }
" NERDTree {
noremap  <F2> :NERDTreeToggle<cr>
" }
" Neomake
let g:neomake_python_enabled_makers = ['pylint']
" }
" Tagbar {
nnoremap <F9> :TagbarToggle<CR>
" }
" General {

set list
set listchars=
set listchars+=tab:>\ 
set listchars+=trail:·
set listchars+=extends:»
"set listchars+=space:⋅
set listchars+=precedes:«
set listchars+=nbsp:⣿
let g:indentLine_leadingSpaceEnabled = 1
let g:indentLine_leadingSpaceChar = '·'

let mapleader = ","

set number

set clipboard=unnamedplus

syntax enable

set termguicolors

colorscheme blackarch

set background=dark

map <C-t> :terminal<cr>
" }

call plug#end()
