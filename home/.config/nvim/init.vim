call plug#begin('~/.local/share/nvim/plugged')



" Plugins {
Plug 'davidhalter/jedi-vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'deoplete-plugins/deoplete-clang'
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
Plug 'terryma/vim-multiple-cursors'
Plug 'chriskempson/base16-vim'
" }
" Deoplete {
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
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

colorscheme monokai


set background=dark

map <C-t> :terminal<cr>
noremap <F5> :Neomake<CR>

set tabstop=8
set softtabstop=0
set expandtab
set shiftwidth=4
set smarttab
set scrolloff=8
" }

call plug#end()
call neomake#configure#automake('nrwi', 500)

hi! Normal guibg=NONE
hi! NonText guibg=NONE

