syntax on

set clipboard=unnamedplus

let mapleader = "\<Space>"

colorscheme monokai

set number

set softtabstop=4
set tabstop=4
set expandtab

" Transparent background
hi Normal guibg=NONE ctermbg=NONE

" Display these characters
set list listchars=tab:\ ,eol:\ 

let g:rainbow_active = 1


" Plugins here
call plug#begin()

" Fzf search plugin
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Info line
Plug 'itchyny/lightline.vim'

"" Surround text with brackets
Plug 'tpope/vim-surround'


"" Syntax checking
Plug 'w0rp/ale'

" Git marks
Plug 'airblade/vim-gitgutter'

" Configs for projects
Plug 'editorconfig/editorconfig-vim'

" File tree
Plug 'scrooloose/nerdtree'


" Folding plugin
Plug 'pseewald/vim-anyfold'

" Rainbow brackets yey
Plug 'frazrepo/vim-rainbow'

" Commenter
Plug 'preservim/nerdcommenter'

call plug#end()

nnoremap <leader>op :NERDTreeToggle<CR>


" Copy to system clipboard whem exit
autocmd VimLeave * call system("xsel -ib", getreg('+'))
