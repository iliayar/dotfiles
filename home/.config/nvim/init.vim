set clipboard=unnamedplus

let mapleader = "\<Space>"

let g:rainbow_active = 1

set number

set softtabstop=4
set tabstop=4
set shiftwidth=4
set expandtab

" Display these characters
set list listchars=tab:\ ,eol:\ 


" Plugins here
call plug#begin()

" Fzf search plugin
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Info line
Plug 'itchyny/lightline.vim'

" Surround text with brackets
Plug 'tpope/vim-surround'


" Syntax checking
" Plug 'w0rp/ale'

" Git marks
Plug 'airblade/vim-gitgutter'

" Configs for projects
Plug 'editorconfig/editorconfig-vim'

" File tree
Plug 'scrooloose/nerdtree'

" Folding plugin
Plug 'pseewald/vim-anyfold'

" Rainbow brackets yey
Plug 'kien/rainbow_parentheses.vim'

" Commenter
Plug 'preservim/nerdcommenter'

" Gruvbox
Plug 'morhetz/gruvbox'

" pywal
Plug 'dylanaraps/wal.vim'

" Improved search
Plug 'easymotion/vim-easymotion'

" Kotlin support
Plug 'udalov/kotlin-vim'

call plug#end()

" Gruvbox colorscheme
" autocmd VimEnter * colorscheme gruvbox

" Monokai colorscheme
"colorscheme monokai

" Pywal
colorscheme wal

" Transparent background
"autocmd VimEnter * hi Normal guibg=NONE ctermbg=NONE

" Rainbow brackets
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
"au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces


nnoremap <ESC> :noh<CR>
nnoremap <leader>op :NERDTreeToggle<CR>

map gs <Plug>(easymotion-sn)

" Copy to system clipboard whem exit
autocmd VimLeave * call system("xsel -ib", getreg('+'))
