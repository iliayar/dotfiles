{ config, pkgs, code-stats-vim, secrets, ... }:


{

  home.packages = [ pkgs.xsel ];

  programs.neovim = {
    enable = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [
      vim-nix
      # fzf
      vim-airline
      vim-surround
      vim-gitgutter
      editorconfig-vim
      nerdtree
      rainbow_parentheses
      nerdcommenter
      gruvbox
      # wal
      vim-easymotion
      # rust

      (pkgs.vimUtils.buildVimPluginFrom2Nix { name = "code-stats-vim"; src = code-stats-vim; })
    ];

    extraConfig = ''
      set clipboard=unnamedplus

      let mapleader = "\<Space>"

      let g:rainbow_active = 1

      set rnu

      set softtabstop=4
      set tabstop=4
      set shiftwidth=4
      set expandtab
      set termguicolors

      set guifont=Fira\ Code:h13

      " Display these characters
      set list listchars=tab:→\ ,eol:\ 

      set foldmethod=indent
      set nofoldenable

      filetype plugin on

      let g:codestats_api_key = ${secrets.code-stats-api-key}

      " Airline code::stats
      let g:airline_section_x = airline#section#create_right(['tagbar', 'filetype', '%{CodeStatsXp()}'])

      " Comments
      let g:NERDSpaceDelims = 1

      " Gruvbox colorscheme
      " autocmd VimEnter * colorscheme gruvbox

      " Monokai colorscheme
      " colorscheme monokai

      " Pywal
      " colorscheme wal

      " Transparent background
      autocmd VimEnter * hi Normal guibg=NONE ctermbg=NONE

      " Rainbow brackets
      let g:rbpt_colorpairs = [
      \ ['brown',       'RoyalBlue3'],
      \ ['Darkblue',    'SeaGreen3'],
      \ ['darkgray',    'DarkOrchid3'],
      \ ['darkgreen',   'firebrick3'],
      \ ['darkcyan',    'RoyalBlue3'],
      \ ['darkred',     'SeaGreen3'],
      \ ['darkmagenta', 'DarkOrchid3'],
      \ ['darkmagenta', 'DarkOrchid3'],
      \ ['Darkblue',    'firebrick3'],
      \ ['darkgreen',   'RoyalBlue3'],
      \ ['darkcyan',    'SeaGreen3'],
      \ ['darkred',     'DarkOrchid3'],
      \ ['red',         'firebrick3'],
      \ ]
      au VimEnter * RainbowParenthesesToggle
      au Syntax * RainbowParenthesesLoadRound
      "au Syntax * RainbowParenthesesLoadSquare
      au Syntax * RainbowParenthesesLoadBraces

      nnoremap <ESC> :noh<CR>
      nnoremap <leader>op :NERDTreeToggle<CR>
      nnoremap <leader>e :new COMMAND_OUTPUT<CR>:read !

      map! <S-Down> <Down>
      map! <S-Up> <Up>
      map <S-Down> <Down>
      map <S-Up> <Up>

      " map <leader>gs <Plug>(easymotion-sn)

      imap <Left> <Nop>
      imap <Right> <Nop>
      imap <Up> <Nop>
      imap <Down> <Nop>

      map <Left> <Nop>
      map <Right> <Nop>
      map <Up> <Nop>
      map <Down> <Nop>

      imap <C-Left> <Nop>
      imap <C-Right> <Nop>
      imap <C-Up> <Nop>
      imap <C-Down> <Nop>

      map <C-Left> <Nop>
      map <C-Right> <Nop>
      map <C-Up> <Nop>
      map <C-Down> <Nop>

      " Copy to system clipboard whem exit
      autocmd VimLeave * call system("xsel -ib", getreg('+'))
    '';
  };
}
