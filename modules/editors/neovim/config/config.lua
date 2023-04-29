nixcfg = require('config.nix')

if nixcfg.codestats.enable then
  -- vim.g.codestats_api_key = nixcfg.codestats.key
  vim.env.CODESTATS_API_KEY = nixcfg.codestats.key
end

vim.g.mapleader = ' '

vim.opt.rnu = true
vim.opt.clipboard = 'unnamedplus'
vim.opt.formatoptions = { n = true, j = true, t = true}

vim.opt.softtabstop = 4
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.listchars['tab'] = '→'

vim.opt.termguicolors = true
vim.cmd('colorscheme monokai')


-- Tree
require("nvim-web-devicons").setup()

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.g.nvim_tree_icons = {
  default = '',
}

require("nvim-tree").setup()

vim.keymap.set('n', '<Leader>op', '<cmd>NvimTreeToggle<cr>')

-- Search

require("fzf-lua").setup()

vim.keymap.set('n', '<Leader>fr', '<cmd>FzfLua live_grep<cr>')
vim.keymap.set('n', '<Leader>ff', '<cmd>FzfLua files<cr>')
vim.keymap.set('n', '<Leader>fb', '<cmd>FzfLua buffers<cr>')

-- Other

vim.keymap.set('n', '<ESC>', '<cmd>noh<cr>')

vim.api.nvim_create_autocmd({'VimEnter'}, {
    pattern = '*',
    command = 'hi Normal guibg=NONE ctermbg=NONE',
})

vim.api.nvim_create_autocmd({'VimLeave'}, {
    pattern = '*',
    command = 'call system("xsel -ib", getreg("+"))',
})
