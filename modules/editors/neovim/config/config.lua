nixcfg = require('config.nix')

if nixcfg.codestats.enable then
  vim.g.codestats_api_key = nixcfg.codestats.key
end

vim.g.mapleader = "<Space>"

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

vim.keymap.set('n', '<ESC>', '<cmd>noh<cr>')

-- FIXME: Is it working
vim.api.nvim_create_autocmd({'VimEnter'}, {
    pattern = '*',
    command = 'hi Normal guibg=NONE ctermbg=NONE',
})

vim.api.nvim_create_autocmd({'VimLeave'}, {
    pattern = '*',
    command = 'call system("xsel -ib", getreg("+"))',
})
