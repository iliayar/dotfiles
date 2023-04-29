nixcfg = require('config.nix')

if nixcfg.codestats.enable then
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

require("nvim-tree").setup({
    disable_netrw = true,
    hijack_netrw = true,
    hijack_directories = {
        enable = true,
        auto_open = true,
    },
})

local function open_nvim_tree(data)

  -- buffer is a directory
  local directory = vim.fn.isdirectory(data.file) == 1

  if not directory then
    return
  end

  -- change to the directory
  vim.cmd.cd(data.file)

  -- open the tree
  require("nvim-tree.api").tree.open()
end

vim.api.nvim_create_autocmd({ "VimEnter" }, { callback = open_nvim_tree })
vim.keymap.set('n', '<Leader>op', '<cmd>NvimTreeToggle<cr>')

-- Search

require("fzf-lua").setup()

-- Comments
require("nvim_comment").setup({
    operator_mapping = '<Leader>cl',
})

vim.keymap.set('n', '<Leader>fr', '<cmd>FzfLua live_grep<cr>')
vim.keymap.set('n', '<Leader>ff', '<cmd>FzfLua files<cr>')
vim.keymap.set('n', '<Leader>fb', '<cmd>FzfLua buffers<cr>')

-- Other

vim.keymap.set('n', '<ESC>', '<cmd>noh<cr>')

vim.api.nvim_create_autocmd({'VimEnter'}, {
    command = 'hi Normal guibg=NONE ctermbg=NONE',
})

vim.api.nvim_create_autocmd({'VimLeave'}, {
    command = 'call system("xsel -ib", getreg("+"))',
})
