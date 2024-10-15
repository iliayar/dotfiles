nixcfg = require("config.nix")

vim.opt.clipboard = "unnamedplus"

vim.opt.softtabstop = 4
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.keymap.set("n", "<Esc>", "<Cmd>noh<CR>")

if nixcfg.misc.enable then
    require("nvim-surround").setup({})
end