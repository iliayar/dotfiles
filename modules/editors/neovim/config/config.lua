nixcfg = require('config.nix')

if nixcfg.codeStats.enable then
  vim.env.CODESTATS_API_KEY = nixcfg.codeStats.key
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

-- tree-sitter

require('nvim-treesitter.configs').setup({
    highlight = {
        enable = true,
    },
})

-- icons

require("nvim-web-devicons").setup()

-- Tree

if nixcfg.tree.enable then
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
end

-- Search

if nixcfg.search then
    require("telescope").setup()
    require("telescope").load_extension('fzf')
    local builtin = require("telescope.builtin")

    vim.keymap.set('n', '<Leader>fr', builtin.live_grep, {})
    vim.keymap.set('n', '<Leader>fg', builtin.find_files, {})
    vim.keymap.set('n', '<Leader>bf', builtin.buffers, {})
end

-- Comments
require("nvim_comment").setup({
    operator_mapping = '<Leader>cl',
})

-- Snippets

if nixcfg.lsp.enable then
    snippy = require('snippy')
    snippy.setup({
        mappings = {
            is = {
                ['<Tab>'] = 'expand_or_advance',
            },
        },
    })
end
-- Completion

if nixcfg.lsp.enable then
    local cmp = require('cmp')
    cmp.setup({
        snippet = {
            expand = function(args)
                snippy.expand_snippet(args.body)
            end
        },
        mapping = cmp.mapping.preset.insert({
            ['<S-Tab>'] = cmp.mapping.confirm({ select = true }),
        }),
        window = {
            documentation = cmp.config.disable,
        },
        sources = cmp.config.sources({
            { name = 'nvim_lsp' },
        }),
    })
    local capabilities = require('cmp_nvim_lsp').default_capabilities()

    vim.keymap.set('n', '<Leader>ca', '<cmd>lua vim.lsp.buf.code_action()<cr>')
end

-- LSP

if nixcfg.lsp.enable then
    local lspconfig = require('lspconfig')
    lspconfig.rust_analyzer.setup({
        autostart = false,
        capabilities = capabilities
    })
end

-- Other

vim.keymap.set('n', '<ESC>', '<cmd>noh<cr>')

vim.api.nvim_create_autocmd({'VimEnter'}, {
    command = 'hi Normal guibg=NONE ctermbg=NONE',
})

if nixcfg.linux and false then
    -- Disable for a while. Using wayland(
    vim.api.nvim_create_autocmd({'VimLeave'}, {
        command = 'call system("xsel -ib", getreg("+"))',
    })
end
