nixcfg = require("config.nix")

if nixcfg.codeStats.enable then
    vim.env.CODESTATS_API_KEY = nixcfg.codeStats.key
end

vim.g.mapleader = " "

vim.opt.rnu = true
vim.opt.clipboard = "unnamedplus"
vim.opt.formatoptions = {n = true, j = true, t = true}

vim.opt.softtabstop = 4
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.listchars["tab"] = "→"

vim.opt.scrolloff = 10

vim.opt.termguicolors = true

if nixcfg.misc.enable then
    require("nvim-surround").setup({})
    require("hop").setup({})
    require("nvim-web-devicons").setup()
    require("gitsigns").setup()
    require("window-picker").setup(
        {
            hint = "statusline-winbar"
        }
    )

    local hop = require("hop")

    vim.keymap.set(
        "",
        "<Leader>gs",
        function()
            hop.hint_patterns({})
        end,
        {}
    )

    vim.keymap.set(
        "",
        "<Leader>gl",
        function()
            hop.hint_lines({})
        end,
        {}
    )

    vim.keymap.set(
        "",
        "<Leader>ww",
        function()
            require("window-picker").pick_window()
        end
    )

    vim.keymap.set(
        "",
        "<Leader>wd",
        function()
            local winid = vim.api.nvim_get_current_win()
            vim.api.nvim_win_close(winid, false)
        end
    )
end

-- tree-sitter

if nixcfg.codeMisc.enable then
    require("nvim-treesitter.configs").setup(
        {
            highlight = {
                enable = true
            }
        }
    )
    require("treesitter-context").setup(
        {
            enable = true,
            line_numbers = true,
            max_lines = 5,
            trim_scope = "outer",
            mode = "topline"
        }
    )

    require("formatter").setup(
        {
            filetype = {
                nix = {
                    require("formatter.defaults.nixfmt")
                },
                lua = {
                    require("formatter.filetypes.lua").luafmt
                },
                python = {
                    require("formatter.filetypes.python").black
                }
            }
        }
    )

    vim.keymap.set("n", "<C-=>", "<cmd>Format<CR>")
end

-- visual

if nixcfg.prettyGruvbox then
    require("gruvbox").setup(
        {
            transparent_mode = true
        }
    )
    vim.cmd("colorscheme gruvbox")
else
    vim.cmd("colorscheme monokai")
end

if nixcfg.statusBar then
    require("lualine").setup(
        {
            sections = {
                lualine_c = {
                    "lsp_progress"
                }
            }
        }
    )
end

-- Tree

if nixcfg.tree.enable then
    require("nvim-tree").setup(
        {
            disable_netrw = true,
            hijack_netrw = true,
            hijack_directories = {
                enable = true,
                auto_open = true
            }
        }
    )

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

    vim.api.nvim_create_autocmd({"VimEnter"}, {callback = open_nvim_tree})
    vim.keymap.set("n", "<Leader>op", "<Cmd>NvimTreeToggle<CR>")
end

-- Search

if nixcfg.search.enable then
    local trouble = require("trouble.providers.telescope")
    local fb_actions = require("telescope").extensions.file_browser.actions
    local actions = require("telescope.actions")

    require("telescope").setup(
        {
            defaults = {
                color_devicons = true,
                mappings = {
                    i = {["<C-t>"] = trouble.open_with_trouble},
                    n = {["<C-t>"] = trouble.open_with_trouble}
                },
                layout_strategy = "flex"
            },
            pickers = {
                buffers = {
                    mappings = {
                        i = {
                            ["<C-d>"] = actions.delete_buffer + actions.move_to_top
                        }
                    }
                }
            },
            extensions = {
                ["ui-select"] = {
                    require("telescope.themes").get_dropdown({})
                }
            }
        }
    )

    require("telescope").load_extension("fzf")
    require("telescope").load_extension("file_browser")
    require("telescope").load_extension("ui-select")

    local themes = require("telescope.themes")
    local builtin = require("telescope.builtin")

    vim.keymap.set(
        "n",
        "<Leader>ff",
        function()
            require("telescope").extensions.file_browser.file_browser(
                {
                    cwd_to_path = true,
                    path = "%:p:h"
                }
            )
        end,
        {}
    )
    vim.keymap.set("n", "<Leader>fr", builtin.live_grep, {})
    vim.keymap.set("n", "<Leader>fg", builtin.find_files, {})
    vim.keymap.set(
        "n",
        "<Leader>bf",
        function()
            builtin.buffers(
                {
                    sort_lastused = true,
                    ignore_current_buffer = true
                }
            )
        end,
        {}
    )

    vim.keymap.set(
        "n",
        "<Leader>ot",
        function()
            require("trouble").open()
        end
    )
end

-- Comments
require("nvim_comment").setup(
    {
        operator_mapping = "<Leader>cl"
    }
)
-- LSP

if nixcfg.lsp.enable then
    require("snippy").setup(
        {
            mappings = {
                is = {
                    ["<Tab>"] = "expand_or_advance"
                }
            }
        }
    )

    local cmp = require("cmp")
    local snippy = require("snippy")
    cmp.setup(
        {
            snippet = {
                expand = function(args)
                    snippy.expand_snippet(args.body)
                end
            },
            mapping = cmp.mapping.preset.insert(
                {
                    ["<S-Tab>"] = cmp.mapping.confirm({select = true})
                }
            ),
            window = {
                documentation = cmp.config.disable
            },
            sources = cmp.config.sources(
                {
                    {name = "nvim_lsp"}
                }
            )
        }
    )

    local trouble = require("trouble")
    local builtin = require("telescope.builtin")

    vim.keymap.set("n", "<Leader>sl", "<Cmd>LspStart<CR>")
    vim.keymap.set("n", "<Leader>ss", "<Cmd>LspStop<CR>")
    vim.keymap.set("n", "<Leader>sr", "<Cmd>LspRestart<CR>")

    vim.keymap.set({"n", "i"}, "<C-e>", vim.diagnostic.open_float)
    vim.keymap.set(
        "n",
        "<Leader>ste",
        function()
            trouble.open("workspace_diagnostics")
        end
    )
    vim.keymap.set(
        "n",
        "<Leader>stl",
        function()
            trouble.open("loclist")
        end
    )
    vim.keymap.set(
        "n",
        "<Leader>se",
        function()
            builtin.diagnostics({})
        end
    )

    local common_on_attach = function(client, bufnr)
        local sr = client.server_capabilities

        vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"

        local opts = {buffer = bufnr}
        vim.keymap.set("n", "gsD", vim.lsp.buf.declaration, opts)
        vim.keymap.set("n", "gsd", builtin.lsp_definitions, opts)
        vim.keymap.set("n", "gsi", builtin.lsp_implementations, opts)
        vim.keymap.set("n", "gsx", builtin.lsp_references, opts)
        vim.keymap.set("n", "gst", builtin.lsp_type_definitions, opts)
        vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
        vim.keymap.set("n", "<Leader>cr", vim.lsp.buf.rename, opts)
        vim.keymap.set({"n", "v"}, "<Leader>ca", vim.lsp.buf.code_action, opts)
        vim.keymap.set({"n", "i"}, "<C-k>", vim.lsp.buf.signature_help, opts)
        vim.keymap.set("n", "<Leader>swa", vim.lsp.buf.add_workspace_folder, opts)
        vim.keymap.set("n", "<Leader>swr", vim.lsp.buf.remove_workspace_folder, opts)
        vim.keymap.set(
            "n",
            "<Leader>swl",
            function()
                print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
            end,
            opts
        )

        if sr.documentFormattingProvider then
            vim.keymap.set(
                "n",
                "<C-=>",
                function()
                    vim.lsp.buf.format {async = true}
                end,
                opts
            )
        end
    end

    local capabilities = require("cmp_nvim_lsp").default_capabilities()

    local lspconfig = require("lspconfig")
    lspconfig.rust_analyzer.setup(
        {
            capabilities = capabilities,
            on_attach = common_on_attach
        }
    )
    lspconfig.nixd.setup(
        {
            autostart = false,
            capabilities = capabilities,
            on_attach = common_on_attach
        }
    )
    lspconfig.gopls.setup(
        {
            capabilities = capabilities,
            on_attach = common_on_attach
        }
    )
    lspconfig.pyright.setup(
        {
            autostart = false,
            capabilities = capabilities,
            on_attach = common_on_attach
        }
    )
end

-- Other

if nixcfg.neogit.enable then
    require("neogit").setup({})

    local neogit = require("neogit")
    vim.keymap.set(
        "n",
        "<Leader>og",
        function()
            neogit.open()
        end
    )
end

vim.keymap.set("n", "<Esc>", "<Cmd>noh<CR>")

vim.api.nvim_create_autocmd(
    {"VimEnter"},
    {
        command = "hi Normal guibg=NONE ctermbg=NONE"
    }
)

if nixcfg.linux and false then
    -- Disable for a while. Using wayland(
    vim.api.nvim_create_autocmd(
        {"VimLeave"},
        {
            command = 'call system("xsel -ib", getreg("+"))'
        }
    )
end
