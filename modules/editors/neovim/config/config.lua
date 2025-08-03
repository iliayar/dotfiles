nixcfg = require("config.nix")

vim.g.mapleader = " "

vim.opt.number = true
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

vim.opt.conceallevel = 1

vim.opt.undofile = true

vim.keymap.set("n", "<Esc>", "<Cmd>noh<CR>")

vim.api.nvim_create_augroup("UserGroup", {})

if nixcfg.codeStats.enable then
    require("codestats-nvim").setup({
        token = nixcfg.codeStats.key,
    })
end

if nixcfg.misc.enable then
    require("nvim-surround").setup({})
    require("hop").setup({})
    require("nvim-web-devicons").setup()
    require("gitsigns").setup()
    require("Comment").setup(
        {
            mappings = {
                basic = false,
                extra = false
            }
        }
    )

    local commentApi = require("Comment.api")

    local esc = vim.api.nvim_replace_termcodes("<ESC>", true, false, true)

    vim.keymap.set(
        "x",
        "<Leader>cl",
        function()
            vim.api.nvim_feedkeys(esc, "nx", false)
            commentApi.toggle.linewise(vim.fn.visualmode())
        end
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

    local trouble = require("trouble.sources.telescope")
    local fb_actions = require("telescope").extensions.file_browser.actions
    local actions = require("telescope.actions")

    require("telescope").setup(
        {
            defaults = {
                color_devicons = true,
                mappings = {
                    i = {
                        ["<C-t>"] = trouble.open,
                        ["<C-S-o>"] = require("telescope.actions.layout").toggle_preview
                    },
                    n = {["<C-t>"] = trouble.open}
                },
                layout_strategy = "flex",
                preview = {
                    hide_on_startup = true
                }
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

    require("neogit").setup({})

    local neogit = require("neogit")
    vim.keymap.set(
        "n",
        "<Leader>og",
        function()
            neogit.open()
        end
    )

    require("config.linters").setup()
    local linters_by_ft = {}

    if nixcfg.langProtobuf.enable then
        linters_by_ft["proto"] = {"proto_api_linter"}
    end

    if nixcfg.langPython.enable then
        linters_by_ft["python"] = {"ruff"}
    end

    require("lint").linters_by_ft = linters_by_ft

    vim.api.nvim_create_autocmd(
        {"BufWritePost"},
        {
            callback = function()
                require("lint").try_lint()
            end
        }
    )

    local harpoon_mark = require("harpoon.mark")
    local harpoon_ui = require("harpoon.ui")
    vim.keymap.set("n", "<Leader>hh", harpoon_ui.toggle_quick_menu)
    vim.keymap.set("n", "<Leader>ha", harpoon_mark.add_file)
    vim.keymap.set("n", "<Leader>j", harpoon_ui.nav_next)
    vim.keymap.set("n", "<Leader>k", harpoon_ui.nav_prev)
    for i = 1, 10 do
        local key = ""
        if i == 10 then
            key = "0"
        else
            key = tostring(i)
        end
        vim.keymap.set(
            "n",
            "<Leader>h" .. key,
            function()
                harpoon_ui.nav_file(i)
            end
        )
    end
end

if nixcfg.exp.enable then
    require("nvim-exp").setup()
end

if nixcfg.codeMisc.enable then
    treesitterConfig = {
        highlight = {
            enable = true,
            additional_vim_regex_highlighting = {"markdown"}
        },
        ensure_installed = {}
    }

    if nixcfg.orgmode.enable then
        -- require("orgmode").setup_ts_grammar()
        table.insert(treesitterConfig.highlight.additional_vim_regex_highlighting, "org")
    end

    require("nvim-treesitter.configs").setup(treesitterConfig)
    require("treesitter-context").setup(
        {
            enable = true,
            line_numbers = true,
            max_lines = 5,
            trim_scope = "outer",
            mode = "topline"
        }
    )

    params = {filetype = {}}

    if nixcfg.langNix.enable then
        params.filetype["nix"] = {
            require("formatter.defaults.nixpkgs_fmt")
        }
    end

    if nixcfg.langLua.enable then
        params.filetype["lua"] = {
            require("formatter.filetypes.lua").luafmt
        }
    end

    if nixcfg.langPython.enable then
        params.filetype["python"] = {
            require("formatter.filetypes.python").black
        }
    end

    if nixcfg.langOcaml.enable then
        params.filetype["ocaml"] = {
            require("formatter.filetypes.ocaml").ocamlformat
        }
    end

    if nixcfg.langSql.enable then
        params.filetype["sql"] = {
            require("formatter.filetypes.sql").pgformat
        }
    end

    if nixcfg.langLatex.enable then
        params.filetype["latex"] = {
            require("formatter.filetypes.latex").latexindent
        }
    end

    if nixcfg.langCpp.enable then
        params.filetype["cpp"] = {
            require("formatter.filetypes.cpp").clangformat
        }
    end

    if nixcfg.langProtobuf.enable then
        params.filetype["proto"] = {
            require("formatter.filetypes.proto").buf_format
        }
    end

    if nixcfg.langTypst.enable then
        require("typst-preview").setup({})
    end

    require("formatter").setup(params)

    vim.keymap.set("n", "<C-=>", "<cmd>Format<CR>")
    vim.keymap.set("n", "<Leader>ad", "<cmd>Format<CR>")

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

    cmp_sources = {
        { name = "snippy" },
        { name = "buffer" },
    }

    if nixcfg.lsp.enable then
        table.insert(
            cmp_sources,
            {
                name = "nvim_lsp"
            }
        )
    end

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
            window = {},
            sources = cmp.config.sources(cmp_sources)
        }
    )
end

if nixcfg.prettyGruvbox.enable then
    require("gruvbox").setup(
        {
            transparent_mode = true
        }
    )
    vim.cmd("colorscheme gruvbox")
else
    vim.cmd("colorscheme monokai")
end

vim.api.nvim_create_autocmd(
    {"VimEnter"},
    {
        command = "hi Normal guibg=NONE ctermbg=NONE"
    }
)

if nixcfg.statusBar.enable then
    params = {sections = {lualine_c = {"filename"}}}

    if nixcfg.lsp.enable then
        table.insert(params.sections.lualine_c, "lsp_progress")
    end

    require("lualine").setup(params)
end

if nixcfg.todoComments.enable then
    require("todo-comments").setup({})
end

if nixcfg.lsp.enable then
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

    if nixcfg.langRust.enable then
        vim.lsp.config('rust_analyzer', {
            capabilities = capabilities,
            on_attach = common_on_attach
        })
        vim.lsp.enable('rust_analyzer')
    end

    if nixcfg.langZig.enable then
        vim.lsp.config('zls',{
            capabilities = capabilities,
            on_attach = common_on_attach
        })
        vim.lsp.enable('zls')
    end

    if nixcfg.langTypescript.enable then
        vim.lsp.config('tsserver', {
            capabilities = capabilities,
            on_attach = common_on_attach
        })
        vim.lsp.enable('tsserver')
    end

    if nixcfg.langNix.enable then
        vim.lsp.config('nixd', {
            capabilities = capabilities,
            on_attach = common_on_attach
        })
    end

    if nixcfg.langGo.enable then
        vim.lsp.config('gopls', {
            capabilities = capabilities,
            on_attach = common_on_attach
        })
        vim.lsp.enable('gopls')
    end

    if nixcfg.langPython.enable then
        vim.lsp.config('pyright', {
            capabilities = capabilities,
            on_attach = common_on_attach
        })
    end

    if nixcfg.langOcaml.enable then
        vim.lsp.config('ocamllsp', {
            capabilities = capabilities,
            on_attach = common_on_attach,
        })
    end

    if nixcfg.langCpp.enable then
        cfg = {
            capabilities = capabilities,
            on_attach = common_on_attach
        }

        if nixcfg.langCpp.lsp == "clangd" then
            if nixcfg.langCpp.command then
                cfg.cmd = nixcfg.langCpp.command
            end

            vim.lsp.config('clangd', cfg)
        end

        if nixcfg.langCpp.lsp == "ccls" then
            vim.lsp.config('ccls', cfg)
        end
    end

    if nixcfg.langTypst.enable then
        vim.lsp.config('tinymist', {
            capabilities = capabilities,
            on_attach = common_on_attach
        })
    end

    if nixcfg.langHaskell.enable then
        vim.lsp.config('hls', {
            capabilities = capabilities,
            on_attach = common_on_attach
        })
    end

    if nixcfg.langLean.enable then
        require("lean").setup {
            lsp = {on_attach = common_on_attach},
            mappings = true
        }
    end

    if nixcfg.langCoq.enable then
        -- vim.g.loaded_coqtail = 1
        -- vim.g.coqtail.supported = 0

        -- NOTE: Pretty bad
        -- require("coq-lsp").setup()
    end

    if nixcfg.langFSharp.enable then
        vim.lsp.config('fsautocomplete', {
            capabilities = capabilities,
            on_attach = common_on_attach
        })
    end

    if nixcfg.exp.enable then
        local util = require('lspconfig.util')
        vim.lsp.config('exp', {
            cmd = {'exp-ls'},
            filetypes = {'exp'},
            root_dir = util.root_pattern('.exp'),
            single_file_support = true,

            capabilities = capabilities,
            on_attach = common_on_attach,
        })
    end
end

if nixcfg.obsidian.enable then
    require("obsidian").setup(
        {
            workspaces = {
                {
                    name = "notes",
                    path = nixcfg.obsidian.path
                }
            },
            legacy_commands = false,
        }
    )

    vim.keymap.set("n", "<C-c>nf", "<Cmd>Obsidian quick_switch<CR>")
    vim.keymap.set("n", "<C-c>nr", "<Cmd>Obsidian search<CR>")
end

if nixcfg.orgmode.enable then
    require("orgmode").setup(
        {
            org_agenda_files = {"~/org/**/*"},
            org_default_notes_file = "~/org/Notes.org"
        }
    )
end

vim.api.nvim_create_autocmd(
    "BufReadPost",
    {
        group = "UserGroup",
        callback = function(args)
            local valid_line = vim.fn.line([['"]]) >= 1 and vim.fn.line([['"]]) < vim.fn.line("$")
            local not_commit = vim.b[args.buf].filetype ~= "commit"

            if valid_line and not_commit then
                vim.cmd([[normal! g`"]])
            end
        end
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

if nixcfg.agi.enable then
    local home = vim.fn.expand("$HOME")
    require("chatgpt").setup(
        {
            api_key_cmd = "cat " .. home .. "/.chatgpt-key"
        }
    )
end

if nixcfg.remote.enable then
    require("remote-nvim").setup()
end

if nixcfg.sonicpi.enable then
    require("sonicpi").setup({
        server_dir = "",
    })
end

if nixcfg.strudel.enable then
    local strudel = require("strudel")
    strudel.setup({
        update_on_save = true,
    })

    vim.keymap.set("n", "<Leader>sul", strudel.launch)
    vim.keymap.set("n", "<Leader>suq", strudel.quit)
    vim.keymap.set("n", "<Leader>sut", strudel.toggle)
    vim.keymap.set("n", "<Leader>suu", strudel.update)
    vim.keymap.set("n", "<Leader>sus", strudel.stop)
    vim.keymap.set("n", "<Leader>sub", strudel.set_buffer)
    vim.keymap.set("n", "<Leader>sux", strudel.execute)
end
