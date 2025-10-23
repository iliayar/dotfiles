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
    require("git-conflict").setup()
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
        "<Leader>wd",
        function()
            local winid = vim.api.nvim_get_current_win()
            vim.api.nvim_win_close(winid, false)
        end
    )

    if nixcfg.picker.t == "telescope" then
        local telescope = require("telescope")
        local trouble = require("trouble.sources.telescope")
        local fb_actions = telescope.extensions.file_browser.actions
        local actions = require("telescope.actions")
        local lga_actions = require("telescope-live-grep-args.actions")

        telescope.setup(
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
                    },
                    ["live_grep_args"] = {
                      auto_quoting = true,
                      mappings = {
                        i = {
                          ["<C-k>"] = lga_actions.quote_prompt(),
                          ["<C-i>"] = lga_actions.quote_prompt({ postfix = " --iglob " }),
                          ["<C-space>"] = lga_actions.to_fuzzy_refine,
                        },
                      },
                    },
                }
            }
        )

        telescope.load_extension("fzf")
        telescope.load_extension("file_browser")
        telescope.load_extension("ui-select")
        telescope.load_extension("lines")
        telescope.load_extension("live_grep_args")

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
        vim.keymap.set(
            "n",
            "<Leader>fr",
            function()
                telescope.extensions.live_grep_args.live_grep_args()
            end,
            {})
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
        vim.keymap.set("n", "<Leader>fl", function()
            require("telescope").extensions.lines.lines()
        end, {})
    elseif nixcfg.picker.t == "snacks" then
        local trouble = require("trouble.sources.snacks")
        local yazi = require("yazi")
        local Snacks = require("snacks")
        Snacks.setup({
            picker = {
                actions = trouble.actions,
                win = {
                    input = {
                        keys = {
                            ["<C-t>"] = trouble.open
                        }
                    }
                },
                layout = { 
                    preset = "ivy",
                    preview = "main",
                },
                ui_select = true,
            }
        })

        yazi.setup({
        })

        vim.keymap.set("n", "<Leader>ff", function()
            yazi.yazi()
        end, {})
        vim.keymap.set("n", "<Leader>fF", function()
            yazi.yazi(nil, vim.fn.getcwd())
        end, {})
        vim.keymap.set("n", "<Leader>fr", Snacks.picker.grep, {})
        vim.keymap.set("n", "<Leader>fg", Snacks.picker.files, {})
        vim.keymap.set("n", "<Leader>bf", function()
            Snacks.picker.buffers({
                current = false,
            })
        end, {})
    end

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

if nixcfg.langCangjie.enable then
    require("cangjie").setup()
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
        params = {
            dependencies_bin = {
                ["tinymist"] = "tinymist",
            },
        }
        if nixcfg.langTypst.remote then
            params.no_open = true
            params.host = nixcfg.langTypst.host;
            params.data_plane_port = nixcfg.langTypst.dataPlanePort;
            params.control_plane_port = nixcfg.langTypst.controlPlanePort;
        end
        require("typst-preview").setup(params)
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
elseif nixcfg.prettyAlabaster.enable then
    vim.cmd("colorscheme alabaster")
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

    local picker = nil
    if nixcfg.picker.t == "telescope" then
        picker = require("telescope.builtin")
    elseif nixcfg.picker.t == "snacks" then
        picker = require("snacks").picker
    end

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
            picker.diagnostics({})
        end
    )

    local on_attach = function(client, bufnr)
        local sr = client.server_capabilities

        vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"

        local opts = {buffer = bufnr}
        vim.keymap.set("n", "gsD", vim.lsp.buf.declaration, opts)
        vim.keymap.set("n", "gsd", picker.lsp_definitions, opts)
        vim.keymap.set("n", "gsi", picker.lsp_implementations, opts)
        vim.keymap.set("n", "gsx", picker.lsp_references, opts)
        vim.keymap.set("n", "gst", picker.lsp_type_definitions, opts)
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
            vim.keymap.set(
                "v",
                "<C-=>",
                function()
                    local start_pos = vim.api.nvim_buf_get_mark(0, "<")
                    local end_pos = vim.api.nvim_buf_get_mark(0, ">")
                    vim.lsp.buf.format {
                        range = { ["start"] = start_pos, ["end"] = end_pos }
                    }
                end,
                opts
            )
        end
    end
    vim.api.nvim_create_autocmd('LspAttach', {
        callback = function(args)
            local client = vim.lsp.get_client_by_id(args.data.client_id)
            local bufnr = args.buf
            on_attach(client, bufnr)
        end
    })

    local on_init = function(client, _)
        if nixcfg.prettyAlabaster.enable then
            -- FIXME(iliayar): Remove when support for semantic tokens added to theme
            client.server_capabilities.semanticTokensProvider = nil
        end
    end

    vim.lsp.config('*', {
        capabilities = require("cmp_nvim_lsp").default_capabilities(),
        on_init = on_init,
    })

    if nixcfg.langRust.enable then
        vim.lsp.config('rust_analyzer', {})
        vim.lsp.enable('rust_analyzer')
    end

    if nixcfg.langZig.enable then
        vim.lsp.config('zls', {})
        vim.lsp.enable('zls')
    end

    if nixcfg.langTypescript.enable then
        vim.lsp.config('tsserver', {})
        vim.lsp.enable('tsserver')
    end

    if nixcfg.langNix.enable then
        vim.lsp.config('nixd', {})
    end

    if nixcfg.langGo.enable then
        vim.lsp.config('gopls', {})
        vim.lsp.enable('gopls')
    end

    if nixcfg.langPython.enable then
        vim.lsp.config('pyright', {})
    end

    if nixcfg.langOcaml.enable then
        vim.lsp.config('ocamllsp', {})
    end

    if nixcfg.langCpp.enable then
        cfg = {}

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
        vim.lsp.config('tinymist', {})
    end

    if nixcfg.langHaskell.enable then
        vim.lsp.config('hls', {})
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
        vim.lsp.config('fsautocomplete', {})
    end

    if nixcfg.exp.enable then
        local util = require('lspconfig.util')
        vim.lsp.config('exp', {
            cmd = {'exp-ls'},
            filetypes = {'exp'},
            root_dir = util.root_pattern('.exp'),
            single_file_support = true,
        })
    end

    if nixcfg.langCangjie.enable then
        vim.lsp.config('cangjie-lsp', {})
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

if nixcfg.debugger.enable then
    local dap = require('dap')
    dap.adapters.lldb = {
        type = 'executable',
        command = 'lldb-dap',
        name = 'lldb',
    }

    vim.keymap.set("n", "<Leader>db", dap.toggle_breakpoint)
    vim.keymap.set("n", "<Leader>dB", function()
        local condition = vim.fn.input("Condition: ")
        dap.toggle_breakpoint(condition)
    end)
    vim.keymap.set("n", "<Leader>od", dap.repl.open)
    vim.keymap.set("n", "<Leader>dc", dap.continue)
    vim.keymap.set("n", "<Leader>dn", dap.step_over)
    vim.keymap.set("n", "<Leader>ds", dap.step_into)
    vim.keymap.set("n", "<Leader>df", dap.step_out)
    vim.keymap.set("n", "<Leader>dk", dap.terminate)

    if nixcfg.langCpp.enable then
        cjcConfiguration = {
            name = 'cjc',
            type = 'lldb',
            request = 'launch',
            program = function()
                local cangjieHome = os.getenv('CANGJIE_HOME')
                if not cangjieHome then
                    return 'cjc'
                end

                local binDir = cangjieHome .. "/bin/"
                local cjcPath = binDir .. 'cjc'

                if vim.fn.filereadable(binDir .. '.cjc-wrapped') then
                    -- cjc is wrapped for nix compatibility
                    cjcPath = binDir .. '.cjc-wrapped'
                end

                return cjcPath
            end,
            cwd = function()
                local dir = vim.fn.getcwd()
                local cjRoot = os.getenv('CJ_ROOT')
                if cjRoot then
                    local argsFile = cjRoot .. '/playground/debug_path'
                    dir = vim.fn.readfile(argsFile)[1]
                end
                return dir
            end,
            stopOnEntry = false,
            args = function()
                local args = {}

                local nixArgs = os.getenv('NIX_CJC_ARGS')
                if not nixArgs then
                    for _, arg in ipairs(vim.split(nixArgs, ' +')) do
                        table.insert(args, arg)
                    end
                end

                local cjRoot = os.getenv('CJ_ROOT')
                local additionalArgs = ''
                if cjRoot then
                    local argsFile = cjRoot .. '/playground/debug_args'
                    additionalArgs = vim.fn.readfile(argsFile)[1]
                else
                    additionalArgs = vim.fn.input("Args: ")
                end

                for _, arg in ipairs(vim.split(additionalArgs, ' +')) do
                    table.insert(args, arg)
                end

                return args
            end,
        }
        dap.configurations.cpp = {
            cjcConfiguration
        }
    end

    require('nvim-dap-virtual-text').setup({
        enabled = false,
        enabled_commands = true,
    })
    vim.keymap.set("n", "<Leader>dl", "<Cmd>DapVirtualTextToggle<CR>")
end
