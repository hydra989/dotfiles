local M = {}

function M.setup()
    local lspconfig = require('lspconfig')

    local cmp = require('cmp')
    local capabilities = require('cmp_nvim_lsp').default_capabilities()
    local cmp_ultisnips_mappings = require('cmp_nvim_ultisnips.mappings')

    -- cmp setup

    cmp.setup({
        snippet = {
            expand = function(args)
                vim.fn["UltiSnips#Anon"](args.body)
            end,
        },
        mapping = {
            ['<C-b>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),
            ['<C-Space>'] = cmp.mapping.complete(),
            ['<C-e>'] = cmp.mapping.abort(),
            ['<CR>'] = cmp.mapping.confirm({ select = true }),
            ["<Tab>"] = cmp.mapping(function(fallback)
                    cmp_ultisnips_mappings.expand_or_jump_forwards(fallback)
                end,
                { 'i', 's' }),
            ['<S-tab>'] = cmp.mapping(function(fallback)
               if cmp.visible() then
                   cmp.select_prev_item()
               else
                   cmp_ultisnips_mappings.jump_backwards(fallback)
               end
               end, { 'i', 's' }),
        },
        sources = cmp.config.sources({
            { name = "ultisnips" },
            { name = 'nvim_lsp' },
        }, {
            { name = 'buffer' },
        }),
    })

    -- lsp setup

    lspconfig.clangd.setup { capabilities = capabilities }
    lspconfig.gopls.setup { capabilities = capabilities }
    lspconfig.jdtls.setup { capabilities = capabilities }

    lspconfig.nil_ls.setup {
        settings = {
            ['nil'] = {
                testSetting = 42,
                formatting = {
                    command = { "nixpkgs-fmt" },
                },
            },
        },
        capabilities = capablities
    }

    lspconfig.pylsp.setup {
        settings = {
            pylsp = {
                plugins = {
                    pylint = {
                        enabled = true,
                    },
                    flake8 = {
                        enabled = true,
                    },
                    rope_completion = {
                        enabled = true,
                    },
                },
            },
        },
        capabilities = capablities
    }

    lspconfig.lua_ls.setup {
        settings = {
            Lua = {
                diagnostics = {
                    globals = {'vim'},
                },
            },
        },
        capabilities = capablities
    }

    vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
    vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
    vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

    vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
            vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

            -- Buffer local mappings.
            -- See `:help vim.lsp.*` for documentation on any of the below functions
            local opts = { buffer = ev.buf }
            vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
            vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
            vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
            vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
            vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
            vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
            vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
            vim.keymap.set('n', '<space>wl', function()
                print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
            end, opts)
            vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
            vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
            vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
            vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
            vim.keymap.set('n', '<space>f', function()
                vim.lsp.buf.format { async = true }
            end, opts)
        end,
    })
end

return M
