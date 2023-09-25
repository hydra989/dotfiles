local M = {}

function M.setup()
    -- Indicate first time installation
    local packer_bootstrap = false

    -- packer.nvim configuration
    local conf = {
        display = {
            open_fn = function()
                return require("packer.util").float { border = "rounded" }
            end,
        },
    }

    -- Check if packer.nvim is installed
    -- Run PackerCompile if there are changes in this file
    local function packer_init()
        local fn = vim.fn
        local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
        if fn.empty(fn.glob(install_path)) > 0 then
            packer_bootstrap = fn.system {
                "git",
                "clone",
                "--depth",
                "1",
                "https://github.com/wbthomason/packer.nvim",
                install_path,
            }
            vim.cmd [[packadd packer.nvim]]
        end
        vim.cmd "autocmd BufWritePost plugins.lua source <afile> | PackerCompile"
    end

    -- Plugins
    local function plugins(use)
        use { "wbthomason/packer.nvim" }

        use { "roxma/vim-tmux-clipboard" }

        use { "ms-jpq/coq_nvim" }
        use { "ms-jpq/chadtree" }

        -- snippets
        use { "L3MON4D3/LuaSnip" }
        -- use { "ms-jpq/coq.artifacts" }

        -- telescope
        use { "nvim-telescope/telescope.nvim" }
        use { "xiyaowong/telescope-emoji.nvim" }

        -- appearance
        use { "goolord/alpha-nvim" }
        use {
            'nvim-lualine/lualine.nvim',
            requires = { 'nvim-tree/nvim-web-devicons', opt = true }
        }

        -- lsp
        use { "neovim/nvim-lspconfig" }

        -- languages
        use {
            'nvim-treesitter/nvim-treesitter',
            run = function()
                local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
                ts_update()
            end,
        }

        -- magit replacement
        use {
            "TimUntersberger/neogit",
            requires = "nvim-lua/plenary.nvim",
        }

        -- themes
        use { "nyoom-engineering/oxocarbon.nvim" }
        use { "EdenEast/nightfox.nvim" }
        use { "vimoxide/vim-cinnabar" }
        use {'ray-x/starry.nvim', setup = function() 
        vim.g.starry_deep_black = true
        end}

        if packer_bootstrap then
            print "Restart Neovim required after installation!"
            require("packer").sync()
        end
    end

    packer_init()

    local packer = require "packer"
    packer.init(conf)
    packer.startup(plugins)
end

return M
