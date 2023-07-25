local M = {}

function M.setup()
        treesitter = require('nvim-treesitter')
        treesitterconf = require('nvim-treesitter.configs')

        treesitterconf.setup {
                ensure_installed = {
                        "c",
                        "lua",
                        "vim",
                        "vimdoc",
                        "query",
                        "java",
                        "javascript",
                        "nix",
                        "python",
                        "rust",
                        "typescript",
                        "html",
                        "go",
                        "bash",
                        "dockerfile",
                },
                auto_install = true,
                highlight = {
                        enable = true,
                }
        }
end

return M
