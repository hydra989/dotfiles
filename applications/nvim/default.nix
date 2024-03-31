{ pkgs, ... }:
{
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
        nvim-cmp
        cmp-nvim-lsp
        cmp-nvim-ultisnips

        leap-nvim
        ultisnips
        telescope-nvim
        lualine-nvim
        nvim-web-devicons
        nvim-lspconfig

        # themes
        base16-nvim

        # language plugins
        vimtex
        vim-nix
    ];
  };
}
