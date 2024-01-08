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
        alpha-nvim
        lualine-nvim
        nvim-web-devicons
        nvim-lspconfig
        nvim-treesitter.withAllGrammars
        neogit

        # themes
        nvim-base16

        # language plugins
        vimtex
    ];
  };
}
