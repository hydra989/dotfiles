{ pkgs, ... }:
{
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;

    plugins = with pkgs.vimPlugins; [
        coq_nvim
        coq-artifacts
        leap-nvim
        luasnip
        telescope-nvim
        alpha-nvim
        lualine-nvim
        nvim-web-devicons
        nvim-lspconfig
        nvim-treesitter.withAllGrammars
        neogit

        # themes
        nvim-base16
    ];
  };
}
