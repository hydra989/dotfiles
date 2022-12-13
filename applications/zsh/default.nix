{ inputs, ... }:
{
  programs.zsh = {
    enable = true;

    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
      theme = "ys";
    };
    # currently in .zshrc
    # initExtra = ''
    #   (cat ${config.xdg.cacheHome)/wal/sequences &)
    # '';
  };
}
