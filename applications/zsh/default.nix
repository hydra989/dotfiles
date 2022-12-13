{ config, inputs, ... }:
{
  programs.zsh = {
    enable = true;

    dotDir = ".config/zsh";

    initExtra = ''
      (cat ${config.xdg.cacheHome}/wal/sequences &)
    '';

    history = {
      save = 1000;
      size = 1000;
      path = "$HOME/.cache/zsh_history";
    };

    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
      theme = "ys";
    };

    shellAliases = {
      clock = "tty-clock -c -t";
      reboot = "sudo reboot";
      unixporn = "clear && neofetch";
      rescan = "nmcli device wifi rescan";
      quit = "exit";
      resemacs = "systemctl --user restart emacs";
      dots = "cd ~/s/dotfiles";
      fountainpdf = "find . -type f \( -name \"*.fountain\" \) -exec wrap pdf {} \;";
    };
  };
}
