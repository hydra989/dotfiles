{ ... }:
{
  programs.zsh = {
    enable = true;

    dotDir = ".config/zsh";

    initExtra = ''
        SOURCEDIR=$HOME/s
    '';

    history = {
        save = 1000;
        size = 1000;
        path = "$HOME/.cache/zsh_history";
    };

    enableAutosuggestions = true;
    syntaxHighlighting.enable = true;
    oh-my-zsh = {
        enable = true;
        plugins = [ "git" ];
        theme = "ys";
    };

    shellAliases = {
        clock = "tty-clock -c -t";
        reboot = "sudo reboot";
        unixporn = "clear && neofetch";
        quit = "exit";
        dots = "cd ~/s/dotfiles";
        dvim = "cd ~/s/dotfiles && vim";
        rebuild = "sudo nixos-rebuild switch";
        ls = "eza --icons";
    };
  };
}
