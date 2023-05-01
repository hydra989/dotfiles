{ ... }: {
  programs.neovim = { enable = true; };

  home.file."/.config/nvim/init.vim" = {
    text = ''
      set runtimepath^=~/.vim runtimepath+=~/.vim/after
      let &packpath = &runtimepath
      source ~/.vimrc
    '';
  };
}
