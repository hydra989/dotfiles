# dotfiles

 - **distro**: nixos
 - **wm**: hyprland
 - **bar**: waybar
 - **$EDITOR**: neovim
 - **shell**: zsh + oh-my-zsh
 - **terminal**: alacritty
 - **browser**: firefox

My NixOS & home-manager configurations, managed as a flake.

```
 - .config/
  - hypr/ -> configs for hyprland, hyprpaper
  - neovim/ -> lua sources for neovim configuration
 - .github/ -> configs for automatic flake updates
 - applications/ -> nix configs for various applications
  - alacritty/
  - firefox/
  - neovim/
  - virt-manager/
  - waybar/
  - zsh/
 - hardware/ -> nix configs for different machines, hardware-specific changes
  - nightingale.nix -> my desktop (r5 5600x, RTX 3060)
  - songbird.nix -> my laptop (r5 3500u, vega iGPU)
  - serv.nix -> nextcloud/plex/*arr vm, work-in-progress
 - machines/ -> nix configs for different machines, software-specific changes
  - nightingale.nix
  - songbird.nix
 - packages/ -> custom packages
  - deity.nix -> daemon that emulates emacs' god-mode
 - sh/ -> miscellaneous scripts
 - wallpapers/ -> wallpaper collection, sources listed inside
```
