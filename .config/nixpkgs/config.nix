# nix was throwing errors with nix-shell
# about usage of non-free packages. this
# alleviated that, although i personally
# think it's a kind of ugly solution.

{
  allowUnfree = true;
}
