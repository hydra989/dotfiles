#!/bin/sh

# empty_trash.sh
# deletes the contents of ~/.local/share/Trash.
# *don't* run as sudo.

sudo rm -rf $HOME/.local/share/Trash/files/{*,.*}
sudo rm -rf $HOME/.local/share/Trash/info/{*,.*}
echo "Trash emptied."
