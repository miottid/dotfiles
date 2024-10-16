# zshrc
ZSHRC_FILE="${HOME}/.zshrc"
if [ ! -f $ZSHRC_FILE ]; then
    ln -s "${PWD}/.zshrc" $ZSHRC_FILE
fi

# tmux
TMUX_FILE="${HOME}/.tmux.conf"
if [ ! -f $TMUX_FILE ]; then
    ln -s "${PWD}/.tmux.conf" $TMUX_FILE
fi

# wezterm terminal
WEZTERM_FILE="${HOME}/.wezterm.lua"
if [ ! -f $WEZTERM_FILE ]; then
    ln -s "${PWD}/.wezterm.lua" $WEZTERM_FILE
fi

# global .gitignore
GITIGNORE_FILE="${HOME}/.gitignore"
if [ ! -f $GITIGNORE_FILE ]; then
    ln -s "${PWD}/.gitignore" $GITIGNORE_FILE
fi

# emacs config
EMACS_LOCAL_DIR="${HOME}/.emacs.local"
if [ ! -d $EMACS_LOCAL_DIR ]; then
    ln -s "${PWD}/.emacs.local" $EMACS_LOCAL_DIR
fi
EMACS_FILE="${HOME}/.emacs"
if [ ! -f $EMACS_FILE ]; then
    ln -s "${PWD}/.emacs" $EMACS_FILE
fi

# SQLITE
SQLITE_FILE="${HOME}/.sqliterc"
if [ ! -f $SQLITE_FILE ]; then
    ln -s "${PWD}/.sqliterc" $SQLITE_FILE
fi
