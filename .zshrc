export LANG=fr_FR.UTF-8
export LC_ALL=fr_FR.UTF-8

export EDITOR="emacs -nw"
export VISUAL="emacs -nw"

# - ohmyzsh
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="zhann"
plugins=(git)
source $ZSH/oh-my-zsh.sh

# gpg
export GPG_TTY=$(tty)

# - aws
alias aws_get_token="python $HOME/bin/update_aws_token.py"

# - apps
alias vim="nvim"
alias opena="open -a"
alias code="open -a Visual\ Studio\ Code\ -\ Insiders"
alias xcode="open -a Xcode"
alias he="heroku"
alias cleanup="brew update && brew upgrade && brew cleanup"
alias emacs="emacs -nw"
alias htop="btop"
alias code="cursor"
alias simulator="open -a Simulator.app"

# - git
alias hub="hub"
alias diff="git diff -- . ':(exclude)package-lock.json'"
alias co="git checkout"
alias st="git status"
alias gaa="git add ."
alias gbs="git branch | fzf --reverse | xargs git checkout"
alias gbd="git for-each-ref --format '%(refname:short)' refs/heads | grep -v 'master\|main' | fzf --reverse | xargs git branch -D"
alias gbm="git for-each-ref --format '%(refname:short)' refs/heads | fzf --reverse | xargs git merge"
alias git-fetch-all="find . -name .git -type d -execdir git pull -v ';'"
alias ggpull="git pull --rebase"

# - docker
alias dc="docker compose"

# - tmux
alias tn="tmux new -s"
alias tt="tmux attach-session -t"
alias tl="tmux list-session"
alias td="tmux kill-session -t"

# - node
alias npmi="npm i"
command -v fnm &> /dev/null && eval "$(fnm env --use-on-cd)"

# - ruby
alias bi="bundle install"
alias be="bundle exec"
alias bu="bundle update"
command -v rbenv &> /dev/null && eval "$(rbenv init - zsh)"

# - Python
if command -v pyenv &> /dev/null; then
    export PATH="$HOME/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
fi

# - load extra source files
ZSH_SOURCES_DIR="$HOME/.zsh_sources"
if [ -d "$ZSH_SOURCES_DIR" ]; then
    find "$ZSH_SOURCES_DIR" -type f -maxdepth 1 | while read -r file; do
        source "$file"
    done
fi

# My binaries
if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/davidmiotti/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/davidmiotti/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/davidmiotti/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/davidmiotti/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
# Homebrew
export GOROOT="$(brew --prefix golang)/libexec"
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin

# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
[[ ! -r '/Users/davidmiotti/.opam/opam-init/init.zsh' ]] || source '/Users/davidmiotti/.opam/opam-init/init.zsh' > /dev/null 2> /dev/null
# END opam configuration
