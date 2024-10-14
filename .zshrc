export EDITOR="emacs -nw"
export VISUAL="emacs -nw"

# - Oh my zsh
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="zhann"
plugins=(git)

source $ZSH/oh-my-zsh.sh

# GPG
export GPG_TTY=$(tty)

# Aliases

# - AWS
alias aws_get_token="python $HOME/bin/update_aws_token.py"

# - Apps
alias vim="nvim"
alias opena="open -a"
alias code="open -a Visual\ Studio\ Code\ -\ Insiders"
alias xcode="open -a Xcode"
alias he="heroku"
alias cleanup="brew update && brew upgrade && brew cleanup"
alias emacs="emacs -nw"

# - Git
alias git="hub"
alias diff="git diff -- . ':(exclude)package-lock.json'"
alias co="git checkout"
alias st="git status"
alias gaa="git add ."
alias gbd="git for-each-ref --format '%(refname:short)' refs/heads | grep -v \"master\\|main\" | xargs git branch -D"
alias git-fetch-all="find . -name .git -type d -execdir git pull -v ';'"
alias ggpull="git pull --rebase"

# - Docker
alias dc="docker compose"

# - Tmux
alias tn="tmux new -s"
alias tt="tmux attach-session -t"
alias tl="tmux list-session"
alias td="tmux kill-session -t"

# - npm
alias npmi="npm i"

# - ruby
alias bi="bundle install"
alias be="bundle exec"
alias bu="bundle update"

# - fnm
eval "$(fnm env --use-on-cd)"

# My binaries
PATH=$HOME/bin:$PATH

# - LLVM
PATH="$(brew --prefix)/opt/llvm/bin:$PATH"

# - Python
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# - rbenv
eval "$(rbenv init - zsh)"

# Export sensitive keys
source $HOME/.zsh_sensitive_exports

