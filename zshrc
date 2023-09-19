export PATH=$HOME/bin:/usr/local/bin:$PATH

setopt hist_ignore_all_dups interactivecomments

export TIMEFMT="${TIMEFMT}
space used: %K KB
max memory: %M MB
page faults: %F, %R
context switches: %w, %c"

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

export EDITOR=vim
export SUDO_PROMPT='%p to sudo as %U: '

export LESSHISTFILE=/dev/null

stderred_path='/usr/lib/libstderred.so'

if [[ -f $stderred_path ]]; then
	export LD_PRELOAD="${stderred_path}${LD_PRELOAD:+:$LD_PRELOAD}"
fi

# Activate a virtualenv from anywhere inside it.
vact() {
	local last=""
	local dir="$(pwd)"

	while [[ "$last" != "$dir" ]]; do
		local try="${dir}/bin/activate"

		if [[ -f "$try" ]]; then
			. "$try"

			return
		fi

		last="$dir"
		dir="$(dirname "$dir")"
	done
}

alias :q=exit
alias :Q=exit

alias t=true
alias nil=false

if [[ "$(uname)" == 'Darwin' ]]; then
	# BSD ls
	alias ls='ls -FGh'
else
	# GNU ls
	alias ls='ls --color=auto --classify --human-readable'
fi

# Replace newlines with commas, but leave the final newline as is.
alias comma="tr '\n' , | rev | cut -c 2- | rev"
alias grep='grep --color=auto --line-number'
alias gst='git status --short --branch'
alias mytop='htop -u "${USER}"'
alias pathto="readlink -f"
# Without connection sharing.
alias ssh-fresh='ssh -S none'
alias ta='tmux attach'
alias tree='tree -C'
alias vimpg='vim -R -'
alias vlc='vlc --extraintf oldrc --rc-unix /tmp/vlc.sock'

if [[ -f ~/.zshrc.local ]]; then
	source ~/.zshrc.local
fi
