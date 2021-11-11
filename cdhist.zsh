alias cdp='cdh_print'
alias cds='cdh_goto'

zmodload zsh/mapfile

function chpwd() {
	emulate -L zsh
	cdh_add
}

function cdh_print() {
	hist=( "${(f)mapfile[${HOME}/.cdhist]}" )
	i=0
	for dir in $hist; do
		echo "$i: $dir"
		i=$((i+1))
	done
}

function cdh_goto() {
	if [ $# == 0 ]; then
		ARG=0;
	else
		ARG=${1};
	fi
	hist=( "${(f)mapfile[${HOME}/.cdhist]}" )
	cd ${~hist[$(($ARG+1))]}
}

function cdh_add() {
	if [ $PWD != $HOME ]; then
		hist=( "${(f)mapfile[${HOME}/.cdhist]}" )
		hist=( "$(print -P %~)" $hist[1,9] )
		print -rl -- $hist > "${HOME}/.cdhist"
	fi
}
