# http://unix.stackexchange.com/a/9124
mkcd () {
    case "$1" in
        */..|*/../) cd -- "$1";; # that doesn't make any sense unless the directory already exists
        /*/../*) (cd "${1%/../*}/.." && mkdir -p "./${1##*/../}") && cd -- "$1";;
        /*) mkdir -p "$1" && cd "$1";;
        */../*) (cd "./${1%/../*}/.." && mkdir -p "./${1##*/../}") && cd "./$1";;
        ../*) (cd .. && mkdir -p "${1#.}") && cd "$1";;
        *) mkdir -p "./$1" && cd "./$1";;
    esac
}

git-push-origin() {
    git add . && git commit -m -- && git push origin
}

git-update-all() {
    for d in $(find . ! -name '.' -maxdepth 1 -type d) ; do cd "$d" ; git status ; git pull origin ; cd .. ; done
}

hg-update-all() {
    for d in $(find . -type d -maxdepth 1 -not -name '.*') ; do echo $d; cd $d; hg pull; hg update; cd ..; done
}

brew-up() {
    brew update ; brew upgrade ; brew cleanup
}
