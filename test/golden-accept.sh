#/usr/bin/env zsh

for f in **/*.prism; do
    if [[ -a "${f}.actual" ]]; then
        echo "Accepting ${f}"
        mv "${f}.actual" "$f"
    fi
done
