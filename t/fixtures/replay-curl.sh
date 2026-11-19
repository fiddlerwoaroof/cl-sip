#!/bin/sh
# Stands in for curl in the tests: the Nth run saves the request body as
# $0.body.N and prints $0.out.N.
n=$(cat "$0.count" 2>/dev/null || echo 0)
n=$((n + 1))
echo "$n" > "$0.count"
config=$(cat)
body=$(printf '%s\n' "$config" | sed -n 's/^data-binary = "@\(.*\)"$/\1/p')
cp "$body" "$0.body.$n"
cat "$0.out.$n"
