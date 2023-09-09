#!/bin/sh
# Prepare a release.
# Depends on set -e to exit on failures.
set -e
set -u
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 VERSION"
  exit 1
fi

version="$1"
echo "Using $version"

sed -i "s/^version = .*/version = \"$version\"/" Cargo.toml
cargo fmt
cargo build
git add Cargo.lock Cargo.toml
echo "git commit -m 'Prepare v$version'"
echo "git tag v$version"
echo "git push --tags"
echo "cargo publish --allow-dirty"
