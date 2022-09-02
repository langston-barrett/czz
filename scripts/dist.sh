#!/usr/bin/env bash
set -Eeuo pipefail

if [[ -n "${CI:-}" ]]; then
  set -x
fi

if [[ -z "${1:-}" ]]; then
  printf "Usage: %s BINARY_NAME\n" "${0}"
  exit 1
fi

# Bundle czz for distribution.
#
# Arguments:
#
# - Positional:
#   - $1: Binary to package
# - Environment:
#   - $OS_TAG: Linux, macOS, or Windows

bin="${1}"
os="${OS_TAG:-Linux}"

# https://stackoverflow.com/q/4774054/
path_to_script="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd "${path_to_script}/.."

ver=$(grep '^version' "${bin}/${bin}.cabal" | awk '{print $2}')
name="${bin}-v${ver}-${os}-x86_64"
exe="$(cabal list-bin "exe:${bin}" | tail -1)"
mkdir -p "${name}/bin"
cp --dereference --force "${exe}" "${name}/bin"
exe="$(basename "${exe}")"
chmod +x "${name}/bin/${exe}"
strip "${name}/bin/${exe}"
tar -czf "${name}.tar.gz" "${name}"
rm -rf "${name}"
