#!/bin/bash -eu

mitamae_version='1.14.0'
mitamae_darwin_sha256='818d47ef5ea9d5404e248ec87d3088d9447d1a6f53b611eb3dea07b53133fcb5'
script_dir=$(dirname "$0")

if ! [ -f "${script_dir}/mitamae" ]; then
    curl -LsS https://github.com/itamae-kitchen/mitamae/releases/download/v${mitamae_version}/mitamae-x86_64-darwin -o /tmp/mitamae
    actual_sha256="$(/usr/bin/openssl dgst -sha256 "/tmp/mitamae" | cut -d" " -f2)"

    if [ "$mitamae_darwin_sha256" != "$actual_sha256" ]; then
        echo 'checksum verification failed!'
        echo "  expected: ${mitamae_darwin_sha256}"
        echo "  actual:   ${actual_sha256}"
        exit 1
    fi

    chmod +x /tmp/mitamae
    mv /tmp/mitamae "$script_dir"
fi

exec "$script_dir/mitamae" "$@"
