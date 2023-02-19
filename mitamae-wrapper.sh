#!/bin/bash -eu

mitamae_version='1.14.0'
mitamae_x86_64_darwin_sha256='818d47ef5ea9d5404e248ec87d3088d9447d1a6f53b611eb3dea07b53133fcb5'
mitamae_aarch64_darwin_sha256='1e531c09ee435f1967dfed408d0b968c1ba294d4b1cb81e1ccb8cff817c7c259'
script_dir=$(dirname "$0")
arch=$(uname -m)

mitamae_asset=$([ "$arch" == x86_64 ] && echo mitamae-x86_64-darwin || echo mitamae-aarch64-darwin)
mitamae_url="https://github.com/itamae-kitchen/mitamae/releases/download/v${mitamae_version}/${mitamae_asset}"
mitamae_sha256=$([ "$arch" == x86_64 ] && echo "$mitamae_x86_64_darwin_sha256" || echo "$mitamae_aarch64_darwin_sha256")

if ! [ -f "${script_dir}/mitamae" ]; then
    curl -LsS "$mitamae_url" -o /tmp/mitamae
    actual_sha256="$(/usr/bin/openssl dgst -sha256 "/tmp/mitamae" | cut -d" " -f2)"

    if [ "$mitamae_sha256" != "$actual_sha256" ]; then
        echo 'checksum verification failed!'
        echo "  expected: ${mitamae_sha256}"
        echo "  actual:   ${actual_sha256}"
        exit 1
    fi

    chmod +x /tmp/mitamae
    mv /tmp/mitamae "$script_dir"
fi

exec "$script_dir/mitamae" "$@"
