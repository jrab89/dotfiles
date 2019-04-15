#!/bin/bash -eu

mitamae_version="1.7.5"
mitamae_darwin_sha256="89d6de16fe1398281c1d46c28274d3ab14203f41605f9f5c9863773fc0af9ef8"
script_dir=$(dirname "$0")

if ! [ -f "${script_dir}/mitamae" ]; then
    curl -LsS https://github.com/itamae-kitchen/mitamae/releases/download/v${mitamae_version}/mitamae-x86_64-darwin -o /tmp/mitamae
    actual_sha256="$(/usr/bin/openssl dgst -sha256 "/tmp/mitamae" | cut -d" " -f2)"

    if [ "$mitamae_darwin_sha256" != "$actual_sha256" ]; then
        echo "checksum verification failed! \n expected: ${mitamae_darwin_sha256} \n actual: ${actual_sha256}"
        echo "  expected: ${mitamae_darwin_sha256}"
        echo "  actual:   ${actual_sha256}"
        exit 1
    fi

    chmod +x /tmp/mitamae
    mv /tmp/mitamae "$script_dir"
fi

exec "$script_dir/mitamae" local "$script_dir/mac/default.rb"
