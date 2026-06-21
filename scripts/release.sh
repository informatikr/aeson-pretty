#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

rm -rf dist-newstyle
rm -rf .ghc.env*

cabal update
cabal sdist
cabal check
cabal v2-haddock lib:aeson-pretty \
  --with-compiler ghc-9.14.1 \
  --haddock-for-hackage \
  --haddock-hyperlinked-source \
  --enable-documentation

TOKEN=${TOKEN:-token-here}
SDIST=$(find . -name 'aeson-pretty-*.tar.gz' | grep -v docs)
DDIST=$(find . -name 'aeson-pretty-*.tar.gz' | grep docs)

echo "To publish a release candidate, run:"
echo "  cabal upload --username=martijnbastiaan --token=${TOKEN} ${SDIST}"
echo "  cabal upload --documentation --username=martijnbastiaan --token=${TOKEN} ${DDIST}"
echo ""
echo "To make a release, run:"
echo "  cabal upload --publish --username=martijnbastiaan --token=${TOKEN} ${SDIST}"
echo "  cabal upload --publish --documentation --username=martijnbastiaan --token=${TOKEN} ${DDIST}"
