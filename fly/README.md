Copy `tryhaskell` binary to `fly/`.

    cp $(stack exec which tryhaskell) fly/

From the git root directory, run:

    docker image build -f fly/Dockerfile . -t ghcr.io/chrisdone/tryhaskell
