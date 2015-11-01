FROM haskell:latest
MAINTAINER Umut Isik "umutisik@gmail.com"

# Add user
RUN groupadd umutcoderunner
RUN useradd -m -d /home/umutcoderunner -g umutcoderunner -s /bin/bash umutcoderunner

# Install libs
RUN cabal update && \
    cabal install --global \
        async \
        attoparsec \
        case-insensitive \
        cgi \
        exceptions \
        fgl \
        hashable \
        haskell-src \
        html \
        HTTP \
        HUnit \
        mtl \
        multipart \
        network \
        network-uri \
        ObjectName \
        old-locale \
        old-time \
        parallel \
        parsec \
        primitive \
        QuickCheck \
        random \
        regex-base \
        regex-compat \
        regex-posix \
        scientific \
        split \
        StateVar \
        stm \
        syb \
        text \
        tf-random \
        transformers \
        transformers-compat \
        unordered-containers \
        vector \
        xhtml \
        zlib \
        repa \
        repa-io

RUN apt-get update && apt-get install -y imagemagick && apt-get clean

# Copy files
#COPY runnerfiles/runandconvert.sh /home/umutcoderunner/runandconvert.sh

USER umutcoderunner
WORKDIR /home/umutcoderunner/

