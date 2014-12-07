FROM haskell:7.8

RUN cabal update

ADD ./hdose.cabal /opt/hdose/hdose.cabal
RUN cd /opt/hdose && cabal install --only-dep -j4

ADD ./ /opt/hdose
RUN rm -f /opt/hdose/cabal.sandbox.config
RUN cd /opt/hdose && cabal install

ENV PATH /root/.cabal/bin:$PATH

WORKDIR /opt/hdose
