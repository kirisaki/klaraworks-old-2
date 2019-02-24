FROM haskell:8.6.3

RUN echo >> ~/.stack/configure.yaml && echo 'system-ghc: true' >> ~/.stack/configure.yaml

RUN stack setup --resolver lts-13.8

RUN curl -sL https://deb.nodesource.com/setup_11.x | bash -  && apt-get install -y nodejs

RUN npm i -g npm
