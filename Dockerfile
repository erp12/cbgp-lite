FROM ubuntu:latest

ENV PYTHON_VERSION 3.11.4

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update

# Use pyenv to install Python, required to run the run launcher script.
RUN apt-get install -y make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget ca-certificates curl llvm libncurses5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev libncursesw5-dev mecab-ipadic-utf8 git
ENV PYENV_ROOT /root/.pyenv
ENV PATH $PYENV_ROOT/shims:$PYENV_ROOT/bin:$PATH
RUN set -ex \
    && curl https://pyenv.run | bash \
    && pyenv update \
    && pyenv install $PYTHON_VERSION \
    && pyenv global $PYTHON_VERSION \
    && pyenv rehash

# Install OpenJDK-8
RUN apt-get install -y openjdk-11-jdk

# Install Clojure
RUN curl -L -O https://github.com/clojure/brew-install/releases/latest/download/posix-install.sh
RUN chmod +x posix-install.sh
RUN ./posix-install.sh

# Include project source
COPY ./deps.edn /usr/src/cbgp-lite/deps.edn
COPY ./scripts /usr/src/cbgp-lite/scripts
COPY ./src /usr/src/cbgp-lite/src
COPY ./benchmarks /usr/src/cbgp-lite/benchmarks

WORKDIR "/usr/src/cbgp-lite"

ENTRYPOINT ["python3", "scripts/local_runner.py" ]
