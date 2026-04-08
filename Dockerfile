FROM rocker/r-ver:4.3.3

ENV DEBIAN_FRONTEND=noninteractive
ENV PATH=/usr/local/bin:$PATH

RUN apt-get update && apt-get install -y --no-install-recommends \
    bash \
    build-essential \
    ca-certificates \
    curl \
    git \
    jq \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libssl-dev \
    libtiff5-dev \
    libxml2-dev \
    nodejs \
    npm \
    pandoc \
    ripgrep \
    && rm -rf /var/lib/apt/lists/*

RUN npm install -g @openai/codex

RUN R -q -e "install.packages(c('pak', 'remotes'), repos = 'https://cloud.r-project.org')"

WORKDIR /workspace/ggpaintr

CMD ["/bin/bash"]
