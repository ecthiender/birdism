FROM debian:bullseye-slim
WORKDIR /birdism/server
RUN apt-get -y update \
  && apt-get install -y libpq-dev \
  && apt-get clean \
  && apt-get autoremove \
  && rm -rf /var/lib/apt/lists/*
ARG project_bin
COPY ${project_bin} /birdism/server/
COPY app /birdism/app
CMD ["/birdism/server/birdism", "serve"]
