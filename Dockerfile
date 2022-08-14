FROM debian:sid-slim
WORKDIR /birdism/server
RUN apt-get -y update \
  && apt-get install -y libpq-dev ca-certificates \
  && apt-get clean \
  && apt-get autoremove \
  && rm -rf /var/lib/apt/lists/*
ARG project_bin
COPY ${project_bin} /birdism/server/
COPY gooey/build /birdism/app
CMD ["/birdism/server/birdism", "serve"]
