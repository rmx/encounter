FROM b.gcr.io/ewa/haskell:v3-gfbc4374-t20151222145545

RUN apt-get update && \
    apt-get install -y libffi6 zlib1g-dev curl build-essential git

ADD . /src
RUN cd src && \
    stack setup && \
    stack install && \
    cp $(stack path --local-bin-path)/rmx-api-server /usr/bin/ && \
    cd / && \
    rm -rf /src

EXPOSE 80
CMD ["/usr/bin/rmx-api-server", "--port", "80"]
