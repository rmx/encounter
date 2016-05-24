FROM b.gcr.io/ewa/haskell:v3-gfbc4374-t20151222145545

RUN apt-get update && \
    apt-get install -y libffi6 zlib1g-dev curl

ADD . /src
RUN cd src && \
    stack setup && \
    stack install && \
    cp $(stack path --local-bin-path)/web-socket-proxy /usr/bin/ && \
    cd / && \
    rm -rf /src

EXPOSE 80
CMD ["/usr/bin/web-socket-proxy", "--port", "80"]
