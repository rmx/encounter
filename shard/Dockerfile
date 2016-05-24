FROM iojs:2.4.0

ADD  . /app
WORKDIR /app

RUN ./script/bootstrap && \
    ./node_modules/.bin/gulp compile && \
    echo "module.exports = '"$(git describe --match=rmx)"';" > dist/build/version.js

CMD ["node", "dist/build/bin/shard"]
