FROM ubuntu:14.04

RUN apt-get install wget libgmp10 -y && \
    wget https://s3.amazonaws.com/stackage-travis/wai-crowd/wai-crowd.bz2 && \
    bunzip2 wai-crowd.bz2 && \
    chmod +x wai-crowd && \
    mv wai-crowd /usr/local/bin && \
    mkdir -p /var/www

ENTRYPOINT ["/usr/local/bin/wai-crowd"]
