FROM ubuntu:14.04

# Get the wai-crowd executable
RUN apt-get install wget libgmp10 -y
RUN wget https://s3.amazonaws.com/stackage-travis/wai-crowd/wai-crowd.bz2 && \
    bunzip2 wai-crowd.bz2 && \
    chmod +x wai-crowd && \
    mv wai-crowd /usr/local/bin

# docroot for static warp server
RUN mkdir /var/www
VOLUME /var/www

# WAI Crowd proxy
EXPOSE 3000

ENTRYPOINT ["/usr/local/bin/wai-crowd"]
CMD ["-p 3000", "--crowd-root $CROWD_ROOT"]
