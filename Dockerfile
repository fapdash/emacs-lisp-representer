FROM silex/emacs:29.4-alpine

# TODO(FAP): install emacs package dependencies during Docker image build?

# TODO(FAP): either use jq or don't install
RUN apk add --no-cache bash jq

WORKDIR /opt/representer
COPY . .
ENTRYPOINT ["/opt/representer/bin/run.sh"]
