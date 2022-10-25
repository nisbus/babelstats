FROM erlang:22.2.2
RUN apt-get update
RUN apt-get install -y \
	apt-transport-https \
	ca-certificates \
	curl \
	gnupg-agent \
	software-properties-common \
	wget


RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
COPY ./rebar.config /.
RUN rebar3 get-deps
RUN rebar3 compile
COPY ./ /.
RUN rebar3 release
RUN sed -i 's|CODE_LOADING_MODE="${CODE_LOADING_MODE:-embedded}"|CODE_LOADING_MODE=""|g' _build/default/rel/babelstats/bin/babelstats

ENTRYPOINT ["./_build/default/rel/babelstats/bin/babelstats"]
