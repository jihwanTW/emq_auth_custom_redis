PROJECT = emq_auth_custom_redis
PROJECT_DESCRIPTION = EMQ auth custom redis
PROJECT_VERSION = 2.3.1

BUILD_DEPS = emqttd cuttlefish eredis emysql
dep_emqttd = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish
dep_eredis = git https://github.com/wooga/eredis
dep_emysql = git https://github.com/Eonblast/Emysql 0.4.0


ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

NO_AUTOPATCH = cuttlefish

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_auth_custom_redis.conf -i priv/emq_auth_custom_redis.schema -d data
