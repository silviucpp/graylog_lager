graylog_lager
================

[![Build Status](https://travis-ci.org/silviucpp/graylog_lager.svg?branch=master)](https://travis-ci.org/silviucpp/graylog_lager)
![GitHub](https://img.shields.io/github/license/silviucpp/graylog_lager)
![Hex.pm](https://img.shields.io/hexpm/v/graylog_lager)
![Maintenance](https://img.shields.io/maintenance/yes/2019)

[Lager][1] is a standard logging tool for Erlang, this project crates lager formatter to output messages in [GELF format][2] and lager backend to send messages via UDP to a graylog server.

Features
-----------

- Support for compression gzip or zlib
- Support for chunk encoded GELF messages
- Works with lager 3.x

Quick start
-----------

Include this backend into your project using rebar:

```erl
{graylog_lager, ".*", {git, "https://github.com/silviucpp/graylog_lager.git", "master"}}
```

Then you need to add a new handler in lager configuration, usually in your `app.config` file, for example:

```erl
{lager, [
    {handlers, [
        {graylog_lager_udp_backend, [
            {host, "127.0.0.1"},
            {port, 12201},
            {level, info},
            {format_config, [
                {compression, disabled},
                {extra_fields, [
                    {<<"_environment">>, <<"production">>}
                ]}
            ]}
        ]}
    ]}
]}
```

Configuration
-----------

Backend configuration parameters:

- `host`: graylog server host, example: `{host, "127.0.0.1"}`
- `port`: graylog server port, example: `{port, 12201}`
- `level`: minimum logging level - messages below that level will be dropped. One of the above values (debug, info, notice, warning, error, critical, alert)
- `format_config`: backend-specific configuration - a proplist with:
    - `compression`: one of `disabled`, `gzip`, `zlib` (atom, defaults to `disabled`)
    - `extra_fields`: optional proplist of `{name, value}`, they will be sent as additional fields to graylog. The name must be a binary starting with an underscore, while the value must be any term.
    - `hostname`: optional binary with the hostname. If not provided will be automatically detected.
- `formatter`: In case you want to change the GELF formatter module. Default : `{formatter, graylog_lager_gelf_formatter}`
- `chunk_size`: The max size of each UPD packet: default 8154. Valid value between interval 1420 - 8154. Example: `{chunk_size, 8154}`
- `inet_family`: Specify the inet family. Default to `inet`. Supported values `inet` or `inet6`       

[1]:https://github.com/erlang-lager/lager
[2]:http://docs.graylog.org/en/3.1/pages/gelf.html
