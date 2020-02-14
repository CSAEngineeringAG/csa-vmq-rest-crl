# vmq_rest_crl

Plugin to continuously poll a CRL from an HTTP API

## Build

You must have a recent version of Erlang installed (it's recommended to use the
same one VerneMQ is compiled for, typically > 17). To compile do:

    $ rebar3 compile


Then enable the plugin using:

    $ vmq-admin plugin enable --name vmq_rest_crl --path /path/to/plugin

Or if you start the plugin via `vernemq.conf`:

    plugins.vmq_rest_crl = on
    plugins.vmq_rest_crl.path = /path/to/plugin

## Supported Endpoints
Currently only the [CFSSL](https://github.com/cloudflare/cfssl) CRL endpoint is supported:
    
    THE CRL ENDPOINT

    Endpoint: /api/v1/cfssl/crl
    Method:   GET

    Optional URL Query parameters:

        * expiry: a value, in seconds, after which the CRL should expire
        from the moment of the request.

    Example:

        $ curl ${CFSSL_HOST}/api/v1/cfssl/crl
        $ curl ${CFSSL_HOST}/api/v1/cfssl/crl?expiry=7200h
    
    Result:
        {
            "success": true,
            "result": "MIIB9zCB4AIBATANBgkqhkiG...",
            "errors": [],
            "messages": []
        }

Taken from https://github.com/cloudflare/cfssl/blob/master/doc/api/endpoint_crl.txt

## Sample Configuration

The following configuration entries can either be set via `vernemq.conf` or via the Docker environment variables. 

To enable a CRL polling agent use:

    vmq_rest_crl.default=on

Set the URL of the GET-Endpoint:

    vmq_rest_crl.default.url=http://hostname:8888/api/v1/cfssl/crl

Set the priority of the agent:

    vmq_rest_crl.default.priority=1

Set the refresh interval in **milliseconds** of the agent (default: 60'000 = 1 minute):

    vmq_rest_crl.default.refresh_interval=10000

Set the CRL file which should be updated after a successfull API call. This file should correspond to the CRL file you define in the SSL listener config of [VerneMQ](https://github.com/vernemq/vernemq/blob/master/apps/vmq_server/priv/vmq_server.schema#L1227) and you will need **write** permissions:

    vmq_rest_crl.default.crlfile=/etc/ssl/revoke.crl


### Additional Agents

An additional agent can be added by using a different **name**. In the example above the name equals to `default` and can be used for further configuring this particular agent.

### Docker 

If you are using Docker, you must prefix all entries with `DOCKER_VERNEMQ_` e.g.:

    DOCKER_VERNEMQ_vmq_rest_crl.default=on