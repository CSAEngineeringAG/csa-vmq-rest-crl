FROM vernemq/vernemq
COPY . /etc/plugins/crl
COPY priv/vmq_rest_crl.schema /vernemq/share/schema/40-vmq_rest_crl.schema
COPY --chown=vernemq priv/crl.pem /etc/ssl/crl.pem 
ENV DOCKER_VERNEMQ_ACCEPT_EULA=yes
ENV DOCKER_VERNEMQ_vmq_rest_crl.default=on
ENV DOCKER_VERNEMQ_vmq_rest_crl.default.priority=1
ENV DOCKER_VERNEMQ_vmq_rest_crl.default.url=http://host.docker.internal:8888/api/v1/cfssl/crl
ENV DOCKER_VERNEMQ_vmq_rest_crl.default.refresh_interval=10000
ENV DOCKER_VERNEMQ_vmq_rest_crl.default.crlfile=/etc/ssl/crl.pem
CMD ["start_vernemq"]