APP:=fortran-amedas
DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
MKFILE:=-C .. -f makefile

release:
	make release $(MKFILE)

install:
	make install $(MKFILE)

uninstall:
	make uninstall $(MKFILE)

service: install
	cp ${DIR}${APP}.service /etc/systemd/system/ && \
	cp ${DIR}${APP}.timer /etc/systemd/system/ && \
	systemctl daemon-reload && \
	systemctl enable ${APP}.service ${APP}.timer && \
	systemctl start ${APP}.service ${APP}.timer &&  \
	systemctl status ${APP}.service ${APP}.timer

stop:
	systemctl stop ${APP}.service ${APP}.timer ;\
	systemctl disable ${APP}.service ${APP}.timer ; \
	systemctl status ${APP}.service ${APP}.timer
	rm /etc/systemd/system/${APP}.service ; \
	rm /etc/systemd/system/${APP}.timer

watchdog:
	journalctl -f -u ${APP}.service
