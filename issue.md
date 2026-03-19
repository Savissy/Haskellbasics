dell_8th_gen_core_i5@DESKTOP-R4MNH88:~/haskel-IDE/IDE$ docker compose logs haskell-api --tail=200
haskell-api-1  | Warning: Both /root/.cabal and /root/.config/cabal/config exist - ignoring the
haskell-api-1  | former.
haskell-api-1  | It is advisable to remove one of them. In that case, we will use the remaining
haskell-api-1  | one by default (unless '$CABAL_DIR' is explicitly set).
haskell-api-1  | cardano-ide-api: ConnectionError {errFunction = "connect", errNumber = 2061, errMessage = "RSA Encryption not supported - caching_sha2_password plugin was built with GnuTLS support"}
haskell-api-1  | Warning: Both /root/.cabal and /root/.config/cabal/config exist - ignoring the
haskell-api-1  | former.
haskell-api-1  | It is advisable to remove one of them. In that case, we will use the remaining
haskell-api-1  | one by default (unless '$CABAL_DIR' is explicitly set).
haskell-api-1  | cardano-ide-api: ConnectionError {errFunction = "connect", errNumber = 2061, errMessage = "RSA Encryption not supported - caching_sha2_password plugin was built with GnuTLS support"}
haskell-api-1  | Warning: Both /root/.cabal and /root/.config/cabal/config exist - ignoring the
haskell-api-1  | former.
haskell-api-1  | It is advisable to remove one of them. In that case, we will use the remaining
haskell-api-1  | one by default (unless '$CABAL_DIR' is explicitly set).
haskell-api-1  | cardano-ide-api: ConnectionError {errFunction = "connect", errNumber = 2061, errMessage = "RSA Encryption not supported - caching_sha2_password plugin was built with GnuTLS support"}
haskell-api-1  | Warning: Both /root/.cabal and /root/.config/cabal/config exist - ignoring the
haskell-api-1  | former.
haskell-api-1  | It is advisable to remove one of them. In that case, we will use the remaining
haskell-api-1  | one by default (unless '$CABAL_DIR' is explicitly set).
haskell-api-1  | cardano-ide-api: ConnectionError {errFunction = "connect", errNumber = 2061, errMessage = "RSA Encryption not supported - caching_sha2_password plugin was built with GnuTLS support"}
dell_8th_gen_core_i5@DESKTOP-R4MNH88:~/haskel-IDE/IDE$ docker compose logs mysql --tail=200
mysql-1  | 2026-03-18 23:00:12+00:00 [Note] [Entrypoint]: Entrypoint script for MySQL Server 8.4.8-1.el9 started.
mysql-1  | 2026-03-18 23:00:14+00:00 [Note] [Entrypoint]: Switching to dedicated user 'mysql'
mysql-1  | 2026-03-18 23:00:14+00:00 [Note] [Entrypoint]: Entrypoint script for MySQL Server 8.4.8-1.el9 started.
mysql-1  | 2026-03-18 23:00:14+00:00 [Note] [Entrypoint]: Initializing database files
mysql-1  | 2026-03-18T23:00:14.976893Z 0 [System] [MY-015017] [Server] MySQL Server Initialization - start.
mysql-1  | 2026-03-18T23:00:14.980996Z 0 [System] [MY-013169] [Server] /usr/sbin/mysqld (mysqld 8.4.8) initializing of server in progress as process 80
mysql-1  | 2026-03-18T23:00:15.008853Z 1 [System] [MY-013576] [InnoDB] InnoDB initialization has started.
mysql-1  | 2026-03-18T23:00:16.079576Z 1 [System] [MY-013577] [InnoDB] InnoDB initialization has ended.
mysql-1  | 2026-03-18T23:00:20.788246Z 6 [Warning] [MY-010453] [Server] root@localhost is created with an empty password ! Please consider switching off the --initialize-insecure option.
mysql-1  | 2026-03-18T23:00:26.686583Z 0 [System] [MY-015018] [Server] MySQL Server Initialization - end.
mysql-1  | 2026-03-18 23:00:26+00:00 [Note] [Entrypoint]: Database files initialized
mysql-1  | 2026-03-18 23:00:26+00:00 [Note] [Entrypoint]: Starting temporary server
mysql-1  | 2026-03-18T23:00:26.802106Z 0 [System] [MY-015015] [Server] MySQL Server - start.
mysql-1  | 2026-03-18T23:00:27.223869Z 0 [System] [MY-010116] [Server] /usr/sbin/mysqld (mysqld 8.4.8) starting as process 121
mysql-1  | 2026-03-18T23:00:27.269509Z 1 [System] [MY-013576] [InnoDB] InnoDB initialization has started.
mysql-1  | 2026-03-18T23:00:28.413422Z 1 [System] [MY-013577] [InnoDB] InnoDB initialization has ended.
mysql-1  | 2026-03-18T23:00:29.243225Z 0 [Warning] [MY-010068] [Server] CA certificate ca.pem is self signed.
mysql-1  | 2026-03-18T23:00:29.243367Z 0 [System] [MY-013602] [Server] Channel mysql_main configured to support TLS. Encrypted connections are now supported for this channel.
mysql-1  | 2026-03-18T23:00:29.254731Z 0 [Warning] [MY-011810] [Server] Insecure configuration for --pid-file: Location '/var/run/mysqld' in the path is accessible to all OS users. Consider choosing a different directory.
mysql-1  | 2026-03-18T23:00:29.324887Z 0 [System] [MY-011323] [Server] X Plugin ready for connections. Socket: /var/run/mysqld/mysqlx.sock
mysql-1  | 2026-03-18T23:00:29.325344Z 0 [System] [MY-010931] [Server] /usr/sbin/mysqld: ready for connections. Version: '8.4.8'  socket: '/var/run/mysqld/mysqld.sock'  port: 0  MySQL Community Server - GPL.
mysql-1  | 2026-03-18 23:00:29+00:00 [Note] [Entrypoint]: Temporary server started.
mysql-1  | '/var/lib/mysql/mysql.sock' -> '/var/run/mysqld/mysqld.sock'
mysql-1  | Warning: Unable to load '/usr/share/zoneinfo/iso3166.tab' as time zone. Skipping it.
mysql-1  | Warning: Unable to load '/usr/share/zoneinfo/leap-seconds.list' as time zone. Skipping it.
mysql-1  | Warning: Unable to load '/usr/share/zoneinfo/leapseconds' as time zone. Skipping it.
mysql-1  | Warning: Unable to load '/usr/share/zoneinfo/tzdata.zi' as time zone. Skipping it.
mysql-1  | Warning: Unable to load '/usr/share/zoneinfo/zone.tab' as time zone. Skipping it.
mysql-1  | Warning: Unable to load '/usr/share/zoneinfo/zone1970.tab' as time zone. Skipping it.
mysql-1  | 2026-03-18 23:00:36+00:00 [Note] [Entrypoint]: Creating database cardano_ide
mysql-1  | 2026-03-18 23:00:36+00:00 [Note] [Entrypoint]: Creating user cardano_ide_user
mysql-1  | 2026-03-18 23:00:36+00:00 [Note] [Entrypoint]: Giving user cardano_ide_user access to schema cardano_ide
mysql-1  | 
mysql-1  | 2026-03-18 23:00:36+00:00 [Note] [Entrypoint]: /usr/local/bin/docker-entrypoint.sh: running /docker-entrypoint-initdb.d/init.sql
mysql-1  | 
mysql-1  | 
mysql-1  | 2026-03-18 23:00:37+00:00 [Note] [Entrypoint]: Stopping temporary server
mysql-1  | 2026-03-18T23:00:37.649159Z 14 [System] [MY-013172] [Server] Received SHUTDOWN from user root. Shutting down mysqld (Version: 8.4.8).
mysql-1  | 2026-03-18T23:00:38.560387Z 0 [System] [MY-010910] [Server] /usr/sbin/mysqld: Shutdown complete (mysqld 8.4.8)  MySQL Community Server - GPL.
mysql-1  | 2026-03-18T23:00:38.560455Z 0 [System] [MY-015016] [Server] MySQL Server - end.
mysql-1  | 2026-03-18 23:00:38+00:00 [Note] [Entrypoint]: Temporary server stopped
mysql-1  | 
mysql-1  | 2026-03-18 23:00:38+00:00 [Note] [Entrypoint]: MySQL init process done. Ready for start up.
mysql-1  | 
mysql-1  | 2026-03-18T23:00:38.709692Z 0 [System] [MY-015015] [Server] MySQL Server - start.
mysql-1  | 2026-03-18T23:00:39.094888Z 0 [System] [MY-010116] [Server] /usr/sbin/mysqld (mysqld 8.4.8) starting as process 1
mysql-1  | 2026-03-18T23:00:39.121313Z 1 [System] [MY-013576] [InnoDB] InnoDB initialization has started.
mysql-1  | 2026-03-18T23:00:40.170614Z 1 [System] [MY-013577] [InnoDB] InnoDB initialization has ended.
mysql-1  | 2026-03-18T23:00:40.924587Z 0 [Warning] [MY-010068] [Server] CA certificate ca.pem is self signed.
mysql-1  | 2026-03-18T23:00:40.924694Z 0 [System] [MY-013602] [Server] Channel mysql_main configured to support TLS. Encrypted connections are now supported for this channel.
mysql-1  | 2026-03-18T23:00:40.934450Z 0 [Warning] [MY-011810] [Server] Insecure configuration for --pid-file: Location '/var/run/mysqld' in the path is accessible to all OS users. Consider choosing a different directory.
mysql-1  | 2026-03-18T23:00:41.010302Z 0 [System] [MY-011323] [Server] X Plugin ready for connections. Bind-address: '::' port: 33060, socket: /var/run/mysqld/mysqlx.sock
mysql-1  | 2026-03-18T23:00:41.010955Z 0 [System] [MY-010931] [Server] /usr/sbin/mysqld: ready for connections. Version: '8.4.8'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server - GPL.
mysql-1  | 2026-03-19 07:05:29+00:00 [Note] [Entrypoint]: Entrypoint script for MySQL Server 8.4.8-1.el9 started.
mysql-1  | 2026-03-19 07:05:31+00:00 [Note] [Entrypoint]: Switching to dedicated user 'mysql'
mysql-1  | 2026-03-19 07:05:31+00:00 [Note] [Entrypoint]: Entrypoint script for MySQL Server 8.4.8-1.el9 started.
mysql-1  | '/var/lib/mysql/mysql.sock' -> '/var/run/mysqld/mysqld.sock'
mysql-1  | 2026-03-19T07:05:32.453460Z 0 [System] [MY-015015] [Server] MySQL Server - start.
mysql-1  | 2026-03-19T07:05:32.951572Z 0 [System] [MY-010116] [Server] /usr/sbin/mysqld (mysqld 8.4.8) starting as process 1
mysql-1  | 2026-03-19T07:05:32.984753Z 1 [System] [MY-013576] [InnoDB] InnoDB initialization has started.
mysql-1  | 2026-03-19T07:05:34.352504Z 1 [System] [MY-013577] [InnoDB] InnoDB initialization has ended.
mysql-1  | 2026-03-19T07:05:34.716278Z 0 [System] [MY-010229] [Server] Starting XA crash recovery...
mysql-1  | 2026-03-19T07:05:34.740430Z 0 [System] [MY-010232] [Server] XA crash recovery finished.
mysql-1  | 2026-03-19T07:05:34.885314Z 0 [Warning] [MY-010068] [Server] CA certificate ca.pem is self signed.
mysql-1  | 2026-03-19T07:05:34.885400Z 0 [System] [MY-013602] [Server] Channel mysql_main configured to support TLS. Encrypted connections are now supported for this channel.
mysql-1  | 2026-03-19T07:05:34.896137Z 0 [Warning] [MY-011810] [Server] Insecure configuration for --pid-file: Location '/var/run/mysqld' in the path is accessible to all OS users. Consider choosing a different directory.
mysql-1  | 2026-03-19T07:05:34.959288Z 0 [System] [MY-011323] [Server] X Plugin ready for connections. Bind-address: '::' port: 33060, socket: /var/run/mysqld/mysqlx.sock
mysql-1  | 2026-03-19T07:05:34.959573Z 0 [System] [MY-010931] [Server] /usr/sbin/mysqld: ready for connections. Version: '8.4.8'  socket: '/var/run/mysqld/mysqld.sock'  port: 3306  MySQL Community Server - GPL.
dell_8th_gen_core_i5@DESKTOP-R4MNH88:~/haskel-IDE/IDE$ 
