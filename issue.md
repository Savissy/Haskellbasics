mysql:
  image: mysql:8.4
  restart: unless-stopped
  command: --default-authentication-plugin=mysql_native_password
  environment:
    MYSQL_ROOT_PASSWORD: rootpassword
    MYSQL_DATABASE: cardano_ide
    MYSQL_USER: cardano_ide_user
    MYSQL_PASSWORD: cardano_ide_pass
  ports:
    - "3306:3306"
  volumes:
    - mysql-data:/var/lib/mysql
    - ./apps/haskell-api/db/init.sql:/docker-entrypoint-initdb.d/init.sql


INSERT IGNORE INTO users (email, display_name, auth_provider, provider_user_id)
VALUES ('local@cardano-ide.dev', 'Local User', 'local', 'local-user');

dell_8th_gen_core_i5@DESKTOP-R4MNH88:~/haskel-IDE/IDE$ docker compose ps
NAME               IMAGE               COMMAND                  SERVICE      CREATED          STATUS                          PORTS
ide-frontend-1     node:20-alpine      "docker-entrypoint.s…"   frontend     38 seconds ago   Up 35 seconds                   0.0.0.0:5173->5173/tcp, [::]:5173->5173/tcp
ide-mysql-1        mysql:8.4           "docker-entrypoint.s…"   mysql        6 minutes ago    Restarting (1) 15 seconds ago   
ide-phpmyadmin-1   phpmyadmin:latest   "/docker-entrypoint.…"   phpmyadmin   38 seconds ago   Up 36 seconds                   0.0.0.0:8081->80/tcp, [::]:8081->80/tcp
dell_8th_gen_core_i5@DESKTOP-R4MNH88:~/haskel-IDE/IDE$ docker compose exec haskell-api sh -lc 'env | grep MYSQL; getent hosts mysql || true'
service "haskell-api" is not running
dell_8th_gen_core_i5@DESKTOP-R4MNH88:~/haskel-IDE/IDE$ 


  sqlite-web:
    image: coleifer/sqlite-web
    restart: unless-stopped
    command: sqlite_web -H 0.0.0.0 -p 8082 /data/db/cardano_ide.sqlite
    volumes:
      - ./data/db:/data/db
    ports:
      - "8082:8082"
    depends_on:
      - haskell-api
