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


  
