Imitate Word Press
-----------------
-- AI prompt 
-- it must use cardano wallet(Coxy Wallet) on preprod testnet
1. languages - Haskell/aiken/opshin/helios...
2. link to github
(1) Examples - a) Lock/Unlock b) ...
    i)      - onchain
        - unit tests
    ii)     - offchain
        - unit tests
    iii)     - front end
        - unit tests
    iv)    - backend
        - database
        - server side 
        - tests
    v)     - different very nice templates for frontend
(2) Editor
(3) Wizard style building
3. IDE - Compile - Test - Deploy





Plutus Playground Studio

    - Frontend
    - Server Side API
    - Database 
    - file system
    
1. languages - Haskell/aiken/opshin/helios.../midnight

2. Examples - a) Lock/Unlock b) market place c) etc
    
3. For Each example this is displayed
    i)      - onchain
        - unit tests
    ii)     - offchain
        - unit tests
    iii)     - front end
        - unit tests
    iv)    - backend
        - database
        - server side 
        - tests
    v)     - different very nice templates for frontend
    vi)     - also link to hosted application
4.     a)   Wizard style building script
        i)   Compile
        ii)  Test
        iii) Deploy 
    b) Editor
        i)     code
        ii)    debug
        i)     Compile
        ii)    Test
        iii)   Deploy 


Imitate Word Press Easy of Use
------------------------------

    -- AI integrated
    -- Coxy Wallet integrated

services:
  mysql:
    image: mysql:8.4
    restart: unless-stopped
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
    healthcheck:
      test: ["CMD", "mysqladmin", "ping", "-h", "localhost", "-prootpassword"]
      interval: 10s
      timeout: 5s
      retries: 20
      start_period: 30s

  phpmyadmin:
    image: phpmyadmin:latest
    restart: unless-stopped
    environment:
      PMA_HOST: mysql
      PMA_PORT: 3306
      PMA_USER: root
      PMA_PASSWORD: rootpassword
    ports:
      - "8081:80"
    depends_on:
      mysql:
        condition: service_healthy

  haskell-api:
    build: ./apps/haskell-api
    environment:
      - WORKSPACE_ROOT=/data/workspaces
      - ALLOWED_ORIGINS=http://127.0.0.1:5173,http://localhost:5173,http://127.0.0.1:5174,http://localhost:5174,http://127.0.0.1:5175,http://localhost:5175
      - MYSQL_HOST=mysql
      - MYSQL_PORT=3306
      - MYSQL_DATABASE=cardano_ide
      - MYSQL_USER=cardano_ide_user
      - MYSQL_PASSWORD=cardano_ide_pass
    volumes:
      - ./data/workspaces:/data/workspaces
    ports: ["8080:8080"]
    depends_on:
      mysql:
        condition: service_healthy

  compiler-worker:
    build: ./apps/compiler-worker
    environment:
      - WORKSPACE_ROOT=/data/workspaces
      - DEFAULT_PROJECT=demo
      - COMPILE_CMD=bash -lc "cd /data/workspaces/demo && cabal build 2>&1"
    volumes:
      - ./data/workspaces:/data/workspaces

  frontend:
    image: node:20-alpine
    working_dir: /app
    volumes:
      - ./:/app
    ports: ["5173:5173"]
    environment:
      - NODE_OPTIONS=--dns-result-order=ipv4first
    command: sh -lc "npm i -g pnpm@9 && pnpm i && pnpm -C apps/shell-frontend dev --host 0.0.0.0"
    depends_on:
      haskell-api:
        condition: service_started

volumes:
  mysql-data:
