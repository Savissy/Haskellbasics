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

CREATE DATABASE IF NOT EXISTS cardano_ide;
USE cardano_ide;

DROP USER IF EXISTS 'cardano_ide_user'@'%';
CREATE USER 'cardano_ide_user'@'%' IDENTIFIED WITH mysql_native_password BY 'cardano_ide_pass';
GRANT ALL PRIVILEGES ON cardano_ide.* TO 'cardano_ide_user'@'%';
FLUSH PRIVILEGES;

CREATE TABLE IF NOT EXISTS users (
  id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  email VARCHAR(255) NOT NULL UNIQUE,
  display_name VARCHAR(255) NULL,
  auth_provider VARCHAR(64) NOT NULL DEFAULT 'local',
  provider_user_id VARCHAR(255) NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS workspaces (
  name VARCHAR(255) NOT NULL PRIMARY KEY,
  user_id BIGINT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  file_writes BIGINT NOT NULL DEFAULT 0,
  build_count BIGINT NOT NULL DEFAULT 0,
  last_build_status VARCHAR(32) NULL,
  last_build_at TIMESTAMP NULL,
  INDEX idx_workspaces_user_id (user_id),
  CONSTRAINT fk_workspaces_user FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS builds (
  job_id VARCHAR(255) NOT NULL PRIMARY KEY,
  user_id BIGINT NOT NULL,
  workspace_name VARCHAR(255) NOT NULL,
  selected_path TEXT NOT NULL,
  started_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  finished_at TIMESTAMP NULL,
  ok BOOLEAN NULL,
  INDEX idx_builds_workspace_name (workspace_name),
  INDEX idx_builds_user_id (user_id),
  CONSTRAINT fk_builds_user FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS build_logs (
  id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  job_id VARCHAR(255) NOT NULL,
  line_text TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX idx_build_logs_job_id (job_id)
);

CREATE TABLE IF NOT EXISTS file_events (
  id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  user_id BIGINT NOT NULL,
  workspace_name VARCHAR(255) NOT NULL,
  file_path TEXT NOT NULL,
  event_type VARCHAR(64) NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  INDEX idx_file_events_workspace_name (workspace_name),
  INDEX idx_file_events_user_id (user_id),
  CONSTRAINT fk_file_events_user FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

INSERT IGNORE INTO users (email, display_name, auth_provider, provider_user_id)
VALUES ('local@cardano-ide.dev', 'Local User', 'local', 'local-user');
  
