create table if not exists users (
  id bigserial primary key,
  email text not null unique,
  handle text not null unique,
  password text not null,
  reset_password_token text unique,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);