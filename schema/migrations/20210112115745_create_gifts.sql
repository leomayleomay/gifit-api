create table if not exists gifts (
  id bigserial primary key,
  sender bigserial not null references users (id) on delete restrict,
  receiver bigserial not null references users (id) on delete restrict,
  amount integer not null,
  state text not null,
  notes text,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);