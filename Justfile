_default:
	@just --list --unsorted --list-heading '' --list-prefix '—— '

run *ARGS:
	cargo run -F debug -- {{ARGS}}
