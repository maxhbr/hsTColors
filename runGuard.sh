#!/usr/bin/env bash
if [[ -z "$TMUX" ]]; then
  if tmux has-session -t "hsTColors-Guard" 2>/dev/null; then
    tmux att "hsTColors-Guard"
  else
    tmux new-session -s "hsTColors-Guard" "nix-shell -I ~ --command 'guard start'"
  fi
else
  nix-shell -I ~ --command 'guard start'
fi
