# Triage Labels

The skills speak in terms of five canonical triage roles. This repo uses the default strings (no remote label system); each one is written as the `Status:` value in an issue file under `.scratch/`.

| Role in mattpocock/skills | String used here  | Meaning                                  |
| ------------------------- | ----------------- | ---------------------------------------- |
| `needs-triage`            | `needs-triage`    | Maintainer needs to evaluate this issue  |
| `needs-info`              | `needs-info`      | Waiting on reporter for more information  |
| `ready-for-agent`         | `ready-for-agent` | Fully specified, ready for an AFK agent   |
| `ready-for-human`         | `ready-for-human` | Requires human implementation             |
| `wontfix`                 | `wontfix`         | Will not be actioned                      |

When a skill mentions a role (e.g. "apply the AFK-ready triage label"), set the issue file's `Status:` line to the corresponding string from this table.

Edit the right-hand column if you adopt a different vocabulary.
