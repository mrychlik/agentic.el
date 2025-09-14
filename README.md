![CI](https://github.com/mrychlik/agentic.el/actions/workflows/elisp-ci.yml/badge.svg)
# agentic.el

**Agentic workflows for Emacs** — use GPT (via [gptel]) to rewrite buffers, generate **unified diffs** for your repo, apply them with Git, and open PRs with [Forge]. Great for teaching AI/Data Science: from **prompt → patch → PR** — with you in control via Magit.

> Requires Emacs 27+ and configured [gptel] (API key + model). Recommended: Magit, Forge, yasnippet.

## Features (keybindings active when `agentic-mode` is on)

- `C-c g r` — **Rewrite** region/buffer with instructions (returns raw code).
- `C-c g p` — **Patch (apply)**: ask for a *single unified diff* for the project and apply it (`git apply`).
- `C-c g P` — **Patch (preview)**: show the diff, don’t apply.
- `C-c g b` — **Safe branch**, `C-c g c` **Commit**, `C-c g u` **Push**, `C-c g R` **Open PR** (Forge).
- `C-c g C` — **Compose PATCH** prompt; `C-c g W` — **Compose REWRITE** prompt.

## Install

### A) `straight.el`
```elisp
(use-package agentic
  :straight (agentic :type git :host github :repo "mrychlik/agentic.el" :branch "main")
  :config
  (global-agentic-mode 1))
