# agentic.el

![CI](https://github.com/mrychlik/agentic.el/actions/workflows/elisp-ci.yml/badge.svg)

**Agentic workflows for Emacs** offer a streamlined interface to enhance your coding experience by leveraging GPT (via [gptel]). This package allows you to rewrite buffers, generate **unified diffs** for your repository, and apply these diffs with Git, all while facilitating the opening of pull requests (PRs) with [Forge]. Ideal for those teaching AI or Data Science, agentic.el efficiently follows the workflow from **prompt → patch → PR** with you maintaining full control via Magit.

### Requirements
- Emacs 27 or later
- Configured [gptel] with an API key and selected model
- Recommended packages: Magit, Forge, and yasnippet for enhanced functionality

### Features
Agentic.el activates specific keybindings when `agentic-mode` is enabled, allowing for easy access to powerful features:

- `C-c g r` — **Rewrite**: Rewrite the selected region or entire buffer based on provided instructions. Returns raw code.
- `C-c g p` — **Patch (apply)**: Request a single unified diff for the current project and apply it using `git apply`.
- `C-c g P` — **Patch (preview)**: View the unified diff without applying it to the codebase; useful for review before making changes.
- `C-c g b` — **Safe branch**: Create a new branch for your changes.
- `C-c g c` — **Commit**: Commit your changes to the repository.
- `C-c g u` — **Push**: Push your committed changes to the remote repository.
- `C-c g R` — **Open PR**: Initiate a pull request using Forge for collaborative reviews.
- `C-c g C` — **Compose PATCH**: Open a prompt to compose a patch.
- `C-c g W` — **Compose REWRITE**: Open a prompt to compose a rewrite instruction.
- `C-c g v` — **Project review**: Read and review all files within the current project directory.
- `C-c g l` — **Open log file**: Show the ChatGPT interaction log.

### Installation

#### A) Using `straight.el`
To install the package using `straight.el`, add the following configuration to your Emacs initialization file:

```elisp
(use-package agentic
  :straight (agentic :type git :host github :repo "mrychlik/agentic.el" :branch "main")
  :config
  (global-agentic-mode 1))
```

Incorporate agentic.el into your Emacs setup to simplify your development process with intelligent tools powered by GPT. Enjoy a seamless coding experience with version control integration and collaborative features at your fingertips!

## Troubleshooting

**No output / nothing inserted?**  
Use `C-u C-c RET` in a gptel buffer to open the routing menu and ensure
output isn’t redirected elsewhere. `agentic.el` calls the request API
directly; if the model returns no text it will warn instead of erroring.

**“Contacting model…” but nothing happens.**  
Check that `gptel` is installed and your API key is set:
```elisp
(use-package gptel :straight t)
(setq gptel-api-key (getenv "OPENAI_API_KEY"))


## Binary/huge files during review
The review command skips files with NUL bytes and enforces byte budgets. Increase:


```elisp
(setq agentic/review-max-files 60
      agentic/review-max-bytes-per-file 40000
      agentic/review-total-byte-budget 500000)
