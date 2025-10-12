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
